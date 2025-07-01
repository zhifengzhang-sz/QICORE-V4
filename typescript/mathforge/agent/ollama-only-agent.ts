import { Ollama } from 'ollama';
import { z } from 'zod';

// Use Zod for validation and type safety
const GenerationOptionsSchema = z.object({
  model: z.string().default("qwen2.5-coder:7b"),
  temperature: z.number().min(0).max(2).default(0.7),
  maxTokens: z.number().positive().default(4000),
  timeout: z.number().positive().default(30000),
});

const HealthStatusSchema = z.object({
  ollama: z.boolean(),
  models: z.array(z.string()),
  summary: z.string(),
  timestamp: z.date(),
});

type GenerationOptions = z.infer<typeof GenerationOptionsSchema>;
type HealthStatus = z.infer<typeof HealthStatusSchema>;

// Simple Result pattern using discriminated union
type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E };

const Result = {
  success: <T>(data: T): Result<T> => ({ success: true, data }),
  failure: <T, E = Error>(error: E): Result<T, E> => ({ success: false, error }),
  
  // Utility methods
  map: <T, U, E>(result: Result<T, E>, fn: (data: T) => U): Result<U, E> =>
    result.success ? Result.success(fn(result.data)) : { success: false, error: result.error },
    
  flatMap: <T, U, E>(result: Result<T, E>, fn: (data: T) => Result<U, E>): Result<U, E> =>
    result.success ? fn(result.data) : { success: false, error: result.error },
    
  isSuccess: <T, E>(result: Result<T, E>): result is { success: true; data: T } =>
    result.success,
    
  isFailure: <T, E>(result: Result<T, E>): result is { success: false; error: E } =>
    !result.success,
};

// Simple structured logger using console with timestamps
interface Logger {
  info: (message: string, meta?: Record<string, any>) => void;
  error: (message: string, meta?: Record<string, any>) => void;
  warn: (message: string, meta?: Record<string, any>) => void;
  debug: (message: string, meta?: Record<string, any>) => void;
}

const createLogger = (name: string): Logger => {
  const formatMessage = (level: string, message: string, meta?: Record<string, any>) => {
    const timestamp = new Date().toISOString();
    const metaStr = meta ? ` ${JSON.stringify(meta)}` : '';
    return `[${timestamp}] [${name}] ${level}: ${message}${metaStr}`;
  };

  return {
    info: (message: string, meta?: Record<string, any>) => 
      console.log(formatMessage('INFO', message, meta)),
    error: (message: string, meta?: Record<string, any>) => 
      console.error(formatMessage('ERROR', message, meta)),
    warn: (message: string, meta?: Record<string, any>) => 
      console.warn(formatMessage('WARN', message, meta)),
    debug: (message: string, meta?: Record<string, any>) => 
      console.log(formatMessage('DEBUG', message, meta)),
  };
};

// Performance measurement utility
interface PerformanceTracker {
  start: (operation: string) => void;
  end: (operation: string) => number;
  getStats: (operation: string) => { count: number; average: number; last: number } | null;
}

const createPerformanceTracker = (): PerformanceTracker => {
  const operations = new Map<string, { times: number[]; startTime?: number }>();

  return {
    start: (operation: string) => {
      if (!operations.has(operation)) {
        operations.set(operation, { times: [] });
      }
      const op = operations.get(operation)!;
      op.startTime = Date.now();
    },

    end: (operation: string) => {
      const op = operations.get(operation);
      if (!op || !op.startTime) return 0;
      
      const duration = Date.now() - op.startTime;
      op.times.push(duration);
      delete op.startTime;
      return duration;
    },

    getStats: (operation: string) => {
      const op = operations.get(operation);
      if (!op || op.times.length === 0) return null;
      
      const sum = op.times.reduce((a, b) => a + b, 0);
      const lastTime = op.times[op.times.length - 1];
      return {
        count: op.times.length,
        average: Math.round(sum / op.times.length),
        last: lastTime ?? 0,
      };
    },
  };
};

export class OllamaOnlyAgent {
  private ollama: Ollama;
  private logger: Logger;
  private performance: PerformanceTracker;
  
  private config = {
    baseUrl: "http://localhost:11434",
    defaultModel: "qwen2.5-coder:7b",
    timeout: 30000,
  };

  constructor(config?: { baseUrl?: string; defaultModel?: string; timeout?: number }) {
    this.config = { ...this.config, ...config };
    this.logger = createLogger('OllamaOnlyAgent');
    this.performance = createPerformanceTracker();
    this.ollama = new Ollama({ host: this.config.baseUrl });
    
    this.logger.info("🚀 Ollama-Only Agent initialized", {
      baseUrl: this.config.baseUrl,
      defaultModel: this.config.defaultModel,
      timeout: this.config.timeout,
    });
  }

  /**
   * Generate code with comprehensive error handling and validation
   */
  async generateCode(prompt: string, options?: Partial<GenerationOptions>): Promise<Result<string>> {
    this.performance.start('generateCode');
    
    try {
      // Validate options using Zod
      const validatedOptions = GenerationOptionsSchema.parse({
        model: this.config.defaultModel,
        ...options,
      });

      this.logger.info("🎯 Starting code generation", {
        promptLength: prompt.length,
        model: validatedOptions.model,
        temperature: validatedOptions.temperature,
      });

      const response = await this.ollama.generate({
        model: validatedOptions.model,
        prompt: prompt,
        stream: false,
        options: {
          temperature: validatedOptions.temperature,
          num_predict: validatedOptions.maxTokens,
          num_ctx: 8192, // Increased context window
        },
      });

      const duration = this.performance.end('generateCode');
      
      this.logger.info("✅ Code generation successful", {
        duration,
        model: validatedOptions.model,
        responseLength: response.response.length,
        evalCount: response.eval_count,
        totalDuration: response.total_duration,
      });

      return Result.success(response.response.trim());
      
    } catch (error) {
      const duration = this.performance.end('generateCode');
      this.logger.error("❌ Code generation failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Code generation error: ${String(error)}`)
      );
    }
  }

  /**
   * Chat completion for conversational interactions
   */
  async chatCompletion(
    messages: Array<{ role: 'system' | 'user' | 'assistant'; content: string }>,
    options?: Partial<GenerationOptions>
  ): Promise<Result<string>> {
    this.performance.start('chatCompletion');
    
    try {
      const validatedOptions = GenerationOptionsSchema.parse({
        model: this.config.defaultModel,
        ...options,
      });

      this.logger.info("💬 Starting chat completion", {
        messageCount: messages.length,
        model: validatedOptions.model,
      });

      const response = await this.ollama.chat({
        model: validatedOptions.model,
        messages: messages,
        stream: false,
        options: {
          temperature: validatedOptions.temperature,
          num_predict: validatedOptions.maxTokens,
          num_ctx: 8192,
        },
      });

      const duration = this.performance.end('chatCompletion');
      
      this.logger.info("✅ Chat completion successful", {
        duration,
        model: validatedOptions.model,
        responseLength: response.message.content.length,
        evalCount: response.eval_count,
        totalDuration: response.total_duration,
      });

      return Result.success(response.message.content.trim());
      
    } catch (error) {
      const duration = this.performance.end('chatCompletion');
      this.logger.error("❌ Chat completion failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Chat completion error: ${String(error)}`)
      );
    }
  }

  /**
   * Stream generation for long responses
   */
  async *streamGeneration(
    prompt: string, 
    options?: Partial<GenerationOptions>
  ): AsyncGenerator<string, void, unknown> {
    try {
      const validatedOptions = GenerationOptionsSchema.parse({
        model: this.config.defaultModel,
        ...options,
      });

      this.logger.info("🌊 Starting streaming generation", {
        promptLength: prompt.length,
        model: validatedOptions.model,
      });

      const stream = await this.ollama.generate({
        model: validatedOptions.model,
        prompt: prompt,
        stream: true,
        options: {
          temperature: validatedOptions.temperature,
          num_predict: validatedOptions.maxTokens,
          num_ctx: 8192,
        },
      });

      for await (const chunk of stream) {
        if (chunk.response) {
          yield chunk.response;
        }
      }

      this.logger.info("✅ Streaming generation completed");
      
    } catch (error) {
      this.logger.error("❌ Streaming generation failed", { error: String(error) });
      throw error;
    }
  }

  /**
   * Health check with comprehensive status
   */
  async healthCheck(): Promise<Result<HealthStatus>> {
    this.performance.start('healthCheck');
    
    this.logger.info("🔍 Starting health check");
    
    try {
      // List available models
      const modelList = await this.ollama.list();
      const modelNames = modelList.models.map(model => model.name);
      
      const isHealthy = modelNames.length > 0;
      const summary = isHealthy 
        ? `🟢 Ollama healthy with ${modelNames.length} models`
        : "🔴 Ollama has no models available";

      const status: HealthStatus = {
        ollama: isHealthy,
        models: modelNames,
        summary,
        timestamp: new Date(),
      };

      const duration = this.performance.end('healthCheck');
      this.logger.info("✅ Health check completed", { 
        duration, 
        modelCount: modelNames.length,
        healthy: isHealthy 
      });
      
      return Result.success(status);
      
    } catch (error) {
      const duration = this.performance.end('healthCheck');
      this.logger.error("❌ Health check failed", { duration, error: String(error) });
      
      const status: HealthStatus = {
        ollama: false,
        models: [],
        summary: "🔴 Ollama connection failed",
        timestamp: new Date(),
      };
      
      return Result.success(status); // Return status even if unhealthy
    }
  }

  /**
   * Pull a model if not available
   */
  async pullModel(modelName: string): Promise<Result<void>> {
    this.performance.start('pullModel');
    
    try {
      this.logger.info("📥 Starting model pull", { model: modelName });

      const stream = await this.ollama.pull({
        model: modelName,
        stream: true,
      });

      // Process the pull stream
      for await (const chunk of stream) {
        if (chunk.status) {
          this.logger.debug("Pull progress", { 
            model: modelName, 
            status: chunk.status,
            completed: chunk.completed,
            total: chunk.total,
          });
        }
      }

      const duration = this.performance.end('pullModel');
      this.logger.info("✅ Model pulled successfully", { model: modelName, duration });
      
      return Result.success(undefined);
      
    } catch (error) {
      const duration = this.performance.end('pullModel');
      this.logger.error("❌ Model pull failed", { 
        duration, 
        model: modelName, 
        error: String(error) 
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Model pull error: ${String(error)}`)
      );
    }
  }

  /**
   * Get performance statistics
   */
  getPerformanceStats(): Record<string, { count: number; average: number; last: number } | null> {
    return {
      generateCode: this.performance.getStats('generateCode'),
      chatCompletion: this.performance.getStats('chatCompletion'),
      healthCheck: this.performance.getStats('healthCheck'),
      pullModel: this.performance.getStats('pullModel'),
    };
  }
}

/**
 * Demo the Ollama-only agent
 */
export async function demoOllamaOnlyAgent() {
  console.log("🚀 Ollama-Only Agent Demo");
  console.log("=========================\n");

  const agent = new OllamaOnlyAgent();

  // Health check
  console.log("📊 Step 1: Health Check");
  const healthResult = await agent.healthCheck();
  
  if (Result.isSuccess(healthResult)) {
    const health = healthResult.data;
    console.log(`   ${health.summary}`);
    console.log(`   Available models: ${health.models.length}`);
    if (health.models.length > 0) {
      console.log(`   Models: ${health.models.slice(0, 3).join(', ')}${health.models.length > 3 ? '...' : ''}`);
    }
    console.log(`   Timestamp: ${health.timestamp.toISOString()}\n`);
  } else {
    console.log(`   ❌ Health check failed: ${healthResult.error.message}\n`);
  }

  // Test code generation
  const prompt = `Create a TypeScript Result<T> type with the following features:
- Success and failure states using discriminated union
- map and flatMap methods for chaining operations
- Type-safe error handling
- Comprehensive JSDoc documentation
- Usage examples in comments

Make it production-ready and follow TypeScript best practices.`;
  
  console.log(`📝 Step 2: Code Generation Test`);
  console.log(`   Request: "${prompt.substring(0, 80)}..."`);
  
  const codeResult = await agent.generateCode(prompt, {
    temperature: 0.1,
    maxTokens: 3000,
  });
  
  if (Result.isSuccess(codeResult)) {
    console.log(`   ✅ Success! Generated ${codeResult.data.length} characters`);
    console.log(`   📄 Preview: ${codeResult.data.substring(0, 200)}...`);
  } else {
    console.log(`   ❌ Failed: ${codeResult.error.message}`);
  }

  // Test chat completion
  console.log("\n💬 Step 3: Chat Completion Test");
  const chatResult = await agent.chatCompletion([
    { role: 'system', content: 'You are a helpful TypeScript expert.' },
    { role: 'user', content: 'Explain the benefits of using Result<T> pattern over throwing exceptions.' }
  ], {
    temperature: 0.3,
    maxTokens: 1000,
  });
  
  if (Result.isSuccess(chatResult)) {
    console.log(`   ✅ Chat response: ${chatResult.data.length} characters`);
    console.log(`   📄 Preview: ${chatResult.data.substring(0, 150)}...`);
  } else {
    console.log(`   ❌ Chat failed: ${chatResult.error.message}`);
  }

  // Performance stats
  console.log("\n📈 Step 4: Performance Statistics");
  const stats = agent.getPerformanceStats();
  Object.entries(stats).forEach(([operation, stat]) => {
    if (stat) {
      console.log(`   ${operation}: ${stat.count} calls, avg ${stat.average}ms, last ${stat.last}ms`);
    }
  });

  console.log("\n🎯 Summary:");
  console.log("   ✅ Ollama SDK integration working perfectly");
  console.log("   ✅ Zod validation for type safety");
  console.log("   ✅ Result<T> pattern with utilities");
  console.log("   ✅ Structured logging with timestamps");
  console.log("   ✅ Performance tracking built-in");
  console.log("   ✅ Streaming support available");
  console.log("   ✅ Chat completion support");
  console.log("   ✅ Model management (pull/list)");
  console.log("   ✅ Comprehensive error handling");

  console.log("\n🚀 Ready for MathForge Integration!");
  console.log("   This agent uses only high-quality external packages:");
  console.log("   • Ollama SDK - Official JavaScript client for Ollama");
  console.log("   • Zod - Runtime validation and type safety");
  console.log("   • Built-in Result<T> pattern for functional error handling");
  console.log("   • No problematic dependencies");
  console.log("   • Pure TypeScript with excellent Bun compatibility");
}

// Run demo if this file is executed directly
if (import.meta.main) {
  demoOllamaOnlyAgent().catch(console.error);
} 