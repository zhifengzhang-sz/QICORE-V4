import { Ollama } from 'ollama';
import { z } from 'zod';

// Use Zod for validation and type safety
const GenerationOptionsSchema = z.object({
  model: z.string().default("qwen3:0.6b"),
  temperature: z.number().min(0).max(2).default(0.7),
  maxTokens: z.number().positive().default(4000),
  timeout: z.number().positive().default(30000),
  keepAlive: z.union([z.string(), z.number()]).optional(),
});

const EmbeddingOptionsSchema = z.object({
  model: z.string().default("nomic-embed-text"),
  truncate: z.boolean().default(true),
  keepAlive: z.union([z.string(), z.number()]).optional(),
});

const ModelOperationSchema = z.object({
  model: z.string(),
  insecure: z.boolean().default(false),
  stream: z.boolean().default(true),
});

const HealthStatusSchema = z.object({
  ollama: z.boolean(),
  models: z.array(z.string()),
  summary: z.string(),
  timestamp: z.date(),
});

// Enhanced types
type GenerationOptions = z.infer<typeof GenerationOptionsSchema>;
type EmbeddingOptions = z.infer<typeof EmbeddingOptionsSchema>;
type ModelOperation = z.infer<typeof ModelOperationSchema>;
type HealthStatus = z.infer<typeof HealthStatusSchema>;

// Message types for chat
interface ChatMessage {
  role: 'system' | 'user' | 'assistant';
  content: string;
  images?: string[];
}

// Tool types for function calling
interface Tool {
  type: 'function';
  function: {
    name: string;
    description: string;
    parameters: Record<string, any>;
  };
}

// Simple Result pattern using discriminated union
type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E };

const Result = {
  success: <T>(data: T): Result<T> => ({ success: true, data }),
  failure: <T, E = Error>(error: E): Result<T, E> => ({ success: false, error }),
  
  // Utility methods
  map: <T, U, E>(result: Result<T, E>, fn: (data: T) => U): Result<U, E> =>
    result.success ? Result.success(fn(result.data)) : result,
    
  flatMap: <T, U, E>(result: Result<T, E>, fn: (data: T) => Result<U, E>): Result<U, E> =>
    result.success ? fn(result.data) : result,
    
  isSuccess: <T, E>(result: Result<T, E>): result is { success: true; data: T } =>
    result.success,
    
  isFailure: <T, E>(result: Result<T, E>): result is { success: false; error: E } =>
    !result.success,
};

// Export Result for use in other files
export { Result };

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
    defaultModel: "qwen3:0.6b",
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
        keep_alive: validatedOptions.keepAlive,
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
    messages: ChatMessage[],
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
        keep_alive: validatedOptions.keepAlive,
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
   * Chat with tools/function calling support
   */
  async chatWithTools(
    messages: ChatMessage[],
    tools: Tool[],
    options?: Partial<GenerationOptions>
  ): Promise<Result<{ content: string; toolCalls?: any[] }>> {
    this.performance.start('chatWithTools');
    
    try {
      const validatedOptions = GenerationOptionsSchema.parse({
        model: this.config.defaultModel,
        ...options,
      });

      this.logger.info("🔧 Starting chat with tools", {
        messageCount: messages.length,
        toolCount: tools.length,
        model: validatedOptions.model,
      });

      const response = await this.ollama.chat({
        model: validatedOptions.model,
        messages: messages,
        tools: tools,
        stream: false,
        keep_alive: validatedOptions.keepAlive,
        options: {
          temperature: validatedOptions.temperature,
          num_predict: validatedOptions.maxTokens,
          num_ctx: 8192,
        },
      });

      const duration = this.performance.end('chatWithTools');
      
      this.logger.info("✅ Chat with tools successful", {
        duration,
        model: validatedOptions.model,
        responseLength: response.message.content.length,
        toolCalls: response.message.tool_calls?.length || 0,
      });

      return Result.success({
        content: response.message.content.trim(),
        toolCalls: response.message.tool_calls,
      });
      
    } catch (error) {
      const duration = this.performance.end('chatWithTools');
      this.logger.error("❌ Chat with tools failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Chat with tools error: ${String(error)}`)
      );
    }
  }

  /**
   * Generate embeddings for text
   */
  async generateEmbeddings(
    input: string | string[],
    options?: Partial<EmbeddingOptions>
  ): Promise<Result<number[][]>> {
    this.performance.start('generateEmbeddings');
    
    try {
      const validatedOptions = EmbeddingOptionsSchema.parse(options || {});

      this.logger.info("🔢 Starting embedding generation", {
        inputType: Array.isArray(input) ? 'array' : 'string',
        inputLength: Array.isArray(input) ? input.length : 1,
        model: validatedOptions.model,
      });

      const response = await this.ollama.embed({
        model: validatedOptions.model,
        input: input,
        truncate: validatedOptions.truncate,
        keep_alive: validatedOptions.keepAlive,
      });

      const duration = this.performance.end('generateEmbeddings');
      
      this.logger.info("✅ Embedding generation successful", {
        duration,
        model: validatedOptions.model,
        embeddingCount: response.embeddings.length,
        embeddingDimension: response.embeddings[0]?.length || 0,
      });

      return Result.success(response.embeddings);
      
    } catch (error) {
      const duration = this.performance.end('generateEmbeddings');
      this.logger.error("❌ Embedding generation failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Embedding generation error: ${String(error)}`)
      );
    }
  }

  /**
   * List all available models
   */
  async listModels(): Promise<Result<any[]>> {
    this.performance.start('listModels');
    
    try {
      this.logger.info("📋 Listing available models");

      const response = await this.ollama.list();

      const duration = this.performance.end('listModels');
      
      this.logger.info("✅ Model listing successful", {
        duration,
        modelCount: response.models.length,
      });

      return Result.success(response.models);
      
    } catch (error) {
      const duration = this.performance.end('listModels');
      this.logger.error("❌ Model listing failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Model listing error: ${String(error)}`)
      );
    }
  }

  /**
   * Show detailed information about a model
   */
  async showModel(modelName: string): Promise<Result<any>> {
    this.performance.start('showModel');
    
    try {
      this.logger.info("🔍 Showing model details", { model: modelName });

      const response = await this.ollama.show({ model: modelName });

      const duration = this.performance.end('showModel');
      
      this.logger.info("✅ Model details retrieved", {
        duration,
        model: modelName,
        format: response.details?.format,
        family: response.details?.family,
        parameterSize: response.details?.parameter_size,
      });

      return Result.success(response);
      
    } catch (error) {
      const duration = this.performance.end('showModel');
      this.logger.error("❌ Model details failed", {
        duration,
        model: modelName,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Model details error: ${String(error)}`)
      );
    }
  }

  /**
   * List currently running models
   */
  async listRunningModels(): Promise<Result<any[]>> {
    this.performance.start('listRunningModels');
    
    try {
      this.logger.info("🏃 Listing running models");

      const response = await this.ollama.ps();

      const duration = this.performance.end('listRunningModels');
      
      this.logger.info("✅ Running models listed", {
        duration,
        runningCount: response.models.length,
      });

      return Result.success(response.models);
      
    } catch (error) {
      const duration = this.performance.end('listRunningModels');
      this.logger.error("❌ Running models listing failed", {
        duration,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Running models error: ${String(error)}`)
      );
    }
  }

  /**
   * Delete a model
   */
  async deleteModel(modelName: string): Promise<Result<void>> {
    this.performance.start('deleteModel');
    
    try {
      this.logger.info("🗑️ Deleting model", { model: modelName });

      await this.ollama.delete({ model: modelName });

      const duration = this.performance.end('deleteModel');
      
      this.logger.info("✅ Model deleted successfully", {
        duration,
        model: modelName,
      });

      return Result.success(undefined);
      
    } catch (error) {
      const duration = this.performance.end('deleteModel');
      this.logger.error("❌ Model deletion failed", {
        duration,
        model: modelName,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Model deletion error: ${String(error)}`)
      );
    }
  }

  /**
   * Copy a model
   */
  async copyModel(source: string, destination: string): Promise<Result<void>> {
    this.performance.start('copyModel');
    
    try {
      this.logger.info("📋 Copying model", { source, destination });

      await this.ollama.copy({ source, destination });

      const duration = this.performance.end('copyModel');
      
      this.logger.info("✅ Model copied successfully", {
        duration,
        source,
        destination,
      });

      return Result.success(undefined);
      
    } catch (error) {
      const duration = this.performance.end('copyModel');
      this.logger.error("❌ Model copying failed", {
        duration,
        source,
        destination,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Model copying error: ${String(error)}`)
      );
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
        keep_alive: validatedOptions.keepAlive,
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
   * Stream chat completion
   */
  async *streamChat(
    messages: ChatMessage[],
    options?: Partial<GenerationOptions>
  ): AsyncGenerator<string, void, unknown> {
    try {
      const validatedOptions = GenerationOptionsSchema.parse({
        model: this.config.defaultModel,
        ...options,
      });

      this.logger.info("💬🌊 Starting streaming chat", {
        messageCount: messages.length,
        model: validatedOptions.model,
      });

      const stream = await this.ollama.chat({
        model: validatedOptions.model,
        messages: messages,
        stream: true,
        keep_alive: validatedOptions.keepAlive,
        options: {
          temperature: validatedOptions.temperature,
          num_predict: validatedOptions.maxTokens,
          num_ctx: 8192,
        },
      });

      for await (const chunk of stream) {
        if (chunk.message?.content) {
          yield chunk.message.content;
        }
      }

      this.logger.info("✅ Streaming chat completed");
      
    } catch (error) {
      this.logger.error("❌ Streaming chat failed", { error: String(error) });
      throw error;
    }
  }

  /**
   * Get performance statistics
   */
  getPerformanceStats(): Record<string, { count: number; average: number; last: number } | null> {
    return {
      generateCode: this.performance.getStats('generateCode'),
      chatCompletion: this.performance.getStats('chatCompletion'),
      chatWithTools: this.performance.getStats('chatWithTools'),
      generateEmbeddings: this.performance.getStats('generateEmbeddings'),
      listModels: this.performance.getStats('listModels'),
      showModel: this.performance.getStats('showModel'),
      listRunningModels: this.performance.getStats('listRunningModels'),
      deleteModel: this.performance.getStats('deleteModel'),
      copyModel: this.performance.getStats('copyModel'),
      healthCheck: this.performance.getStats('healthCheck'),
      pullModel: this.performance.getStats('pullModel'),
    };
  }

  /**
   * Analyze mathematical contracts for algebraic completeness
   * Based on QiCore v4.0 enhanced mathematical specifications
   */
  async analyzeMathematicalContracts(
    contractText: string,
    targetComponent: string
  ): Promise<Result<MathematicalAnalysis>> {
    this.performance.start('analyzeMathematicalContracts');
    
    try {
      this.logger.info("🔬 Starting mathematical contract analysis", {
        component: targetComponent,
        contractLength: contractText.length,
      });

      // Use Claude for deep mathematical analysis
      const claudeAnalysis = await this.chatCompletion([
        { 
          role: 'system', 
          content: `You are a mathematical verification expert specializing in algebraic structures and API design.

Analyze the provided mathematical contract specifications and output your results in this EXACT format:

## ALGEBRAIC STRUCTURES IDENTIFIED:
- [List each structure found: Monad, Monoid, Functor, Semi-group, Category, etc.]

## MATHEMATICAL COMPLETENESS ASSESSMENT:
Completeness Score: [0-100]%
Reasoning: [Explain why this score]

## INEVITABLE API PATTERNS:
- [List patterns that are mathematically forced by the laws]

## GAPS AND MISSING LAWS:
- [List any missing mathematical properties or laws]

## DETAILED ANALYSIS:
[Provide detailed mathematical reasoning for your findings]

Focus on mathematical rigor and be precise about which algebraic structures are present based on the laws provided.` 
        },
        { 
          role: 'user', 
          content: `Analyze this mathematical contract for ${targetComponent}:\n\n${contractText}` 
        }
      ], {
        temperature: 0.1,
        maxTokens: 3000,
      });

      if (!Result.isSuccess(claudeAnalysis)) {
        return Result.failure(claudeAnalysis.error);
      }

      // Use Ollama for pattern matching verification
      const ollamaVerification = await this.chatCompletion([
        {
          role: 'system',
          content: `You are a pattern matching expert for mathematical structures in software.
          
          Given a mathematical analysis, verify:
          1. Pattern recognition accuracy for algebraic structures
          2. Completeness of specified laws
          3. API pattern inevitability assessment
          4. Implementation guidance derivation
          
          Provide concise verification and identify any missed patterns.`
        },
        {
          role: 'user',
          content: `Verify this mathematical analysis:\n\n${claudeAnalysis.data}\n\nOriginal contract:\n${contractText.substring(0, 1000)}...`
        }
      ], {
        temperature: 0.2,
        maxTokens: 2000,
      });

      if (!Result.isSuccess(ollamaVerification)) {
        return Result.failure(ollamaVerification.error);
      }

      const duration = this.performance.end('analyzeMathematicalContracts');
      
      this.logger.info("✅ Mathematical contract analysis completed", {
        duration,
        component: targetComponent,
        analysisLength: claudeAnalysis.data.length,
        verificationLength: ollamaVerification.data.length,
      });

      return Result.success({
        component: targetComponent,
        claudeAnalysis: claudeAnalysis.data,
        ollamaVerification: ollamaVerification.data,
        algebraicStructures: this.extractAlgebraicStructures(claudeAnalysis.data),
        completenessScore: this.assessCompleteness(claudeAnalysis.data),
        inevitablePatterns: this.extractInevitablePatterns(claudeAnalysis.data),
        implementationGuidance: this.extractImplementationGuidance(ollamaVerification.data),
        gaps: this.identifyGaps(claudeAnalysis.data, ollamaVerification.data),
      });
      
    } catch (error) {
      const duration = this.performance.end('analyzeMathematicalContracts');
      this.logger.error("❌ Mathematical contract analysis failed", {
        duration,
        component: targetComponent,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Mathematical analysis error: ${String(error)}`)
      );
    }
  }

  /**
   * Extract algebraic structures from analysis text
   */
  private extractAlgebraicStructures(analysis: string): string[] {
    const structures = [];
    if (analysis.includes('monad') || analysis.includes('Monad')) structures.push('Monad');
    if (analysis.includes('monoid') || analysis.includes('Monoid')) structures.push('Monoid');
    if (analysis.includes('functor') || analysis.includes('Functor')) structures.push('Functor');
    if (analysis.includes('semi-group') || analysis.includes('Semi-group')) structures.push('Semi-group');
    if (analysis.includes('category') || analysis.includes('Category')) structures.push('Category');
    return structures;
  }

  /**
   * Assess mathematical completeness based on analysis
   */
  private assessCompleteness(analysis: string): number {
    let score = 0;
    // Check for law completeness indicators
    if (analysis.includes('associativity') || analysis.includes('Associativity')) score += 20;
    if (analysis.includes('identity') || analysis.includes('Identity')) score += 20;
    if (analysis.includes('composition') || analysis.includes('Composition')) score += 20;
    if (analysis.includes('complete') || analysis.includes('sufficient')) score += 20;
    if (analysis.includes('inevitable') || analysis.includes('forces')) score += 20;
    return Math.min(score, 100);
  }

  /**
   * Extract inevitable API patterns from analysis
   */
  private extractInevitablePatterns(analysis: string): string[] {
    const patterns = [];
    if (analysis.includes('fluent') || analysis.includes('chaining')) patterns.push('Fluent Chaining');
    if (analysis.includes('builder') || analysis.includes('Builder')) patterns.push('Builder Pattern');
    if (analysis.includes('pipeline') || analysis.includes('Pipeline')) patterns.push('Pipeline Composition');
    if (analysis.includes('monadic') || analysis.includes('flatMap')) patterns.push('Monadic Composition');
    return patterns;
  }

  /**
   * Extract implementation guidance from verification
   */
  private extractImplementationGuidance(verification: string): string[] {
    const guidance = [];
    const lines = verification.split('\n');
    for (const line of lines) {
      if (line.includes('implement') || line.includes('should') || line.includes('must')) {
        guidance.push(line.trim());
      }
    }
    return guidance.slice(0, 10); // Limit to top 10 guidance points
  }

  /**
   * Identify gaps in mathematical specification
   */
  private identifyGaps(analysis: string, verification: string): string[] {
    const gaps = [];
    if (analysis.includes('missing') || verification.includes('missing')) {
      gaps.push('Missing mathematical laws identified');
    }
    if (analysis.includes('incomplete') || verification.includes('incomplete')) {
      gaps.push('Incomplete specification detected');
    }
    if (analysis.includes('ambiguous') || verification.includes('ambiguous')) {
      gaps.push('Ambiguous constraints found');
    }
    return gaps;
  }

  /**
   * Verify mathematical laws for a specific algebraic structure
   */
  async verifyAlgebraicLaws(
    structure: 'Monad' | 'Monoid' | 'Functor' | 'Semi-group',
    implementation: string
  ): Promise<Result<LawVerification>> {
    this.performance.start('verifyAlgebraicLaws');
    
    try {
      this.logger.info("⚖️ Verifying algebraic laws", {
        structure,
        implementationLength: implementation.length,
      });

      const lawsToVerify = this.getLawsForStructure(structure);
      
      const verification = await this.chatCompletion([
        {
          role: 'system',
          content: `You are a mathematical law verification expert.
          
          Verify that the provided implementation satisfies the mathematical laws for ${structure}:
          ${lawsToVerify.join('\n')}
          
          For each law, determine:
          1. Whether the implementation satisfies it
          2. Evidence or counterexample
          3. Severity of any violations
          
          Be rigorous and precise in your verification.`
        },
        {
          role: 'user',
          content: `Verify ${structure} laws for this implementation:\n\n${implementation}`
        }
      ], {
        temperature: 0.1,
        maxTokens: 2000,
      });

      if (!Result.isSuccess(verification)) {
        return Result.failure(verification.error);
      }

      const duration = this.performance.end('verifyAlgebraicLaws');
      
      this.logger.info("✅ Algebraic law verification completed", {
        duration,
        structure,
        verificationLength: verification.data.length,
      });

      return Result.success({
        structure,
        laws: lawsToVerify,
        verification: verification.data,
        satisfied: this.parseLawSatisfaction(verification.data),
        violations: this.extractViolations(verification.data),
      });
      
    } catch (error) {
      const duration = this.performance.end('verifyAlgebraicLaws');
      this.logger.error("❌ Algebraic law verification failed", {
        duration,
        structure,
        error: String(error),
      });
      
      return Result.failure(
        error instanceof Error ? error : new Error(`Law verification error: ${String(error)}`)
      );
    }
  }

  /**
   * Get the mathematical laws for a specific algebraic structure
   */
  private getLawsForStructure(structure: string): string[] {
    const laws = {
      Monad: [
        'Left Identity: η(a) >>= f = f(a)',
        'Right Identity: m >>= η = m',
        'Associativity: (m >>= f) >>= g = m >>= (λx. f(x) >>= g)'
      ],
      Monoid: [
        'Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)',
        'Left Identity: ∅ ⊕ a = a',
        'Right Identity: a ⊕ ∅ = a'
      ],
      Functor: [
        'Identity: map(id) = id',
        'Composition: map(f ∘ g) = map(f) ∘ map(g)'
      ],
      'Semi-group': [
        'Associativity: (a ⊕ b) ⊕ c = a ⊕ (b ⊕ c)',
        'Closure: a ⊕ b is in the same set as a and b'
      ]
    };
    return laws[structure] || [];
  }

  /**
   * Parse law satisfaction from verification text
   */
  private parseLawSatisfaction(verification: string): boolean {
    const satisfiedCount = (verification.match(/satisfied|✓|✅|passes/gi) || []).length;
    const violatedCount = (verification.match(/violated|✗|❌|fails/gi) || []).length;
    return satisfiedCount > violatedCount;
  }

  /**
   * Extract law violations from verification text
   */
  private extractViolations(verification: string): string[] {
    const violations = [];
    const lines = verification.split('\n');
    for (const line of lines) {
      if (line.includes('violation') || line.includes('violated') || line.includes('fails')) {
        violations.push(line.trim());
      }
    }
    return violations;
  }
}

/**
 * Demo the enhanced Ollama-only agent
 */
export async function demoOllamaOnlyAgent() {
  console.log("🚀 Enhanced Ollama-Only Agent Demo");
  console.log("===================================\n");

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

  // List all models
  console.log("📋 Step 2: List All Models");
  const modelsResult = await agent.listModels();
  if (Result.isSuccess(modelsResult)) {
    console.log(`   ✅ Found ${modelsResult.data.length} models`);
    modelsResult.data.slice(0, 3).forEach(model => {
      console.log(`   • ${model.name} (${model.size} bytes, modified: ${model.modified_at})`);
    });
  } else {
    console.log(`   ❌ Failed to list models: ${modelsResult.error.message}`);
  }

  // Show model details
  console.log("\n🔍 Step 3: Show Model Details");
  const showResult = await agent.showModel("qwen3:0.6b");
  if (Result.isSuccess(showResult)) {
    const model = showResult.data;
    console.log(`   ✅ Model: ${model.details?.format} format`);
    console.log(`   Family: ${model.details?.family}`);
    console.log(`   Parameters: ${model.details?.parameter_size}`);
    console.log(`   Quantization: ${model.details?.quantization_level}`);
  } else {
    console.log(`   ❌ Failed to show model: ${showResult.error.message}`);
  }

  // Generate embeddings
  console.log("\n🔢 Step 4: Generate Embeddings");
  const embeddingResult = await agent.generateEmbeddings([
    "The quick brown fox jumps over the lazy dog",
    "Machine learning is transforming software development"
  ], { model: "nomic-embed-text" });
  
  if (Result.isSuccess(embeddingResult)) {
    console.log(`   ✅ Generated ${embeddingResult.data.length} embeddings`);
    console.log(`   Dimension: ${embeddingResult.data[0]?.length || 0}`);
    console.log(`   First embedding preview: [${embeddingResult.data[0]?.slice(0, 5).map(n => n.toFixed(3)).join(', ')}...]`);
  } else {
    console.log(`   ❌ Failed to generate embeddings: ${embeddingResult.error.message}`);
  }

  // Test code generation
  const prompt = `Create a TypeScript function that:
- Takes an array of numbers as input
- Returns the median value
- Handles edge cases (empty array, single element)
- Includes comprehensive JSDoc documentation
- Uses modern TypeScript features`;
  
  console.log(`\n📝 Step 5: Code Generation Test`);
  console.log(`   Request: "${prompt.substring(0, 80)}..."`);
  
  const codeResult = await agent.generateCode(prompt, {
    temperature: 0.1,
    maxTokens: 2000,
  });
  
  if (Result.isSuccess(codeResult)) {
    console.log(`   ✅ Success! Generated ${codeResult.data.length} characters`);
    console.log(`   📄 Preview: ${codeResult.data.substring(0, 200)}...`);
  } else {
    console.log(`   ❌ Failed: ${codeResult.error.message}`);
  }

  // Test chat with tools
  console.log("\n🔧 Step 6: Chat with Tools Test");
  const tools: Tool[] = [
    {
      type: 'function',
      function: {
        name: 'calculate',
        description: 'Perform basic mathematical calculations',
        parameters: {
          type: 'object',
          properties: {
            operation: { type: 'string', enum: ['add', 'subtract', 'multiply', 'divide'] },
            a: { type: 'number' },
            b: { type: 'number' }
          },
          required: ['operation', 'a', 'b']
        }
      }
    }
  ];

  const toolsResult = await agent.chatWithTools([
    { role: 'system', content: 'You are a helpful assistant that can perform calculations.' },
    { role: 'user', content: 'What is 15 multiplied by 23?' }
  ], tools, {
    temperature: 0.3,
    maxTokens: 1000,
  });
  
  if (Result.isSuccess(toolsResult)) {
    console.log(`   ✅ Chat response: ${toolsResult.data.content.length} characters`);
    console.log(`   🔧 Tool calls: ${toolsResult.data.toolCalls?.length || 0}`);
    console.log(`   📄 Preview: ${toolsResult.data.content.substring(0, 150)}...`);
  } else {
    console.log(`   ❌ Chat with tools failed: ${toolsResult.error.message}`);
  }

  // List running models
  console.log("\n🏃 Step 7: List Running Models");
  const runningResult = await agent.listRunningModels();
  if (Result.isSuccess(runningResult)) {
    console.log(`   ✅ Running models: ${runningResult.data.length}`);
    runningResult.data.forEach(model => {
      console.log(`   • ${model.name} (${model.size_vram} VRAM)`);
    });
  } else {
    console.log(`   ❌ Failed to list running models: ${runningResult.error.message}`);
  }

  // Performance stats
  console.log("\n📈 Step 8: Performance Statistics");
  const stats = agent.getPerformanceStats();
  Object.entries(stats).forEach(([operation, stat]) => {
    if (stat) {
      console.log(`   ${operation}: ${stat.count} calls, avg ${stat.average}ms, last ${stat.last}ms`);
    }
  });

  console.log("\n🎯 Enhanced Agent Summary:");
  console.log("   ✅ Complete Ollama SDK integration");
  console.log("   ✅ All core API methods implemented");
  console.log("   ✅ Streaming support for generation and chat");
  console.log("   ✅ Embedding generation capability");
  console.log("   ✅ Tool/function calling support");
  console.log("   ✅ Model management (list, show, pull, delete, copy)");
  console.log("   ✅ Running model monitoring");
  console.log("   ✅ Comprehensive error handling with Result<T>");
  console.log("   ✅ Performance tracking and metrics");
  console.log("   ✅ Structured logging with metadata");
  console.log("   ✅ Zod validation for type safety");

  console.log("\n🚀 Ready for Advanced MathForge Research!");
  console.log("   This enhanced agent provides a complete foundation for:");
  console.log("   • Multi-modal mathematical reasoning");
  console.log("   • Tool-assisted problem solving");
  console.log("   • Embedding-based similarity search");
  console.log("   • Streaming interactive experiences");
  console.log("   • Production-ready error handling");
  console.log("   • Performance monitoring and optimization");
}

// Run demo if this file is executed directly
if (import.meta.main) {
  demoOllamaOnlyAgent().catch(console.error);
}

// Type definitions for mathematical analysis
interface MathematicalAnalysis {
  component: string;
  claudeAnalysis: string;
  ollamaVerification: string;
  algebraicStructures: string[];
  completenessScore: number;
  inevitablePatterns: string[];
  implementationGuidance: string[];
  gaps: string[];
}

interface LawVerification {
  structure: string;
  laws: string[];
  verification: string;
  satisfied: boolean;
  violations: string[];
} 