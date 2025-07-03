#!/usr/bin/env bun

/**
 * Quick test of AI integration - just test the core generateText functionality
 */

import { generateText } from "@qicore/agent-lib";
import { ollama } from "ollama-ai-provider";

async function testAI() {
  console.log("🧪 Testing QiAgent + Ollama Integration");
  console.log("=====================================\n");

  try {
    // Use environment variable or default
    const modelName = process.env.AI_MODEL || "qwen3:0.6b";
    
    // Create the model
    const model = ollama(modelName, {
      baseURL: "http://localhost:11434"
    });

    console.log(`🚀 Model created: ${modelName}`);
    console.log("🔍 Sending test prompt...\n");

    const startTime = Date.now();
    
    // Test with longer prompt to better utilize GPU
    const result = await generateText({
      model,
      prompt: `Analyze this mathematical contract in detail:

Result<T> is a type that represents either a successful value of type T or an error. It forms a monad with the following operations:
- return(value): Creates a successful Result containing the value
- flatMap(function): If successful, applies function to the value; if error, returns the error

Please provide a comprehensive analysis of:
1. The algebraic structure (why it's a monad)
2. The identity and associativity laws
3. How it compares to other monadic types like Option<T>
4. Practical applications in error handling
5. Implementation considerations

Provide detailed mathematical reasoning.`,
      temperature: 0.1,
      maxTokens: 500,
    });

    const duration = Date.now() - startTime;
    
    console.log("✅ AI Response received!");
    console.log(`⏱️  Duration: ${duration}ms`);
    console.log("📝 Response:");
    console.log("---");
    console.log(result.text);
    console.log("---\n");
    
    console.log("🎉 SUCCESS: QiAgent + Ollama integration is working!");
    console.log("✅ Real AI execution confirmed (not just prompt generation)");
    
  } catch (error) {
    console.error("❌ AI integration test failed:", error);
    process.exit(1);
  }
}

if (import.meta.main) {
  testAI().catch(console.error);
}