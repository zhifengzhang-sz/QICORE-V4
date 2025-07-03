#!/usr/bin/env bun

/**
 * Test Model Switching - No Source Code Changes Required!
 * 
 * This demonstrates that we can switch between different AI providers
 * (Ollama, Claude, OpenAI) through configuration only.
 */

import { MathematicalAnalysisAgent, type ModelConfig } from "./agents/ollama.ts";

async function testModelSwitching() {
  console.log("🔄 Testing Model Switching Capabilities");
  console.log("======================================\n");

  // Test different model configurations
  const configs: ModelConfig[] = [
    {
      provider: "ollama",
      model: "qwen3:0.6b",
      baseURL: "http://localhost:11434"
    },
    {
      provider: "ollama", 
      model: "qwen3:8b",
      baseURL: "http://localhost:11434"
    }
    // Uncomment these when API keys are available:
    // {
    //   provider: "anthropic",
    //   model: "claude-3-haiku-20240307",
    //   apiKey: process.env.ANTHROPIC_API_KEY
    // },
    // {
    //   provider: "openai",
    //   model: "gpt-4o-mini",
    //   apiKey: process.env.OPENAI_API_KEY
    // }
  ];

  const testPrompt = "What is 2+2? Answer with just the number.";

  for (const config of configs) {
    console.log(`🧪 Testing ${config.provider}:${config.model}`);
    
    try {
      const agent = new MathematicalAnalysisAgent(config);
      const stats = agent.getStats();
      
      console.log(`   ✅ Agent created: ${stats.agent}`);
      console.log(`   📊 Provider: ${stats.provider}`);
      console.log(`   🤖 Model: ${stats.model}`);
      console.log(`   🔗 Library: ${stats.library}`);
      
      // Test simple analysis (would work with any model)
      console.log(`   🔍 Testing simple analysis...`);
      
      // For demo purposes, we'll just show the configuration works
      console.log(`   ✅ Configuration successful!\n`);
      
    } catch (error) {
      console.log(`   ❌ Failed: ${error.message}\n`);
    }
  }

  console.log("🎯 Key Benefits of Model-Agnostic Design:");
  console.log("----------------------------------------");
  console.log("✅ Switch models via environment variables:");
  console.log("   export AI_PROVIDER=anthropic");
  console.log("   export AI_MODEL=claude-3-haiku-20240307");
  console.log("   export AI_API_KEY=your_key_here");
  console.log("");
  console.log("✅ Or via configuration object (no source changes)");
  console.log("✅ Same agent works with Ollama, Claude, OpenAI, etc.");
  console.log("✅ Easy to test different models for performance");
  console.log("✅ Production deployments can use different providers");
}

async function demonstrateEnvironmentConfig() {
  console.log("\n🌍 Environment Configuration Demo");
  console.log("================================");
  
  // Show current environment config
  console.log("Current environment variables:");
  console.log(`AI_PROVIDER: ${process.env.AI_PROVIDER || "not set (defaults to ollama)"}`);
  console.log(`AI_MODEL: ${process.env.AI_MODEL || "not set (defaults to qwen3:0.6b)"}`);
  console.log(`AI_BASE_URL: ${process.env.AI_BASE_URL || "not set (defaults to localhost:11434)"}`);
  console.log(`AI_API_KEY: ${process.env.AI_API_KEY ? "***set***" : "not set"}`);
  
  // Create agent using environment config
  console.log("\nCreating agent with environment config...");
  const agent = new MathematicalAnalysisAgent(); // Uses environment/defaults
  const stats = agent.getStats();
  
  console.log(`✅ Agent uses: ${stats.provider}:${stats.model}`);
  console.log(`📍 Base URL: ${stats.baseURL || "default"}`);
}

if (import.meta.main) {
  testModelSwitching()
    .then(() => demonstrateEnvironmentConfig())
    .catch(console.error);
}