import { OllamaOnlyAgent } from './ollama-only-agent.ts';

/**
 * Super simple hello world test for Approach 0
 * Just test basic agent functionality with file operations
 */
export async function helloTest() {
  console.log("👋 Hello Test: Simple Agent Validation");
  console.log("======================================\n");

  const agent = new OllamaOnlyAgent();
  
  // Simple prompt: create a file, write content, read it, modify it
  const prompt = `Create a TypeScript function that:
1. Creates a file called "hello.md" 
2. Writes "Hello World" to it
3. Reads the file content
4. Adds "AI Generated" to the content
5. Writes it back

Make it simple and working code.`;

  console.log("📝 Testing simple file operations...");
  console.log(`Prompt: ${prompt.substring(0, 80)}...`);
  
  const startTime = Date.now();
  
  try {
    const result = await agent.generateCode(prompt, {
      temperature: 0.1,
      maxTokens: 800,
    });

    const responseTime = Date.now() - startTime;

    if (result.success) {
      console.log(`✅ SUCCESS! (${responseTime}ms)`);
      console.log(`📄 Generated ${result.data.length} characters`);
      console.log("\n📋 Generated Code:");
      console.log("==================");
      console.log(result.data);
      
      // Try to save the generated code to test if it works
      console.log("\n💾 Saving generated code to test-output.ts...");
      await Bun.write("test-output.ts", result.data);
      console.log("✅ Code saved successfully!");
      
      return {
        success: true,
        responseTime,
        codeLength: result.data.length,
        generatedCode: result.data
      };
    } else {
      console.log(`❌ FAILED: ${result.error.message}`);
      return {
        success: false,
        error: result.error.message,
        responseTime
      };
    }
  } catch (error) {
    const responseTime = Date.now() - startTime;
    console.log(`❌ ERROR: ${String(error)}`);
    return {
      success: false,
      error: String(error),
      responseTime
    };
  }
}

// Run test if this file is executed directly
if (import.meta.main) {
  helloTest()
    .then(result => {
      console.log("\n🎯 Test Result:", result.success ? "PASSED" : "FAILED");
      if (result.success) {
        console.log("✅ Agent is working and ready!");
      }
    })
    .catch(console.error);
} 