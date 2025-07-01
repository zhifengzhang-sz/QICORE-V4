/**
 * Simple test to verify LLM connection works
 */

async function testHello() {
  console.log("🤖 Testing LLM connection...");
  
  try {
    const response = await fetch("http://localhost:11434/api/generate", {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        model: "qwen3:8b",
        prompt: "Say: Hello from LLM",
        stream: false,
        options: {
          temperature: 0.1,
          num_predict: 10,
        },
      }),
    });

    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }

    const data = await response.json() as { response: string };
    let reply = data.response.trim();
    
    // Clean up thinking tags and other artifacts
    reply = reply
      .replace(/<think>[\s\S]*?<\/think>/gi, '') // Remove thinking tags completely
      .replace(/^[^a-zA-Z!]*/, '') // Remove leading non-letters (except !)
      .trim();
    
    console.log("✅ LLM Response:", reply);
    console.log("🎉 Connection successful!");
    
  } catch (error) {
    console.error("❌ Connection failed:", error);
  }
}

if (import.meta.main) {
  await testHello();
} 