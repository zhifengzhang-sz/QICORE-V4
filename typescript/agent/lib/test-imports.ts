#!/usr/bin/env bun

// Test imports to see what's working and what's not

console.log("🧪 Testing imports...");

try {
  console.log("Testing memory.js import...");
  await import("./src/qimcp/tools/memory.js");
  console.log("✅ memory.js works");
} catch (e) {
  console.log("❌ memory.js failed:", e.message);
}

try {
  console.log("Testing memory.ts import...");
  await import("./src/qimcp/tools/memory.ts");
  console.log("✅ memory.ts works");
} catch (e) {
  console.log("❌ memory.ts failed:", e.message);
}

try {
  console.log("Testing qiagent index...");
  await import("./src/qiagent/index.js");
  console.log("✅ qiagent index works");
} catch (e) {
  console.log("❌ qiagent index failed:", e.message);
}

try {
  console.log("Testing full lib import...");
  await import("./src/index.ts");
  console.log("✅ Full lib import works");
} catch (e) {
  console.log("❌ Full lib import failed:", e.message);
}