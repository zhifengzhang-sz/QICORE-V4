#!/usr/bin/env bun

// Test the workspace import from the app

console.log("🧪 Testing workspace import from app...");

try {
  console.log("Testing @qicore/agent-lib import...");
  const lib = await import("@qicore/agent-lib");
  console.log("✅ @qicore/agent-lib works");
  console.log("Available exports:", Object.keys(lib));
} catch (e) {
  console.log("❌ @qicore/agent-lib failed:", e.message);
}

try {
  console.log("Testing direct lib path import...");
  const lib = await import("../lib/src/index.ts");
  console.log("✅ Direct lib path works");
  console.log("Available exports:", Object.keys(lib));
} catch (e) {
  console.log("❌ Direct lib path failed:", e.message);
}