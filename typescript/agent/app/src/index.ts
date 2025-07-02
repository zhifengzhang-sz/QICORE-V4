#!/usr/bin/env bun

/**
 * QiCore Agent Application - Main Entry Point
 *
 * Mathematical verification agent powered by:
 * - @qi/mcp (reusable MCP and analysis components)
 * - Ollama for mathematical analysis
 * - External MCP servers for memory and filesystem operations
 */

// Re-export library components for convenience
export { MCPClient, MCPServerConfig } from "@qi/mcp/client.js";
export { AnalysisFileManager, AnalysisResult } from "@qi/mcp/tools/file.js";
export { MCPVerificationAgent } from "./mcp-verification-agent.js";
