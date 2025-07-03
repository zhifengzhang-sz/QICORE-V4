#!/usr/bin/env bun

/**
 * QiCore Agent Application - Main Entry Point
 *
 * Autonomous cryptocurrency trading system powered by:
 * - Multi-agent orchestration for market monitoring, analysis, and execution
 * - Real-time crypto data processing with MCP integration
 * - Risk-managed trading with technical analysis
 * - Scalable agent coordination framework
 */

// Legacy mathematical verification agent (preserved for compatibility)
export { MCPVerificationAgent } from "./mcp-verification-agent.ts";

// New autonomous trading agent system
export { CryptoAgentOrchestrator, DEMO_CONFIG } from "./crypto-agent-orchestrator.ts";
export { BaseAgent, AgentTask, AgentMessage, AgentState } from "./agents/base-agent.ts";
export { AgentCoordinator, OrchestrationPattern } from "./agents/agent-coordinator.ts";
export { MarketMonitoringAgent } from "./agents/market-monitoring-agent.ts";
export { TechnicalAnalysisAgent } from "./agents/technical-analysis-agent.ts";
export { ExecutionAgent } from "./agents/execution-agent.ts";

// Demo execution - can be run with: bun run src/index.ts
if (import.meta.main) {
  console.log("🚀 QiCore Agent Application");
  console.log("============================");
  console.log("");
  console.log("Available modes:");
  console.log("1. Legacy: Mathematical verification agent");
  console.log("2. New: Autonomous crypto trading orchestrator");
  console.log("");
  
  const mode = process.argv[2];
  
  if (mode === "legacy") {
    console.log("🔬 Starting legacy mathematical verification agent...");
    const { MCPVerificationAgent } = await import("./mcp-verification-agent.ts");
    
    const logger = {
      info: (msg: string) => console.log(`[Legacy] ℹ️  ${msg}`),
      warn: (msg: string) => console.log(`[Legacy] ⚠️  ${msg}`),
      error: (msg: string) => console.log(`[Legacy] ❌ ${msg}`)
    };
    
    const agent = new MCPVerificationAgent(logger);
    await agent.runVerification();
    
  } else {
    console.log("🤖 Starting autonomous crypto trading orchestrator...");
    const { CryptoAgentOrchestrator, DEMO_CONFIG } = await import("./crypto-agent-orchestrator.ts");
    
    const logger = {
      info: (msg: string) => console.log(`[Trading] ℹ️  ${msg}`),
      warn: (msg: string) => console.log(`[Trading] ⚠️  ${msg}`),
      error: (msg: string) => console.log(`[Trading] ❌ ${msg}`)
    };
    
    const orchestrator = new CryptoAgentOrchestrator(DEMO_CONFIG, logger);
    
    try {
      await orchestrator.start();
      
      logger.info("🎉 Crypto trading orchestrator is running!");
      logger.info("📊 Monitor the logs to see agent coordination in action");
      logger.info("🔄 Market events are being simulated every 15 seconds");
      logger.info("💡 Press Ctrl+C to stop the system gracefully");
      
      // Keep the process running
      process.on('SIGINT', async () => {
        logger.info("🛑 Gracefully shutting down...");
        await orchestrator.stop();
        process.exit(0);
      });
      
    } catch (error) {
      logger.error(`Failed to start crypto orchestrator: ${error}`);
      process.exit(1);
    }
  }
}
