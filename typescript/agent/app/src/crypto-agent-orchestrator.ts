#!/usr/bin/env bun

/**
 * Crypto Agent Orchestrator - Main Entry Point
 * 
 * Coordinates multiple specialized agents for autonomous cryptocurrency trading
 * Integrates market monitoring, technical analysis, and execution with risk management
 */

import { AgentCoordinator, AgentWorkflow, OrchestrationPattern } from "./agents/agent-coordinator.js";
import { MarketMonitoringAgent } from "./agents/market-monitoring-agent.js";
import { TechnicalAnalysisAgent } from "./agents/technical-analysis-agent.js";
import { ExecutionAgent } from "./agents/execution-agent.js";
import { AgentTask, AgentMessage } from "./agents/base-agent.js";

/**
 * Orchestrator configuration
 */
export interface OrchestratorConfig {
  portfolio: {
    initialCapital: number;
    riskLevel: "conservative" | "moderate" | "aggressive";
  };
  monitoring: {
    symbols: string[];
    exchanges: string[];
    updateInterval: number;
  };
  analysis: {
    timeframes: string[];
    indicators: string[];
    minConfidence: number;
  };
  execution: {
    maxPositionSize: number;
    stopLossPercent: number;
    takeProfitPercent: number;
  };
  mcpServers?: any[];
}

/**
 * Trading session information
 */
export interface TradingSession {
  id: string;
  startTime: Date;
  endTime?: Date;
  status: "active" | "paused" | "stopped";
  totalTrades: number;
  successfulTrades: number;
  totalPnL: number;
  agentPerformance: Record<string, any>;
}

/**
 * Crypto Agent Orchestrator - Main class
 */
export class CryptoAgentOrchestrator {
  private coordinator: AgentCoordinator;
  private marketAgent!: MarketMonitoringAgent;
  private analysisAgent!: TechnicalAnalysisAgent;
  private executionAgent!: ExecutionAgent;
  private currentSession?: TradingSession;
  private workflows: Map<string, AgentWorkflow> = new Map();
  
  constructor(
    private config: OrchestratorConfig,
    private logger: {
      info: (msg: string) => void;
      warn: (msg: string) => void;
      error: (msg: string) => void;
    }
  ) {
    this.coordinator = new AgentCoordinator(logger);
    this.initializeAgents();
    this.setupWorkflows();
  }

  /**
   * Start the orchestrated trading system
   */
  async start(): Promise<void> {
    this.logger.info("🚀 Starting Crypto Agent Orchestrator...");
    
    try {
      // Start all agents
      await this.startAllAgents();
      
      // Register agents with coordinator
      this.registerAgentsWithCoordinator();
      
      // Start trading session
      await this.startTradingSession();
      
      // Begin orchestrated workflows
      await this.startMainWorkflows();
      
      this.logger.info("✅ Crypto Agent Orchestrator started successfully");
      
    } catch (error) {
      this.logger.error(`❌ Failed to start orchestrator: ${error}`);
      throw error;
    }
  }

  /**
   * Stop the orchestrated trading system
   */
  async stop(): Promise<void> {
    this.logger.info("🛑 Stopping Crypto Agent Orchestrator...");
    
    try {
      // End trading session
      await this.endTradingSession();
      
      // Stop all agents
      await this.stopAllAgents();
      
      this.logger.info("✅ Crypto Agent Orchestrator stopped");
      
    } catch (error) {
      this.logger.error(`❌ Error stopping orchestrator: ${error}`);
      throw error;
    }
  }

  /**
   * Get system status and performance metrics
   */
  getSystemStatus() {
    return {
      orchestrator: {
        status: this.currentSession?.status || "stopped",
        uptime: this.currentSession ? Date.now() - this.currentSession.startTime.getTime() : 0
      },
      coordinator: this.coordinator.getStatus(),
      agents: {
        market: this.marketAgent.getStatus(),
        analysis: this.analysisAgent.getStatus(),
        execution: this.executionAgent.getStatus()
      },
      session: this.currentSession,
      workflows: Array.from(this.workflows.keys())
    };
  }

  /**
   * Process a market event (price change, news, etc.)
   */
  async processMarketEvent(event: { type: string; symbol: string; data: any }): Promise<void> {
    this.logger.info(`📈 Processing market event: ${event.type} for ${event.symbol}`);
    
    try {
      // Route event to appropriate workflow
      switch (event.type) {
        case "price_update":
          await this.processTradeSignalWorkflow(event.symbol, event.data);
          break;
        case "volume_spike":
          await this.processVolumeAnalysisWorkflow(event.symbol, event.data);
          break;
        case "news_alert":
          await this.processSentimentAnalysisWorkflow(event.symbol, event.data);
          break;
        case "pattern_detected":
          await this.processPatternTradingWorkflow(event.symbol, event.data);
          break;
        default:
          this.logger.warn(`🤷 Unknown market event type: ${event.type}`);
      }
    } catch (error) {
      this.logger.error(`❌ Error processing market event: ${error}`);
    }
  }

  /**
   * Initialize all specialized agents
   */
  private initializeAgents(): void {
    this.logger.info("🤖 Initializing specialized agents...");
    
    // Market Monitoring Agent
    this.marketAgent = new MarketMonitoringAgent(
      {
        id: "market-monitor",
        type: "market_monitoring",
        capabilities: {
          canMonitor: true,
          canAnalyze: true,
          canExecute: false,
          canCommunicate: true,
          canLearn: false
        },
        monitoringConfig: {
          symbols: this.config.monitoring.symbols,
          exchanges: this.config.monitoring.exchanges,
          alertThresholds: {
            priceChangePercent: 5.0,
            volumeMultiplier: 3.0,
            rsiOverbought: 70,
            rsiOversold: 30
          },
          updateInterval: this.config.monitoring.updateInterval
        },
        mcpServers: this.config.mcpServers
      },
      this.logger
    );

    // Technical Analysis Agent
    this.analysisAgent = new TechnicalAnalysisAgent(
      {
        id: "technical-analyst",
        type: "technical_analysis",
        capabilities: {
          canMonitor: false,
          canAnalyze: true,
          canExecute: false,
          canCommunicate: true,
          canLearn: true
        },
        analysisConfig: {
          timeframes: this.config.analysis.timeframes,
          indicators: this.config.analysis.indicators,
          patternRecognition: true,
          signalGeneration: true,
          minConfidence: this.config.analysis.minConfidence,
          riskLevel: this.config.portfolio.riskLevel
        },
        mcpServers: this.config.mcpServers
      },
      this.logger
    );

    // Execution Agent
    this.executionAgent = new ExecutionAgent(
      {
        id: "execution-engine",
        type: "execution",
        capabilities: {
          canMonitor: true,
          canAnalyze: true,
          canExecute: true,
          canCommunicate: true,
          canLearn: true
        },
        riskParameters: {
          maxPositionSize: this.config.execution.maxPositionSize,
          maxDailyLoss: 5.0, // 5% max daily loss
          maxDrawdown: 15.0, // 15% max drawdown
          stopLossPercent: this.config.execution.stopLossPercent,
          takeProfitPercent: this.config.execution.takeProfitPercent,
          maxLeverage: 1.0, // No leverage for safety
          maxCorrelation: 0.7,
          riskPerTrade: 2.0 // 2% risk per trade
        },
        mcpServers: this.config.mcpServers
      },
      this.logger
    );
  }

  /**
   * Setup orchestration workflows
   */
  private setupWorkflows(): void {
    this.logger.info("📋 Setting up orchestration workflows...");
    
    // Trade Signal Generation Workflow
    const tradeSignalWorkflow: AgentWorkflow = {
      id: "trade-signal-generation",
      name: "Trade Signal Generation",
      pattern: OrchestrationPattern.PIPELINE,
      agents: ["market-monitor", "technical-analyst", "execution-engine"],
      steps: [
        {
          id: "monitor-market",
          agentType: "market_monitoring",
          taskType: "monitor_symbol",
          timeout: 5000
        },
        {
          id: "analyze-data",
          agentType: "technical_analysis",
          taskType: "analyze_symbol",
          dependencies: ["monitor-market"],
          timeout: 10000
        },
        {
          id: "generate-signals",
          agentType: "technical_analysis",
          taskType: "generate_signals",
          dependencies: ["analyze-data"],
          timeout: 5000
        },
        {
          id: "execute-signals",
          agentType: "execution",
          taskType: "execute_signal",
          dependencies: ["generate-signals"],
          timeout: 15000
        }
      ],
      timeout: 60000
    };

    // Risk Management Workflow
    const riskManagementWorkflow: AgentWorkflow = {
      id: "risk-management",
      name: "Portfolio Risk Management",
      pattern: OrchestrationPattern.COLLABORATIVE,
      agents: ["technical-analyst", "execution-engine"],
      steps: [
        {
          id: "assess-portfolio-risk",
          agentType: "execution",
          taskType: "risk_check",
          timeout: 5000
        },
        {
          id: "adjust-positions",
          agentType: "execution",
          taskType: "manage_position",
          dependencies: ["assess-portfolio-risk"],
          timeout: 10000
        },
        {
          id: "update-stop-losses",
          agentType: "execution",
          taskType: "update_stop_loss",
          dependencies: ["assess-portfolio-risk"],
          timeout: 5000
        }
      ],
      timeout: 30000
    };

    // Market Anomaly Detection Workflow
    const anomalyDetectionWorkflow: AgentWorkflow = {
      id: "anomaly-detection",
      name: "Market Anomaly Detection",
      pattern: OrchestrationPattern.SWARM,
      agents: ["market-monitor", "technical-analyst"],
      steps: [
        {
          id: "detect-volume-anomalies",
          agentType: "market_monitoring",
          taskType: "check_alerts",
          timeout: 3000
        },
        {
          id: "detect-price-patterns",
          agentType: "technical_analysis",
          taskType: "pattern_recognition",
          timeout: 8000
        },
        {
          id: "correlate-anomalies",
          agentType: "technical_analysis",
          taskType: "analyze_symbol",
          dependencies: ["detect-volume-anomalies", "detect-price-patterns"],
          timeout: 5000
        }
      ],
      timeout: 25000
    };

    // Store workflows
    this.workflows.set(tradeSignalWorkflow.id, tradeSignalWorkflow);
    this.workflows.set(riskManagementWorkflow.id, riskManagementWorkflow);
    this.workflows.set(anomalyDetectionWorkflow.id, anomalyDetectionWorkflow);
    
    // Register workflows with coordinator
    for (const workflow of this.workflows.values()) {
      this.coordinator.registerWorkflow(workflow);
    }
  }

  /**
   * Start all agents
   */
  private async startAllAgents(): Promise<void> {
    this.logger.info("▶️ Starting all agents...");
    
    await Promise.all([
      this.marketAgent.start(),
      this.analysisAgent.start(),
      this.executionAgent.start()
    ]);
    
    this.logger.info("✅ All agents started");
  }

  /**
   * Stop all agents
   */
  private async stopAllAgents(): Promise<void> {
    this.logger.info("⏹️ Stopping all agents...");
    
    await Promise.all([
      this.marketAgent.stop(),
      this.analysisAgent.stop(),
      this.executionAgent.stop()
    ]);
    
    this.logger.info("✅ All agents stopped");
  }

  /**
   * Register agents with coordinator
   */
  private registerAgentsWithCoordinator(): void {
    this.coordinator.registerAgent(this.marketAgent);
    this.coordinator.registerAgent(this.analysisAgent);
    this.coordinator.registerAgent(this.executionAgent);
  }

  /**
   * Start trading session
   */
  private async startTradingSession(): Promise<void> {
    this.currentSession = {
      id: `session-${Date.now()}`,
      startTime: new Date(),
      status: "active",
      totalTrades: 0,
      successfulTrades: 0,
      totalPnL: 0,
      agentPerformance: {}
    };
    
    this.logger.info(`📊 Trading session started: ${this.currentSession.id}`);
  }

  /**
   * End trading session
   */
  private async endTradingSession(): Promise<void> {
    if (this.currentSession) {
      this.currentSession.endTime = new Date();
      this.currentSession.status = "stopped";
      
      const duration = this.currentSession.endTime.getTime() - this.currentSession.startTime.getTime();
      this.logger.info(`📊 Trading session ended: ${Math.round(duration / 1000)}s duration`);
    }
  }

  /**
   * Start main orchestration workflows
   */
  private async startMainWorkflows(): Promise<void> {
    this.logger.info("🔄 Starting main workflows...");
    
    // Start periodic risk management
    setInterval(async () => {
      try {
        await this.coordinator.executeWorkflow("risk-management", {
          timestamp: Date.now(),
          trigger: "periodic"
        });
      } catch (error) {
        this.logger.error(`❌ Risk management workflow failed: ${error}`);
      }
    }, 30000); // Every 30 seconds

    // Start anomaly detection
    setInterval(async () => {
      try {
        await this.coordinator.executeWorkflow("anomaly-detection", {
          timestamp: Date.now(),
          symbols: this.config.monitoring.symbols
        });
      } catch (error) {
        this.logger.error(`❌ Anomaly detection workflow failed: ${error}`);
      }
    }, 60000); // Every minute
  }

  /**
   * Process trade signal workflow
   */
  private async processTradeSignalWorkflow(symbol: string, data: any): Promise<void> {
    try {
      await this.coordinator.executeWorkflow("trade-signal-generation", {
        symbol,
        marketData: data,
        timestamp: Date.now()
      });
      
      if (this.currentSession) {
        this.currentSession.totalTrades++;
      }
    } catch (error) {
      this.logger.error(`❌ Trade signal workflow failed for ${symbol}: ${error}`);
    }
  }

  /**
   * Process volume analysis workflow
   */
  private async processVolumeAnalysisWorkflow(symbol: string, data: any): Promise<void> {
    // Send volume spike alert to analysis agent
    const message: AgentMessage = {
      from: "orchestrator",
      to: "technical-analyst",
      type: "volume_alert",
      payload: { symbol, data },
      timestamp: new Date()
    };
    
    await this.analysisAgent.sendMessage(message);
  }

  /**
   * Process sentiment analysis workflow
   */
  private async processSentimentAnalysisWorkflow(symbol: string, data: any): Promise<void> {
    // In production, this would integrate with news sentiment analysis
    this.logger.info(`📰 News alert for ${symbol}: ${data.headline}`);
  }

  /**
   * Process pattern trading workflow
   */
  private async processPatternTradingWorkflow(symbol: string, data: any): Promise<void> {
    // Send pattern detection to execution for immediate action
    const message: AgentMessage = {
      from: "orchestrator",
      to: "execution-engine",
      type: "pattern_alert",
      payload: { symbol, pattern: data },
      timestamp: new Date()
    };
    
    await this.executionAgent.sendMessage(message);
  }
}

/**
 * Demo configuration for testing
 */
export const DEMO_CONFIG: OrchestratorConfig = {
  portfolio: {
    initialCapital: 100000,
    riskLevel: "moderate"
  },
  monitoring: {
    symbols: ["BTCUSD", "ETHUSD", "ADAUSD"],
    exchanges: ["binance", "coinbase"],
    updateInterval: 5000
  },
  analysis: {
    timeframes: ["5m", "15m", "1h", "4h"],
    indicators: ["RSI", "MACD", "BollingerBands", "EMA"],
    minConfidence: 65
  },
  execution: {
    maxPositionSize: 25, // 25% max position size
    stopLossPercent: 3,  // 3% stop loss
    takeProfitPercent: 8 // 8% take profit
  }
};

// Logger implementation
const logger = {
  info: (msg: string) => console.log(`[Orchestrator] ℹ️  ${msg}`),
  warn: (msg: string) => console.log(`[Orchestrator] ⚠️  ${msg}`),
  error: (msg: string) => console.log(`[Orchestrator] ❌ ${msg}`)
};

// Main execution
if (import.meta.main) {
  const orchestrator = new CryptoAgentOrchestrator(DEMO_CONFIG, logger);
  
  // Start orchestrator
  orchestrator.start().then(() => {
    logger.info("🎉 Crypto Agent Orchestrator is running!");
    
    // Simulate market events for testing
    setInterval(() => {
      const symbols = DEMO_CONFIG.monitoring.symbols;
      const randomSymbol = symbols[Math.floor(Math.random() * symbols.length)];
      
      orchestrator.processMarketEvent({
        type: "price_update",
        symbol: randomSymbol,
        data: {
          price: 45000 + (Math.random() - 0.5) * 2000,
          volume: Math.random() * 1000,
          timestamp: Date.now()
        }
      });
    }, 15000); // Every 15 seconds
    
  }).catch(error => {
    logger.error(`Failed to start orchestrator: ${error}`);
    process.exit(1);
  });
  
  // Graceful shutdown
  process.on('SIGINT', async () => {
    logger.info("🛑 Shutting down orchestrator...");
    await orchestrator.stop();
    process.exit(0);
  });
}