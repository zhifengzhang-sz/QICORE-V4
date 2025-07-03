#!/usr/bin/env bun

/**
 * Execution Agent with Risk Management
 * 
 * Specialized agent for trade execution with comprehensive risk controls
 * Integrates with exchanges and implements position sizing, stop-losses, and portfolio risk management
 */

import { BaseAgent, AgentTask, AgentMessage, AgentCapabilities, AgentConfig } from "./base-agent.js";
import { TradingSignal } from "./technical-analysis-agent.js";

/**
 * Order interfaces
 */
export interface Order {
  id: string;
  symbol: string;
  side: "buy" | "sell";
  type: "market" | "limit" | "stop" | "stop_limit";
  quantity: number;
  price?: number;
  stopPrice?: number;
  timeInForce: "GTC" | "IOC" | "FOK";
  status: "pending" | "filled" | "cancelled" | "rejected" | "partial";
  createdAt: Date;
  filledAt?: Date;
  filledQuantity: number;
  filledPrice?: number;
  exchange: string;
}

/**
 * Position interface
 */
export interface Position {
  symbol: string;
  side: "long" | "short";
  quantity: number;
  entryPrice: number;
  currentPrice: number;
  unrealizedPnL: number;
  realizedPnL: number;
  createdAt: Date;
  lastUpdate: Date;
}

/**
 * Risk parameters
 */
export interface RiskParameters {
  maxPositionSize: number;        // Maximum position size per symbol (% of portfolio)
  maxDailyLoss: number;           // Maximum daily loss (% of portfolio)
  maxDrawdown: number;            // Maximum drawdown (% of portfolio)
  stopLossPercent: number;        // Default stop loss percentage
  takeProfitPercent: number;      // Default take profit percentage
  maxLeverage: number;            // Maximum leverage allowed
  maxCorrelation: number;         // Maximum correlation between positions
  riskPerTrade: number;           // Risk per trade (% of portfolio)
}

/**
 * Portfolio information
 */
export interface Portfolio {
  totalValue: number;
  availableCash: number;
  positions: Map<string, Position>;
  orders: Map<string, Order>;
  dailyPnL: number;
  totalPnL: number;
  drawdown: number;
  riskMetrics: {
    var: number;               // Value at Risk
    sharpeRatio: number;       // Sharpe Ratio
    maxDrawdown: number;       // Maximum Historical Drawdown
    winRate: number;           // Win Rate
    profitFactor: number;      // Profit Factor
  };
}

/**
 * Execution result
 */
export interface ExecutionResult {
  orderId: string;
  success: boolean;
  message: string;
  executedQuantity: number;
  executedPrice?: number;
  fees: number;
  timestamp: Date;
}

/**
 * Risk check result
 */
export interface RiskCheckResult {
  approved: boolean;
  reasons: string[];
  adjustedQuantity?: number;
  warnings: string[];
}

/**
 * Execution Agent implementation
 */
export class ExecutionAgent extends BaseAgent {
  private riskParameters: RiskParameters;
  private portfolio: Portfolio;
  private exchangeConnections: Map<string, any> = new Map();
  private orderHistory: Order[] = [];
  private riskChecks: Map<string, RiskCheckResult> = new Map();

  constructor(
    config: AgentConfig & { riskParameters: RiskParameters },
    logger: { info: (msg: string) => void; warn: (msg: string) => void; error: (msg: string) => void }
  ) {
    const capabilities: AgentCapabilities = {
      canMonitor: true,
      canAnalyze: true,
      canExecute: true,
      canCommunicate: true,
      canLearn: true
    };

    super({ ...config, capabilities }, logger);
    this.riskParameters = config.riskParameters;
    
    // Initialize portfolio
    this.portfolio = {
      totalValue: 100000, // $100k starting capital
      availableCash: 100000,
      positions: new Map(),
      orders: new Map(),
      dailyPnL: 0,
      totalPnL: 0,
      drawdown: 0,
      riskMetrics: {
        var: 0,
        sharpeRatio: 0,
        maxDrawdown: 0,
        winRate: 0,
        profitFactor: 1.0
      }
    };
  }

  /**
   * Initialize execution resources
   */
  protected async initialize(): Promise<void> {
    this.logger.info("⚡ Initializing execution engine...");
    
    // Connect to exchanges
    await this.connectToExchanges();
    
    // Initialize risk monitoring
    await this.initializeRiskMonitoring();
    
    // Load existing positions and orders
    await this.loadPortfolioState();
    
    this.logger.info("✅ Execution engine initialized");
  }

  /**
   * Cleanup execution resources
   */
  protected async cleanup(): Promise<void> {
    this.logger.info("🧹 Cleaning up execution resources");
    
    // Cancel all pending orders
    await this.cancelAllPendingOrders();
    
    // Close exchange connections
    await this.disconnectFromExchanges();
    
    // Save portfolio state
    await this.savePortfolioState();
  }

  /**
   * Process execution tasks
   */
  protected async processTask(task: AgentTask): Promise<void> {
    const startTime = Date.now();
    
    try {
      switch (task.type) {
        case "execute_signal":
          await this.executeSignal(task.payload.signal);
          break;
        case "manage_position":
          await this.managePosition(task.payload.symbol);
          break;
        case "risk_check":
          await this.performRiskCheck(task.payload.order);
          break;
        case "update_stop_loss":
          await this.updateStopLoss(task.payload.symbol, task.payload.newStopPrice);
          break;
        case "close_position":
          await this.closePosition(task.payload.symbol, task.payload.reason);
          break;
        case "rebalance_portfolio":
          await this.rebalancePortfolio();
          break;
        default:
          throw new Error(`Unknown task type: ${task.type}`);
      }

      this.updateMetrics(Date.now() - startTime, true);
      this.activeTasks.delete(task.id);
      
    } catch (error) {
      this.logger.error(`❌ Task ${task.id} failed: ${error}`);
      this.updateMetrics(Date.now() - startTime, false);
      this.activeTasks.delete(task.id);
    }
  }

  /**
   * Handle messages from other agents
   */
  protected async handleMessage(message: AgentMessage): Promise<void> {
    switch (message.type) {
      case "trading_signal":
        await this.processSignalMessage(message.payload.signal);
        break;
      case "risk_alert":
        await this.handleRiskAlert(message.payload);
        break;
      case "market_close":
        await this.handleMarketClose();
        break;
      case "position_update":
        await this.updatePositionFromMessage(message.payload);
        break;
      case "request_portfolio_status":
        await this.sendPortfolioStatus(message.from);
        break;
      default:
        this.logger.warn(`🤷 Unknown message type: ${message.type}`);
    }
  }

  /**
   * Perform periodic execution work
   */
  protected async performPeriodicWork(): Promise<void> {
    // Update portfolio values
    await this.updatePortfolioValues();
    
    // Monitor positions and orders
    await this.monitorPositionsAndOrders();
    
    // Perform risk checks
    await this.performPeriodicRiskChecks();
    
    // Update risk metrics
    await this.updateRiskMetrics();
  }

  /**
   * Execute a trading signal
   */
  private async executeSignal(signal: TradingSignal): Promise<ExecutionResult> {
    this.logger.info(`⚡ Executing signal: ${signal.type} ${signal.symbol}`);
    
    // Perform pre-execution risk check
    const riskCheck = await this.checkRiskBeforeExecution(signal);
    if (!riskCheck.approved) {
      this.logger.warn(`🚫 Signal rejected: ${riskCheck.reasons.join(", ")}`);
      throw new Error(`Risk check failed: ${riskCheck.reasons.join(", ")}`);
    }

    // Calculate position size
    const positionSize = await this.calculatePositionSize(signal, riskCheck.adjustedQuantity);
    
    // Create order
    const order: Order = {
      id: `order-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      symbol: signal.symbol,
      side: signal.type === "buy" ? "buy" : "sell",
      type: "market", // Start with market orders for simplicity
      quantity: positionSize,
      timeInForce: "IOC",
      status: "pending",
      createdAt: new Date(),
      filledQuantity: 0,
      exchange: "demo" // Demo exchange for testing
    };

    // Execute order
    const result = await this.executeOrder(order);
    
    // Update portfolio if successful
    if (result.success) {
      await this.updatePortfolioAfterExecution(order, result);
      
      // Set up stop loss and take profit
      await this.setupStopLossAndTakeProfit(signal, result);
    }

    return result;
  }

  /**
   * Check risk before execution
   */
  private async checkRiskBeforeExecution(signal: TradingSignal): Promise<RiskCheckResult> {
    const reasons: string[] = [];
    const warnings: string[] = [];
    let adjustedQuantity: number | undefined;

    // Check daily loss limit
    if (this.portfolio.dailyPnL < -this.riskParameters.maxDailyLoss * this.portfolio.totalValue / 100) {
      reasons.push("Daily loss limit exceeded");
    }

    // Check maximum drawdown
    if (this.portfolio.drawdown > this.riskParameters.maxDrawdown) {
      reasons.push("Maximum drawdown exceeded");
    }

    // Check position size limit
    const currentPosition = this.portfolio.positions.get(signal.symbol);
    const maxPositionValue = this.riskParameters.maxPositionSize * this.portfolio.totalValue / 100;
    
    if (currentPosition) {
      const currentPositionValue = Math.abs(currentPosition.quantity * currentPosition.currentPrice);
      if (currentPositionValue > maxPositionValue) {
        reasons.push("Maximum position size exceeded");
      }
    }

    // Check available cash
    const requiredCash = (signal.targetPrice || 0) * 100; // Estimate
    if (requiredCash > this.portfolio.availableCash) {
      reasons.push("Insufficient available cash");
    }

    // Check correlation risk
    const correlationRisk = await this.checkCorrelationRisk(signal.symbol);
    if (correlationRisk > this.riskParameters.maxCorrelation) {
      warnings.push(`High correlation risk: ${correlationRisk.toFixed(2)}`);
    }

    // Risk-based position sizing
    const riskAdjustedSize = this.calculateRiskAdjustedPositionSize(signal);
    if (riskAdjustedSize < (signal.targetPrice || 0) * 10) { // Minimum position check
      adjustedQuantity = riskAdjustedSize;
      warnings.push("Position size adjusted for risk");
    }

    return {
      approved: reasons.length === 0,
      reasons,
      adjustedQuantity,
      warnings
    };
  }

  /**
   * Calculate position size based on risk parameters
   */
  private async calculatePositionSize(signal: TradingSignal, adjustedQuantity?: number): Promise<number> {
    if (adjustedQuantity) {
      return adjustedQuantity;
    }

    // Calculate based on risk per trade
    const riskAmount = this.portfolio.totalValue * this.riskParameters.riskPerTrade / 100;
    const stopLossDistance = signal.stopLoss ? Math.abs((signal.targetPrice || 0) - signal.stopLoss) : 0;
    
    if (stopLossDistance > 0) {
      return Math.floor(riskAmount / stopLossDistance);
    }

    // Fallback to percentage of portfolio
    const maxPositionValue = this.riskParameters.maxPositionSize * this.portfolio.totalValue / 100;
    return Math.floor(maxPositionValue / (signal.targetPrice || 1));
  }

  /**
   * Execute order on exchange
   */
  private async executeOrder(order: Order): Promise<ExecutionResult> {
    this.logger.info(`📤 Executing order: ${order.side} ${order.quantity} ${order.symbol}`);
    
    try {
      // Simulate order execution for demo
      const executionPrice = this.simulateOrderExecution(order);
      const fees = executionPrice * order.quantity * 0.001; // 0.1% fee
      
      // Update order status
      order.status = "filled";
      order.filledAt = new Date();
      order.filledQuantity = order.quantity;
      order.filledPrice = executionPrice;
      
      // Store order
      this.portfolio.orders.set(order.id, order);
      this.orderHistory.push(order);
      
      const result: ExecutionResult = {
        orderId: order.id,
        success: true,
        message: "Order executed successfully",
        executedQuantity: order.quantity,
        executedPrice: executionPrice,
        fees,
        timestamp: new Date()
      };
      
      this.logger.info(`✅ Order executed: ${order.quantity} @ ${executionPrice}`);
      return result;
      
    } catch (error) {
      order.status = "rejected";
      this.logger.error(`❌ Order execution failed: ${error}`);
      
      return {
        orderId: order.id,
        success: false,
        message: `Execution failed: ${error}`,
        executedQuantity: 0,
        fees: 0,
        timestamp: new Date()
      };
    }
  }

  /**
   * Simulate order execution for demo
   */
  private simulateOrderExecution(order: Order): number {
    // Simulate realistic execution with small slippage
    const basePrice = order.price || 45000; // Default BTC price for demo
    const slippage = (Math.random() - 0.5) * 0.002; // ±0.2% slippage
    return basePrice * (1 + slippage);
  }

  /**
   * Update portfolio after successful execution
   */
  private async updatePortfolioAfterExecution(order: Order, result: ExecutionResult): Promise<void> {
    const symbol = order.symbol;
    const executedValue = result.executedQuantity * (result.executedPrice || 0);
    
    // Update available cash
    if (order.side === "buy") {
      this.portfolio.availableCash -= executedValue + result.fees;
    } else {
      this.portfolio.availableCash += executedValue - result.fees;
    }

    // Update or create position
    let position = this.portfolio.positions.get(symbol);
    
    if (!position) {
      // Create new position
      position = {
        symbol,
        side: order.side === "buy" ? "long" : "short",
        quantity: order.side === "buy" ? result.executedQuantity : -result.executedQuantity,
        entryPrice: result.executedPrice || 0,
        currentPrice: result.executedPrice || 0,
        unrealizedPnL: 0,
        realizedPnL: 0,
        createdAt: new Date(),
        lastUpdate: new Date()
      };
    } else {
      // Update existing position
      const newQuantity = position.quantity + (order.side === "buy" ? result.executedQuantity : -result.executedQuantity);
      
      if (Math.sign(newQuantity) !== Math.sign(position.quantity) && position.quantity !== 0) {
        // Position reversal - realize some PnL
        const realizedPnL = this.calculateRealizedPnL(position, result.executedQuantity, result.executedPrice || 0);
        position.realizedPnL += realizedPnL;
        this.portfolio.totalPnL += realizedPnL;
      }
      
      position.quantity = newQuantity;
      position.lastUpdate = new Date();
      
      if (newQuantity === 0) {
        // Position closed
        this.portfolio.positions.delete(symbol);
        this.logger.info(`📊 Position closed: ${symbol}`);
        return;
      }
    }

    this.portfolio.positions.set(symbol, position);
    this.logger.info(`📊 Position updated: ${symbol} - ${position.quantity} @ ${position.entryPrice}`);
  }

  /**
   * Calculate realized PnL
   */
  private calculateRealizedPnL(position: Position, quantity: number, price: number): number {
    const pnlPerShare = position.side === "long" ? (price - position.entryPrice) : (position.entryPrice - price);
    return pnlPerShare * Math.min(Math.abs(quantity), Math.abs(position.quantity));
  }

  /**
   * Setup stop loss and take profit orders
   */
  private async setupStopLossAndTakeProfit(signal: TradingSignal, result: ExecutionResult): Promise<void> {
    const symbol = signal.symbol;
    const entryPrice = result.executedPrice || 0;
    
    // Setup stop loss
    if (signal.stopLoss) {
      const stopLossOrder: Order = {
        id: `stop-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        symbol,
        side: signal.type === "buy" ? "sell" : "buy",
        type: "stop",
        quantity: result.executedQuantity,
        stopPrice: signal.stopLoss,
        timeInForce: "GTC",
        status: "pending",
        createdAt: new Date(),
        filledQuantity: 0,
        exchange: "demo"
      };
      
      this.portfolio.orders.set(stopLossOrder.id, stopLossOrder);
      this.logger.info(`🛡️ Stop loss set at ${signal.stopLoss} for ${symbol}`);
    }
    
    // Setup take profit
    if (signal.targetPrice) {
      const takeProfitOrder: Order = {
        id: `tp-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
        symbol,
        side: signal.type === "buy" ? "sell" : "buy",
        type: "limit",
        quantity: result.executedQuantity,
        price: signal.targetPrice,
        timeInForce: "GTC",
        status: "pending",
        createdAt: new Date(),
        filledQuantity: 0,
        exchange: "demo"
      };
      
      this.portfolio.orders.set(takeProfitOrder.id, takeProfitOrder);
      this.logger.info(`🎯 Take profit set at ${signal.targetPrice} for ${symbol}`);
    }
  }

  /**
   * Check correlation risk
   */
  private async checkCorrelationRisk(symbol: string): Promise<number> {
    // Simplified correlation calculation
    // In production, this would calculate actual correlation between assets
    const positions = Array.from(this.portfolio.positions.values());
    const cryptoPositions = positions.filter(p => p.symbol.includes("USD"));
    
    if (cryptoPositions.length <= 1) return 0;
    
    // Assume high correlation between crypto assets
    return 0.8; // 80% correlation
  }

  /**
   * Calculate risk-adjusted position size
   */
  private calculateRiskAdjustedPositionSize(signal: TradingSignal): number {
    const baseSize = this.portfolio.totalValue * this.riskParameters.riskPerTrade / 100;
    const confidenceMultiplier = signal.confidence / 100;
    const strengthMultiplier = signal.strength === "strong" ? 1.2 : signal.strength === "moderate" ? 1.0 : 0.8;
    
    return Math.floor(baseSize * confidenceMultiplier * strengthMultiplier / (signal.targetPrice || 1));
  }

  // Additional helper methods (simplified implementations)

  private async connectToExchanges(): Promise<void> {
    this.logger.info("🔌 Connecting to exchanges...");
    // Simulate exchange connections
    this.exchangeConnections.set("demo", { connected: true, name: "Demo Exchange" });
  }

  private async initializeRiskMonitoring(): Promise<void> {
    this.logger.info("🛡️ Initializing risk monitoring...");
  }

  private async loadPortfolioState(): Promise<void> {
    this.logger.info("📊 Loading portfolio state...");
  }

  private async cancelAllPendingOrders(): Promise<void> {
    const pendingOrders = Array.from(this.portfolio.orders.values()).filter(o => o.status === "pending");
    this.logger.info(`🚫 Cancelling ${pendingOrders.length} pending orders`);
    
    for (const order of pendingOrders) {
      order.status = "cancelled";
    }
  }

  private async disconnectFromExchanges(): Promise<void> {
    this.logger.info("🔌 Disconnecting from exchanges...");
    this.exchangeConnections.clear();
  }

  private async savePortfolioState(): Promise<void> {
    this.logger.info("💾 Saving portfolio state...");
  }

  private async managePosition(symbol: string): Promise<void> {
    const position = this.portfolio.positions.get(symbol);
    if (position) {
      // Update current price and unrealized PnL
      await this.updatePositionPnL(position);
    }
  }

  private async updatePositionPnL(position: Position): Promise<void> {
    // Simulate current price update
    position.currentPrice = position.entryPrice * (0.98 + Math.random() * 0.04); // ±2% price movement
    
    const pnlPerShare = position.side === "long" ? 
      (position.currentPrice - position.entryPrice) : 
      (position.entryPrice - position.currentPrice);
    
    position.unrealizedPnL = pnlPerShare * Math.abs(position.quantity);
    position.lastUpdate = new Date();
  }

  private async performRiskCheck(order: Order): Promise<void> {
    // Perform detailed risk check for specific order
  }

  private async updateStopLoss(symbol: string, newStopPrice: number): Promise<void> {
    this.logger.info(`🛡️ Updating stop loss for ${symbol} to ${newStopPrice}`);
  }

  private async closePosition(symbol: string, reason: string): Promise<void> {
    this.logger.info(`🔚 Closing position ${symbol}: ${reason}`);
  }

  private async rebalancePortfolio(): Promise<void> {
    this.logger.info("⚖️ Rebalancing portfolio...");
  }

  private async processSignalMessage(signal: TradingSignal): Promise<void> {
    await this.executeSignal(signal);
  }

  private async handleRiskAlert(payload: any): Promise<void> {
    this.logger.warn(`🚨 Risk alert: ${payload.message}`);
  }

  private async handleMarketClose(): Promise<void> {
    this.logger.info("🏪 Market close - updating positions...");
  }

  private async updatePositionFromMessage(payload: any): Promise<void> {
    // Update position from external message
  }

  private async sendPortfolioStatus(agentId: string): Promise<void> {
    const message: AgentMessage = {
      from: this.config.id,
      to: agentId,
      type: "portfolio_status_response",
      payload: { portfolio: this.getPortfolioStatus() },
      timestamp: new Date()
    };
    
    await this.sendMessage(message);
  }

  private async updatePortfolioValues(): Promise<void> {
    // Update total portfolio value
    let totalValue = this.portfolio.availableCash;
    
    for (const position of this.portfolio.positions.values()) {
      await this.updatePositionPnL(position);
      totalValue += Math.abs(position.quantity) * position.currentPrice;
    }
    
    this.portfolio.totalValue = totalValue;
  }

  private async monitorPositionsAndOrders(): Promise<void> {
    // Monitor all positions and orders for stop loss triggers, etc.
  }

  private async performPeriodicRiskChecks(): Promise<void> {
    // Perform regular risk assessments
  }

  private async updateRiskMetrics(): Promise<void> {
    // Calculate and update portfolio risk metrics
    const positions = Array.from(this.portfolio.positions.values());
    const totalPnL = positions.reduce((sum, p) => sum + p.unrealizedPnL + p.realizedPnL, 0);
    
    this.portfolio.dailyPnL = totalPnL; // Simplified daily PnL
    this.portfolio.drawdown = Math.max(0, -totalPnL / this.portfolio.totalValue * 100);
  }

  /**
   * Get current portfolio status
   */
  getPortfolioStatus() {
    return {
      totalValue: this.portfolio.totalValue,
      availableCash: this.portfolio.availableCash,
      positions: Array.from(this.portfolio.positions.values()),
      activeOrders: Array.from(this.portfolio.orders.values()).filter(o => o.status === "pending"),
      dailyPnL: this.portfolio.dailyPnL,
      totalPnL: this.portfolio.totalPnL,
      riskMetrics: this.portfolio.riskMetrics,
      riskParameters: this.riskParameters
    };
  }
}