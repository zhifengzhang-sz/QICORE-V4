#!/usr/bin/env bun

/**
 * Market Monitoring Agent
 * 
 * Specialized agent for real-time cryptocurrency market monitoring
 * Integrates with crypto data platform (CryptoCompare API + TimescaleDB)
 */

import { BaseAgent, AgentTask, AgentMessage, AgentCapabilities, AgentConfig } from "./base-agent.js";

/**
 * Market data interfaces
 */
export interface OHLCVData {
  symbol: string;
  exchange: string;
  timestamp: number;
  open: number;
  high: number;
  low: number;
  close: number;
  volume: number;
  source: string;
}

export interface MarketAlert {
  id: string;
  type: "price_breakout" | "volume_spike" | "trend_change" | "anomaly";
  symbol: string;
  severity: "low" | "medium" | "high" | "critical";
  message: string;
  data: any;
  timestamp: Date;
}

export interface MonitoringConfig {
  symbols: string[];
  exchanges: string[];
  alertThresholds: {
    priceChangePercent: number;
    volumeMultiplier: number;
    rsiOverbought: number;
    rsiOversold: number;
  };
  updateInterval: number;
}

/**
 * Market Monitoring Agent implementation
 */
export class MarketMonitoringAgent extends BaseAgent {
  private monitoringConfig: MonitoringConfig;
  private priceHistory: Map<string, OHLCVData[]> = new Map();
  private activeAlerts: Map<string, MarketAlert> = new Map();
  private websocketConnections: Map<string, WebSocket> = new Map();

  constructor(
    config: AgentConfig & { monitoringConfig: MonitoringConfig },
    logger: { info: (msg: string) => void; warn: (msg: string) => void; error: (msg: string) => void }
  ) {
    const capabilities: AgentCapabilities = {
      canMonitor: true,
      canAnalyze: true,
      canExecute: false,
      canCommunicate: true,
      canLearn: false
    };

    super({ ...config, capabilities }, logger);
    this.monitoringConfig = config.monitoringConfig;
  }

  /**
   * Initialize market monitoring connections
   */
  protected async initialize(): Promise<void> {
    this.logger.info(`📊 Initializing market monitoring for ${this.monitoringConfig.symbols.length} symbols`);
    
    // Initialize price history maps
    for (const symbol of this.monitoringConfig.symbols) {
      this.priceHistory.set(symbol, []);
    }

    // Connect to real-time market data streams
    await this.connectToMarketDataStreams();
    
    // Load historical data for baseline calculations
    await this.loadHistoricalData();
    
    this.logger.info("✅ Market monitoring initialized");
  }

  /**
   * Cleanup monitoring resources
   */
  protected async cleanup(): Promise<void> {
    this.logger.info("🧹 Cleaning up market monitoring resources");
    
    // Close websocket connections
    for (const [symbol, ws] of this.websocketConnections) {
      ws.close();
      this.logger.info(`📡 Closed websocket for ${symbol}`);
    }
    
    this.websocketConnections.clear();
    this.priceHistory.clear();
    this.activeAlerts.clear();
  }

  /**
   * Process monitoring tasks
   */
  protected async processTask(task: AgentTask): Promise<void> {
    const startTime = Date.now();
    
    try {
      switch (task.type) {
        case "monitor_symbol":
          await this.monitorSymbol(task.payload.symbol);
          break;
        case "check_alerts":
          await this.checkAlerts();
          break;
        case "update_thresholds":
          await this.updateAlertThresholds(task.payload.thresholds);
          break;
        case "generate_market_report":
          await this.generateMarketReport();
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
      case "subscribe_alerts":
        await this.subscribeToAlerts(message.from, message.payload.alertTypes);
        break;
      case "request_market_data":
        await this.sendMarketData(message.from, message.payload.symbol);
        break;
      case "update_monitoring_config":
        await this.updateMonitoringConfig(message.payload.config);
        break;
      default:
        this.logger.warn(`🤷 Unknown message type: ${message.type}`);
    }
  }

  /**
   * Perform periodic monitoring work
   */
  protected async performPeriodicWork(): Promise<void> {
    // Check for market alerts
    await this.checkAlerts();
    
    // Update performance metrics
    await this.updatePerformanceMetrics();
    
    // Cleanup old data
    await this.cleanupOldData();
  }

  /**
   * Connect to real-time market data streams
   */
  private async connectToMarketDataStreams(): Promise<void> {
    this.logger.info("🔌 Connecting to market data streams...");
    
    for (const symbol of this.monitoringConfig.symbols) {
      try {
        // Simulate CryptoCompare WebSocket connection
        // In production, this would connect to actual CryptoCompare API
        const ws = await this.createWebSocketConnection(symbol);
        this.websocketConnections.set(symbol, ws);
        
        this.logger.info(`📡 Connected to ${symbol} data stream`);
      } catch (error) {
        this.logger.error(`❌ Failed to connect to ${symbol}: ${error}`);
      }
    }
  }

  /**
   * Create WebSocket connection for a symbol
   */
  private async createWebSocketConnection(symbol: string): Promise<WebSocket> {
    // For demo purposes, simulate market data
    const mockWs = {
      close: () => {},
      onmessage: null as ((event: any) => void) | null,
      send: () => {},
      readyState: 1,
      url: `wss://demo/${symbol}`,
      protocol: "",
      extensions: "",
      bufferedAmount: 0,
      binaryType: "blob" as BinaryType,
      onopen: null,
      onclose: null,
      onerror: null,
      addEventListener: () => {},
      removeEventListener: () => {},
      dispatchEvent: () => false,
      CONNECTING: 0,
      OPEN: 1,
      CLOSING: 2,
      CLOSED: 3
    } as WebSocket;

    // Simulate real-time price updates
    setInterval(() => {
      const mockData = this.generateMockOHLCVData(symbol);
      this.processPriceUpdate(mockData);
    }, this.monitoringConfig.updateInterval);

    return mockWs;
  }

  /**
   * Load historical data for baseline calculations
   */
  private async loadHistoricalData(): Promise<void> {
    this.logger.info("📚 Loading historical market data...");
    
    for (const symbol of this.monitoringConfig.symbols) {
      try {
        // In production, load from TimescaleDB
        const historicalData = await this.fetchHistoricalData(symbol);
        this.priceHistory.set(symbol, historicalData);
        
        this.logger.info(`📈 Loaded ${historicalData.length} historical records for ${symbol}`);
      } catch (error) {
        this.logger.error(`❌ Failed to load historical data for ${symbol}: ${error}`);
      }
    }
  }

  /**
   * Process incoming price updates
   */
  private processPriceUpdate(data: OHLCVData): void {
    const history = this.priceHistory.get(data.symbol) || [];
    
    // Add new data point
    history.push(data);
    
    // Keep only last 1000 data points in memory
    if (history.length > 1000) {
      history.shift();
    }
    
    this.priceHistory.set(data.symbol, history);
    
    // Check for alerts
    this.checkPriceAlerts(data, history);
  }

  /**
   * Check for price-based alerts
   */
  private checkPriceAlerts(current: OHLCVData, history: OHLCVData[]): void {
    if (history.length < 2) return;
    
    const previous = history[history.length - 2];
    const priceChangePercent = ((current.close - previous.close) / previous.close) * 100;
    
    // Price breakout alert
    if (Math.abs(priceChangePercent) > this.monitoringConfig.alertThresholds.priceChangePercent) {
      this.createAlert({
        type: "price_breakout",
        symbol: current.symbol,
        severity: Math.abs(priceChangePercent) > 10 ? "critical" : "high",
        message: `Price ${priceChangePercent > 0 ? "surge" : "drop"} of ${priceChangePercent.toFixed(2)}%`,
        data: { current, previous, changePercent: priceChangePercent }
      });
    }
    
    // Volume spike alert
    if (history.length >= 20) {
      const avgVolume = history.slice(-20).reduce((sum, d) => sum + d.volume, 0) / 20;
      const volumeMultiplier = current.volume / avgVolume;
      
      if (volumeMultiplier > this.monitoringConfig.alertThresholds.volumeMultiplier) {
        this.createAlert({
          type: "volume_spike",
          symbol: current.symbol,
          severity: volumeMultiplier > 5 ? "critical" : "medium",
          message: `Volume spike: ${volumeMultiplier.toFixed(1)}x average`,
          data: { current, avgVolume, multiplier: volumeMultiplier }
        });
      }
    }
  }

  /**
   * Create and dispatch an alert
   */
  private createAlert(alertData: Omit<MarketAlert, "id" | "timestamp">): void {
    const alert: MarketAlert = {
      id: `alert-${Date.now()}-${Math.random().toString(36).substr(2, 9)}`,
      timestamp: new Date(),
      ...alertData
    };
    
    this.activeAlerts.set(alert.id, alert);
    this.logger.info(`🚨 Alert: ${alert.type} for ${alert.symbol} - ${alert.message}`);
    
    // In production, send to alert subscribers
    this.dispatchAlert(alert);
  }

  /**
   * Dispatch alert to subscribers
   */
  private async dispatchAlert(alert: MarketAlert): Promise<void> {
    // Send alert via MCP if memory server is connected
    if (this.mcpClient.isConnected("memory")) {
      await this.mcpClient.storeMemory(`alert-${alert.id}`, JSON.stringify(alert));
    }
    
    // Store in TimescaleDB via MCP filesystem
    if (this.mcpClient.isConnected("filesystem")) {
      const alertFile = `alerts/alert-${alert.id}.json`;
      await this.mcpClient.saveFile(alertFile, JSON.stringify(alert, null, 2));
    }
  }

  /**
   * Generate mock OHLCV data for demonstration
   */
  private generateMockOHLCVData(symbol: string): OHLCVData {
    const basePrice = symbol === "BTCUSD" ? 45000 : symbol === "ETHUSD" ? 3000 : 100;
    const volatility = 0.02; // 2% volatility
    
    const priceChange = (Math.random() - 0.5) * volatility * basePrice;
    const currentPrice = basePrice + priceChange;
    
    return {
      symbol,
      exchange: "DEMO",
      timestamp: Date.now(),
      open: currentPrice * (0.995 + Math.random() * 0.01),
      high: currentPrice * (1.0 + Math.random() * 0.005),
      low: currentPrice * (0.995 - Math.random() * 0.005),
      close: currentPrice,
      volume: Math.random() * 1000 + 100,
      source: "crypto_compare"
    };
  }

  /**
   * Fetch historical data (simulated)
   */
  private async fetchHistoricalData(symbol: string): Promise<OHLCVData[]> {
    // In production, query TimescaleDB
    const data: OHLCVData[] = [];
    const now = Date.now();
    
    for (let i = 100; i >= 0; i--) {
      data.push(this.generateMockOHLCVData(symbol));
    }
    
    return data;
  }

  /**
   * Monitor specific symbol
   */
  private async monitorSymbol(symbol: string): Promise<void> {
    if (!this.monitoringConfig.symbols.includes(symbol)) {
      this.monitoringConfig.symbols.push(symbol);
      await this.connectToMarketDataStreams();
    }
  }

  /**
   * Check all active alerts
   */
  private async checkAlerts(): Promise<void> {
    // Cleanup expired alerts
    const now = Date.now();
    for (const [id, alert] of this.activeAlerts) {
      if (now - alert.timestamp.getTime() > 300000) { // 5 minutes
        this.activeAlerts.delete(id);
      }
    }
  }

  /**
   * Update alert thresholds
   */
  private async updateAlertThresholds(thresholds: Partial<MonitoringConfig["alertThresholds"]>): Promise<void> {
    this.monitoringConfig.alertThresholds = {
      ...this.monitoringConfig.alertThresholds,
      ...thresholds
    };
    this.logger.info("📊 Alert thresholds updated");
  }

  /**
   * Generate market report
   */
  private async generateMarketReport(): Promise<void> {
    const report = {
      timestamp: new Date(),
      symbolsMonitored: this.monitoringConfig.symbols.length,
      activeAlerts: this.activeAlerts.size,
      totalDataPoints: Array.from(this.priceHistory.values()).reduce((sum, history) => sum + history.length, 0),
      alertSummary: Array.from(this.activeAlerts.values()).reduce((acc, alert) => {
        acc[alert.type] = (acc[alert.type] || 0) + 1;
        return acc;
      }, {} as Record<string, number>)
    };
    
    this.logger.info(`📋 Market Report: ${JSON.stringify(report, null, 2)}`);
  }

  /**
   * Subscribe to alerts
   */
  private async subscribeToAlerts(agentId: string, alertTypes: string[]): Promise<void> {
    this.logger.info(`📬 Agent ${agentId} subscribed to alerts: ${alertTypes.join(", ")}`);
  }

  /**
   * Send market data to requesting agent
   */
  private async sendMarketData(agentId: string, symbol: string): Promise<void> {
    const history = this.priceHistory.get(symbol);
    if (history && history.length > 0) {
      const message: AgentMessage = {
        from: this.config.id,
        to: agentId,
        type: "market_data_response",
        payload: { symbol, data: history.slice(-10) }, // Last 10 data points
        timestamp: new Date()
      };
      
      await this.sendMessage(message);
    }
  }

  /**
   * Update monitoring configuration
   */
  private async updateMonitoringConfig(config: Partial<MonitoringConfig>): Promise<void> {
    this.monitoringConfig = { ...this.monitoringConfig, ...config };
    this.logger.info("⚙️ Monitoring configuration updated");
  }

  /**
   * Update performance metrics
   */
  private async updatePerformanceMetrics(): Promise<void> {
    // Calculate metrics like latency, data quality, alert accuracy
    const metrics = {
      dataPoints: Array.from(this.priceHistory.values()).reduce((sum, history) => sum + history.length, 0),
      alertsGenerated: this.activeAlerts.size,
      connectionsActive: this.websocketConnections.size
    };
    
    // Store metrics via MCP if available
    if (this.mcpClient.isConnected("memory")) {
      await this.mcpClient.storeMemory(`${this.config.id}-metrics`, JSON.stringify(metrics));
    }
  }

  /**
   * Cleanup old data to prevent memory leaks
   */
  private async cleanupOldData(): Promise<void> {
    const maxHistorySize = 1000;
    
    for (const [symbol, history] of this.priceHistory) {
      if (history.length > maxHistorySize) {
        this.priceHistory.set(symbol, history.slice(-maxHistorySize));
      }
    }
  }

  /**
   * Get current market status
   */
  getCurrentMarketStatus() {
    return {
      symbolsMonitored: this.monitoringConfig.symbols,
      activeConnections: this.websocketConnections.size,
      dataPoints: Array.from(this.priceHistory.values()).reduce((sum, history) => sum + history.length, 0),
      activeAlerts: Array.from(this.activeAlerts.values()),
      configuration: this.monitoringConfig
    };
  }
}