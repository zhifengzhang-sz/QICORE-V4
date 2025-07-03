#!/usr/bin/env bun

/**
 * Technical Analysis Agent
 * 
 * Specialized agent for cryptocurrency technical analysis
 * Performs pattern recognition, indicator calculations, and signal generation
 */

import { BaseAgent, AgentTask, AgentMessage, AgentCapabilities, AgentConfig } from "./base-agent.js";
import { OHLCVData, MarketAlert } from "./market-monitoring-agent.js";

/**
 * Technical indicators interface
 */
export interface TechnicalIndicators {
  rsi: number;
  macd: {
    macd: number;
    signal: number;
    histogram: number;
  };
  bollingerBands: {
    upper: number;
    middle: number;
    lower: number;
  };
  movingAverages: {
    sma20: number;
    sma50: number;
    ema12: number;
    ema26: number;
  };
  volume: {
    vwap: number;
    volumeRatio: number;
  };
}

/**
 * Trading signal interface
 */
export interface TradingSignal {
  id: string;
  symbol: string;
  type: "buy" | "sell" | "hold";
  strength: "weak" | "moderate" | "strong";
  confidence: number; // 0-100
  indicators: string[];
  reasoning: string;
  timestamp: Date;
  targetPrice?: number;
  stopLoss?: number;
  timeframe: string;
}

/**
 * Pattern recognition result
 */
export interface PatternRecognition {
  pattern: string;
  confidence: number;
  timeframe: string;
  description: string;
  bullish: boolean;
  expectedDuration: number; // in minutes
}

/**
 * Analysis configuration
 */
export interface AnalysisConfig {
  timeframes: string[];
  indicators: string[];
  patternRecognition: boolean;
  signalGeneration: boolean;
  minConfidence: number;
  riskLevel: "conservative" | "moderate" | "aggressive";
}

/**
 * Technical Analysis Agent implementation
 */
export class TechnicalAnalysisAgent extends BaseAgent {
  private analysisConfig: AnalysisConfig;
  private indicatorCache: Map<string, TechnicalIndicators> = new Map();
  private signalHistory: Map<string, TradingSignal[]> = new Map();
  private patternHistory: Map<string, PatternRecognition[]> = new Map();

  constructor(
    config: AgentConfig & { analysisConfig: AnalysisConfig },
    logger: { info: (msg: string) => void; warn: (msg: string) => void; error: (msg: string) => void }
  ) {
    const capabilities: AgentCapabilities = {
      canMonitor: false,
      canAnalyze: true,
      canExecute: false,
      canCommunicate: true,
      canLearn: true
    };

    super({ ...config, capabilities }, logger);
    this.analysisConfig = config.analysisConfig;
  }

  /**
   * Initialize technical analysis resources
   */
  protected async initialize(): Promise<void> {
    this.logger.info("📈 Initializing technical analysis engine...");
    
    // Initialize indicator calculation templates
    await this.initializeIndicatorTemplates();
    
    // Load historical patterns for pattern recognition
    await this.loadHistoricalPatterns();
    
    this.logger.info("✅ Technical analysis engine initialized");
  }

  /**
   * Cleanup analysis resources
   */
  protected async cleanup(): Promise<void> {
    this.logger.info("🧹 Cleaning up technical analysis resources");
    
    this.indicatorCache.clear();
    this.signalHistory.clear();
    this.patternHistory.clear();
  }

  /**
   * Process analysis tasks
   */
  protected async processTask(task: AgentTask): Promise<void> {
    const startTime = Date.now();
    
    try {
      switch (task.type) {
        case "analyze_symbol":
          await this.analyzeSymbol(task.payload.symbol, task.payload.data);
          break;
        case "generate_signals":
          await this.generateTradingSignals(task.payload.symbols);
          break;
        case "pattern_recognition":
          await this.performPatternRecognition(task.payload.symbol, task.payload.data);
          break;
        case "calculate_indicators":
          await this.calculateTechnicalIndicators(task.payload.symbol, task.payload.data);
          break;
        case "backtest_strategy":
          await this.backtestStrategy(task.payload.strategy, task.payload.data);
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
      case "market_data_update":
        await this.processMarketDataUpdate(message.payload);
        break;
      case "request_analysis":
        await this.sendAnalysisResults(message.from, message.payload.symbol);
        break;
      case "update_analysis_config":
        await this.updateAnalysisConfig(message.payload.config);
        break;
      case "pattern_alert":
        await this.handlePatternAlert(message.payload);
        break;
      default:
        this.logger.warn(`🤷 Unknown message type: ${message.type}`);
    }
  }

  /**
   * Perform periodic analysis work
   */
  protected async performPeriodicWork(): Promise<void> {
    // Update cached indicators
    await this.updateIndicatorCache();
    
    // Generate new signals
    await this.generatePeriodicSignals();
    
    // Clean up old data
    await this.cleanupOldAnalysis();
  }

  /**
   * Analyze symbol with comprehensive technical analysis
   */
  private async analyzeSymbol(symbol: string, data: OHLCVData[]): Promise<void> {
    this.logger.info(`📊 Analyzing ${symbol} with ${data.length} data points`);
    
    if (data.length < 50) {
      this.logger.warn(`⚠️ Insufficient data for ${symbol} (${data.length} points)`);
      return;
    }

    // Calculate technical indicators
    const indicators = await this.calculateAllIndicators(symbol, data);
    this.indicatorCache.set(symbol, indicators);

    // Perform pattern recognition
    if (this.analysisConfig.patternRecognition) {
      const patterns = await this.recognizePatterns(symbol, data);
      this.patternHistory.set(symbol, patterns);
    }

    // Generate trading signals
    if (this.analysisConfig.signalGeneration) {
      const signals = await this.generateSignalsFromAnalysis(symbol, data, indicators);
      this.signalHistory.set(symbol, signals);
    }

    this.logger.info(`✅ Analysis completed for ${symbol}`);
  }

  /**
   * Calculate all technical indicators
   */
  private async calculateAllIndicators(symbol: string, data: OHLCVData[]): Promise<TechnicalIndicators> {
    const closes = data.map(d => d.close);
    const highs = data.map(d => d.high);
    const lows = data.map(d => d.low);
    const volumes = data.map(d => d.volume);

    return {
      rsi: this.calculateRSI(closes, 14),
      macd: this.calculateMACD(closes),
      bollingerBands: this.calculateBollingerBands(closes, 20, 2),
      movingAverages: {
        sma20: this.calculateSMA(closes, 20),
        sma50: this.calculateSMA(closes, 50),
        ema12: this.calculateEMA(closes, 12),
        ema26: this.calculateEMA(closes, 26)
      },
      volume: {
        vwap: this.calculateVWAP(data.slice(-20)),
        volumeRatio: volumes[volumes.length - 1] / this.calculateSMA(volumes, 20)
      }
    };
  }

  /**
   * Calculate RSI (Relative Strength Index)
   */
  private calculateRSI(prices: number[], period: number): number {
    if (prices.length < period + 1) return 50;

    let gains = 0;
    let losses = 0;

    for (let i = 1; i <= period; i++) {
      const change = prices[prices.length - i] - prices[prices.length - i - 1];
      if (change > 0) {
        gains += change;
      } else {
        losses -= change;
      }
    }

    const avgGain = gains / period;
    const avgLoss = losses / period;

    if (avgLoss === 0) return 100;

    const rs = avgGain / avgLoss;
    return 100 - (100 / (1 + rs));
  }

  /**
   * Calculate MACD (Moving Average Convergence Divergence)
   */
  private calculateMACD(prices: number[]): { macd: number; signal: number; histogram: number } {
    const ema12 = this.calculateEMA(prices, 12);
    const ema26 = this.calculateEMA(prices, 26);
    const macd = ema12 - ema26;
    
    // Simplified signal line calculation
    const signal = macd * 0.9; // Approximation
    const histogram = macd - signal;

    return { macd, signal, histogram };
  }

  /**
   * Calculate Bollinger Bands
   */
  private calculateBollingerBands(prices: number[], period: number, stdDev: number): { upper: number; middle: number; lower: number } {
    const middle = this.calculateSMA(prices, period);
    const variance = this.calculateVariance(prices.slice(-period));
    const standardDeviation = Math.sqrt(variance);

    return {
      upper: middle + (standardDeviation * stdDev),
      middle,
      lower: middle - (standardDeviation * stdDev)
    };
  }

  /**
   * Calculate Simple Moving Average
   */
  private calculateSMA(prices: number[], period: number): number {
    if (prices.length < period) return prices[prices.length - 1] || 0;
    
    const sum = prices.slice(-period).reduce((a, b) => a + b, 0);
    return sum / period;
  }

  /**
   * Calculate Exponential Moving Average
   */
  private calculateEMA(prices: number[], period: number): number {
    if (prices.length === 0) return 0;
    if (prices.length === 1) return prices[0];

    const multiplier = 2 / (period + 1);
    let ema = prices[0];

    for (let i = 1; i < Math.min(prices.length, period * 2); i++) {
      ema = (prices[i] * multiplier) + (ema * (1 - multiplier));
    }

    return ema;
  }

  /**
   * Calculate VWAP (Volume Weighted Average Price)
   */
  private calculateVWAP(data: OHLCVData[]): number {
    let totalVolume = 0;
    let totalVolumePrice = 0;

    for (const point of data) {
      const typicalPrice = (point.high + point.low + point.close) / 3;
      totalVolumePrice += typicalPrice * point.volume;
      totalVolume += point.volume;
    }

    return totalVolume > 0 ? totalVolumePrice / totalVolume : 0;
  }

  /**
   * Calculate variance for standard deviation
   */
  private calculateVariance(prices: number[]): number {
    const mean = prices.reduce((a, b) => a + b, 0) / prices.length;
    const squaredDiffs = prices.map(price => Math.pow(price - mean, 2));
    return squaredDiffs.reduce((a, b) => a + b, 0) / prices.length;
  }

  /**
   * Recognize chart patterns
   */
  private async recognizePatterns(symbol: string, data: OHLCVData[]): Promise<PatternRecognition[]> {
    const patterns: PatternRecognition[] = [];

    // Head and Shoulders pattern
    const headAndShoulders = this.detectHeadAndShoulders(data);
    if (headAndShoulders) {
      patterns.push(headAndShoulders);
    }

    // Double Top/Bottom
    const doublePattern = this.detectDoubleTopBottom(data);
    if (doublePattern) {
      patterns.push(doublePattern);
    }

    // Triangle patterns
    const trianglePattern = this.detectTrianglePattern(data);
    if (trianglePattern) {
      patterns.push(trianglePattern);
    }

    // Support/Resistance levels
    const supportResistance = this.detectSupportResistance(data);
    if (supportResistance) {
      patterns.push(supportResistance);
    }

    return patterns;
  }

  /**
   * Detect Head and Shoulders pattern
   */
  private detectHeadAndShoulders(data: OHLCVData[]): PatternRecognition | null {
    if (data.length < 20) return null;

    // Simplified pattern detection - in production would use more sophisticated algorithms
    const recentData = data.slice(-20);
    const highs = recentData.map(d => d.high);
    const maxIndex = highs.indexOf(Math.max(...highs));
    
    if (maxIndex > 5 && maxIndex < 15) {
      return {
        pattern: "Head and Shoulders",
        confidence: 65,
        timeframe: "4h",
        description: "Potential bearish reversal pattern detected",
        bullish: false,
        expectedDuration: 240
      };
    }

    return null;
  }

  /**
   * Detect Double Top/Bottom pattern
   */
  private detectDoubleTopBottom(data: OHLCVData[]): PatternRecognition | null {
    if (data.length < 15) return null;

    const recentData = data.slice(-15);
    const closes = recentData.map(d => d.close);
    
    // Simple double top detection
    const peaks = this.findPeaks(closes);
    if (peaks.length >= 2) {
      const lastTwoPeaks = peaks.slice(-2);
      const priceDiff = Math.abs(lastTwoPeaks[0] - lastTwoPeaks[1]) / lastTwoPeaks[0];
      
      if (priceDiff < 0.02) { // Within 2%
        return {
          pattern: "Double Top",
          confidence: 70,
          timeframe: "1h",
          description: "Double top pattern suggests bearish reversal",
          bullish: false,
          expectedDuration: 120
        };
      }
    }

    return null;
  }

  /**
   * Detect Triangle pattern
   */
  private detectTrianglePattern(data: OHLCVData[]): PatternRecognition | null {
    if (data.length < 12) return null;

    // Simplified triangle detection
    const recentData = data.slice(-12);
    const highs = recentData.map(d => d.high);
    const lows = recentData.map(d => d.low);
    
    const isConverging = this.detectConvergence(highs, lows);
    
    if (isConverging) {
      return {
        pattern: "Symmetrical Triangle",
        confidence: 60,
        timeframe: "30m",
        description: "Triangle pattern indicates potential breakout",
        bullish: true, // Direction depends on breakout
        expectedDuration: 90
      };
    }

    return null;
  }

  /**
   * Detect Support/Resistance levels
   */
  private detectSupportResistance(data: OHLCVData[]): PatternRecognition | null {
    if (data.length < 20) return null;

    const closes = data.slice(-20).map(d => d.close);
    const currentPrice = closes[closes.length - 1];
    
    // Find potential support/resistance levels
    const levels = this.findKeyLevels(closes);
    const nearestLevel = levels.find(level => Math.abs(level - currentPrice) / currentPrice < 0.01);
    
    if (nearestLevel) {
      return {
        pattern: currentPrice < nearestLevel ? "Resistance Level" : "Support Level",
        confidence: 75,
        timeframe: "1h",
        description: `Strong ${currentPrice < nearestLevel ? "resistance" : "support"} at ${nearestLevel.toFixed(2)}`,
        bullish: currentPrice > nearestLevel,
        expectedDuration: 180
      };
    }

    return null;
  }

  /**
   * Generate trading signals from analysis
   */
  private async generateSignalsFromAnalysis(symbol: string, data: OHLCVData[], indicators: TechnicalIndicators): Promise<TradingSignal[]> {
    const signals: TradingSignal[] = [];
    const currentPrice = data[data.length - 1].close;

    // RSI-based signals
    if (indicators.rsi < 30) {
      signals.push({
        id: `${symbol}-rsi-oversold-${Date.now()}`,
        symbol,
        type: "buy",
        strength: "moderate",
        confidence: 65,
        indicators: ["RSI"],
        reasoning: `RSI is oversold at ${indicators.rsi.toFixed(1)}`,
        timestamp: new Date(),
        targetPrice: currentPrice * 1.05,
        stopLoss: currentPrice * 0.97,
        timeframe: "1h"
      });
    } else if (indicators.rsi > 70) {
      signals.push({
        id: `${symbol}-rsi-overbought-${Date.now()}`,
        symbol,
        type: "sell",
        strength: "moderate",
        confidence: 65,
        indicators: ["RSI"],
        reasoning: `RSI is overbought at ${indicators.rsi.toFixed(1)}`,
        timestamp: new Date(),
        targetPrice: currentPrice * 0.95,
        stopLoss: currentPrice * 1.03,
        timeframe: "1h"
      });
    }

    // MACD-based signals
    if (indicators.macd.macd > indicators.macd.signal && indicators.macd.histogram > 0) {
      signals.push({
        id: `${symbol}-macd-bullish-${Date.now()}`,
        symbol,
        type: "buy",
        strength: "strong",
        confidence: 75,
        indicators: ["MACD"],
        reasoning: "MACD shows bullish crossover with positive histogram",
        timestamp: new Date(),
        targetPrice: currentPrice * 1.08,
        stopLoss: currentPrice * 0.95,
        timeframe: "4h"
      });
    }

    // Moving Average crossover
    if (indicators.movingAverages.ema12 > indicators.movingAverages.ema26) {
      signals.push({
        id: `${symbol}-ma-golden-cross-${Date.now()}`,
        symbol,
        type: "buy",
        strength: "strong",
        confidence: 80,
        indicators: ["EMA12", "EMA26"],
        reasoning: "Golden cross: EMA12 above EMA26",
        timestamp: new Date(),
        targetPrice: currentPrice * 1.10,
        stopLoss: currentPrice * 0.93,
        timeframe: "4h"
      });
    }

    // Bollinger Bands signals
    if (currentPrice < indicators.bollingerBands.lower) {
      signals.push({
        id: `${symbol}-bb-oversold-${Date.now()}`,
        symbol,
        type: "buy",
        strength: "moderate",
        confidence: 60,
        indicators: ["Bollinger Bands"],
        reasoning: "Price below lower Bollinger Band",
        timestamp: new Date(),
        targetPrice: indicators.bollingerBands.middle,
        stopLoss: currentPrice * 0.97,
        timeframe: "2h"
      });
    }

    return signals.filter(signal => signal.confidence >= this.analysisConfig.minConfidence);
  }

  // Helper methods

  private async initializeIndicatorTemplates(): Promise<void> {
    // Initialize any pre-computed templates or ML models for analysis
  }

  private async loadHistoricalPatterns(): Promise<void> {
    // Load historical pattern data for better recognition
  }

  private findPeaks(data: number[]): number[] {
    const peaks: number[] = [];
    for (let i = 1; i < data.length - 1; i++) {
      if (data[i] > data[i - 1] && data[i] > data[i + 1]) {
        peaks.push(data[i]);
      }
    }
    return peaks;
  }

  private detectConvergence(highs: number[], lows: number[]): boolean {
    // Simplified convergence detection
    const highRange = Math.max(...highs) - Math.min(...highs);
    const lowRange = Math.max(...lows) - Math.min(...lows);
    return (highRange + lowRange) / highs.length < 0.02; // Within 2% range
  }

  private findKeyLevels(prices: number[]): number[] {
    // Simplified key level detection
    const sorted = [...prices].sort((a, b) => a - b);
    return [
      sorted[Math.floor(sorted.length * 0.25)], // 25th percentile
      sorted[Math.floor(sorted.length * 0.5)],  // Median
      sorted[Math.floor(sorted.length * 0.75)]  // 75th percentile
    ];
  }

  private async generateTradingSignals(symbols: string[]): Promise<void> {
    // Generate signals for multiple symbols
  }

  private async performPatternRecognition(symbol: string, data: OHLCVData[]): Promise<void> {
    // Perform pattern recognition for specific symbol
  }

  private async calculateTechnicalIndicators(symbol: string, data: OHLCVData[]): Promise<void> {
    // Calculate indicators for specific symbol
  }

  private async backtestStrategy(strategy: any, data: OHLCVData[]): Promise<void> {
    // Backtest trading strategy
  }

  private async processMarketDataUpdate(payload: any): Promise<void> {
    // Process incoming market data updates
  }

  private async sendAnalysisResults(agentId: string, symbol: string): Promise<void> {
    // Send analysis results to requesting agent
  }

  private async updateAnalysisConfig(config: Partial<AnalysisConfig>): Promise<void> {
    this.analysisConfig = { ...this.analysisConfig, ...config };
  }

  private async handlePatternAlert(payload: any): Promise<void> {
    // Handle pattern alerts from other agents
  }

  private async updateIndicatorCache(): Promise<void> {
    // Update cached indicators periodically
  }

  private async generatePeriodicSignals(): Promise<void> {
    // Generate signals on schedule
  }

  private async cleanupOldAnalysis(): Promise<void> {
    // Cleanup old analysis data
  }

  /**
   * Get current analysis status
   */
  getCurrentAnalysisStatus() {
    return {
      cachedIndicators: this.indicatorCache.size,
      signalHistory: Array.from(this.signalHistory.values()).flat().length,
      patternHistory: Array.from(this.patternHistory.values()).flat().length,
      configuration: this.analysisConfig
    };
  }
}