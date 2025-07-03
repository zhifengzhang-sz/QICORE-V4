# QiCore Agent Application

**Autonomous cryptocurrency trading system with multi-agent orchestration**

## Overview

This application has been transformed from a simple prompt-to-file mathematical verification system into a sophisticated **autonomous agent orchestration platform** for cryptocurrency trading. The system leverages our comprehensive crypto data platform proposal to create a production-ready foundation for AI-driven financial markets.

## Architecture

### 🤖 Multi-Agent System

The system consists of specialized agents that work together:

1. **Market Monitoring Agent** - Real-time price/volume monitoring with alerts
2. **Technical Analysis Agent** - Pattern recognition and trading signal generation  
3. **Execution Agent** - Trade execution with comprehensive risk management
4. **Agent Coordinator** - Orchestrates workflows and agent communication

### 🔄 Orchestration Patterns

- **Hierarchical**: Master-slave coordination for complex workflows
- **Collaborative**: Peer-to-peer agent cooperation
- **Pipeline**: Sequential data processing workflows
- **Swarm**: Distributed task coordination

### 🛡️ Risk Management

- Position sizing based on portfolio risk parameters
- Automated stop-loss and take-profit management
- Real-time portfolio monitoring and drawdown protection
- Correlation risk assessment across positions

## Quick Start

### Run Autonomous Trading System

```bash
# Start the crypto trading orchestrator
bun run trading

# Or directly:
bun run src/index.ts
```

### Run Legacy Mathematical Verification

```bash
# Run the original mathematical verification agent
bun run legacy

# Or via MCP verification:
bun run mcp:verify
```

### Demo Mode

```bash
# Run standalone crypto orchestrator demo
bun run demo:crypto
```

## Features

### 🎯 Core Capabilities

- **Real-time market monitoring** across multiple cryptocurrency pairs
- **Technical analysis** with RSI, MACD, Bollinger Bands, and moving averages
- **Pattern recognition** for head & shoulders, double tops/bottoms, triangles
- **Automated trading** with risk-managed position sizing
- **Portfolio management** with diversification and correlation analysis
- **Multi-agent coordination** with sophisticated workflow orchestration

### 📊 Risk Management

- **Maximum position size limits** (default: 25% of portfolio)
- **Stop-loss protection** (default: 3% loss limit)
- **Take-profit targets** (default: 8% profit target)  
- **Daily loss limits** (default: 5% max daily loss)
- **Drawdown protection** (default: 15% max drawdown)
- **Risk per trade** (default: 2% portfolio risk)

### 🔧 Technical Integration

- **MCP (Model Context Protocol)** for standardized agent-tool interfaces
- **TimescaleDB** integration for real-time operational data
- **ClickHouse** integration for analytical workloads
- **Redpanda/Kafka** for high-performance data streaming
- **WebSocket** connections for real-time market data

## Configuration

### Demo Configuration

```typescript
const DEMO_CONFIG = {
  portfolio: {
    initialCapital: 100000,  // $100k starting capital
    riskLevel: "moderate"    // conservative | moderate | aggressive
  },
  monitoring: {
    symbols: ["BTCUSD", "ETHUSD", "ADAUSD"],
    exchanges: ["binance", "coinbase"],
    updateInterval: 5000     // 5 second updates
  },
  analysis: {
    timeframes: ["5m", "15m", "1h", "4h"],
    indicators: ["RSI", "MACD", "BollingerBands", "EMA"],
    minConfidence: 65        // 65% minimum signal confidence
  },
  execution: {
    maxPositionSize: 25,     // 25% max position size
    stopLossPercent: 3,      // 3% stop loss
    takeProfitPercent: 8     // 8% take profit
  }
};
```

## System Status

Monitor real-time system performance:

```typescript
// Get comprehensive system status
const status = orchestrator.getSystemStatus();

console.log(status);
// {
//   orchestrator: { status: "active", uptime: 123456 },
//   coordinator: { totalAgents: 3, activeWorkflows: 2 },
//   agents: {
//     market: { state: "working", activeTasks: 1 },
//     analysis: { state: "idle", cachedIndicators: 15 },
//     execution: { state: "idle", totalValue: 105234 }
//   },
//   session: { totalTrades: 12, successfulTrades: 10, totalPnL: 5234 }
// }
```

## Market Event Processing

The system processes various market events:

```typescript
// Price update events trigger trading signal workflows
await orchestrator.processMarketEvent({
  type: "price_update",
  symbol: "BTCUSD", 
  data: { price: 45000, volume: 1500, timestamp: Date.now() }
});

// Volume spikes trigger analysis workflows  
await orchestrator.processMarketEvent({
  type: "volume_spike",
  symbol: "ETHUSD",
  data: { volumeMultiplier: 4.2, threshold: 3.0 }
});

// Pattern detection triggers execution workflows
await orchestrator.processMarketEvent({
  type: "pattern_detected", 
  symbol: "ADAUSD",
  data: { pattern: "Head and Shoulders", confidence: 75 }
});
```

## Agent Communication

Agents communicate via standardized message protocols:

```typescript
// Market monitoring agent sends alerts
const alertMessage = {
  from: "market-monitor",
  to: "technical-analyst", 
  type: "price_breakout",
  payload: { symbol: "BTCUSD", changePercent: 5.2 },
  timestamp: new Date()
};

// Technical analysis agent sends trading signals
const signalMessage = {
  from: "technical-analyst",
  to: "execution-engine",
  type: "trading_signal",
  payload: { 
    signal: {
      symbol: "ETHUSD",
      type: "buy",
      confidence: 78,
      targetPrice: 3100,
      stopLoss: 2950
    }
  },
  timestamp: new Date()
};
```

## Development

### Project Structure

```
app/
├── src/
│   ├── agents/                    # Specialized agent implementations
│   │   ├── base-agent.ts         # Base agent class with MCP integration
│   │   ├── agent-coordinator.ts  # Multi-agent orchestration
│   │   ├── market-monitoring-agent.ts    # Real-time market monitoring
│   │   ├── technical-analysis-agent.ts   # Technical analysis & signals
│   │   └── execution-agent.ts    # Trade execution & risk management
│   ├── crypto-agent-orchestrator.ts     # Main orchestrator
│   ├── index.ts                  # Application entry point
│   └── mcp-verification-agent.ts # Legacy mathematical verification
├── output/                       # Analysis results and reports
├── package.json
└── README.md
```

### Running Tests

```bash
# Run all tests
bun run test

# Watch mode for development
bun run test:watch

# Coverage analysis
bun run test:coverage

# Type checking
bun run typecheck
```

## Integration with Crypto Data Platform

This agent system is designed to integrate with our comprehensive crypto data platform:

- **Data Sources**: CryptoCompare API, TwelveData, exchange direct feeds
- **Streaming**: Redpanda/Kafka for real-time data processing
- **Storage**: TimescaleDB for operational data, ClickHouse for analytics
- **MCP Tools**: Standardized interfaces for agent-platform communication

See `docs/dp/` for the complete crypto data platform proposal and architecture.

## Production Deployment

For production deployment:

1. **Configure real exchange connections** (replace demo simulation)
2. **Setup TimescaleDB and ClickHouse** databases
3. **Deploy Redpanda/Kafka** streaming infrastructure
4. **Configure MCP servers** for memory and filesystem operations
5. **Set up monitoring** and alerting for agent performance
6. **Implement proper security** for API keys and trading credentials

## Legacy Mode

The original mathematical verification agent is preserved for compatibility:

```bash
# Run mathematical contract verification
bun run legacy

# This will analyze:
# - QiError algebraic properties
# - Result<T> monad verification  
# - Configuration monoid verification
```

## Contributing

This system demonstrates the transformation from simple prompt-to-file workflows to sophisticated multi-agent orchestration. The architecture is designed to be:

- **Modular**: Each agent is independently testable and deployable
- **Scalable**: Can handle thousands of simultaneous market events
- **Extensible**: Easy to add new agent types and capabilities
- **Robust**: Comprehensive error handling and graceful degradation

## License

MIT License - See LICENSE file for details.

---

*Built with QiCore MCP tools and designed as the foundation for autonomous AI agents in financial markets.*