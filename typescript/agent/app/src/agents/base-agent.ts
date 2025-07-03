#!/usr/bin/env bun

/**
 * Base Agent Architecture for Crypto Data Platform
 * 
 * Foundational agent class that extends the existing MCP infrastructure
 * to support autonomous agent behaviors in cryptocurrency trading.
 */

// Simplified imports for demo - in production these would come from @qicore/agent-lib
export interface MCPServerConfig {
  name: string;
  command: string;
  args: string[];
  env: Record<string, string>;
}

// Stub MCP Client for demo
export class MCPClient {
  constructor(private logger: any) {}
  
  async connectToServer(config: MCPServerConfig): Promise<boolean> {
    this.logger.info(`Connected to ${config.name}`);
    return true;
  }
  
  async disconnect(): Promise<void> {
    this.logger.info("Disconnected from MCP servers");
  }
  
  isConnected(serverName: string): boolean {
    return true; // Simulate connection
  }
  
  getConnectedServers(): string[] {
    return ["memory", "filesystem"];
  }
  
  async saveFile(path: string, content: string): Promise<boolean> {
    this.logger.info(`Saved file: ${path}`);
    return true;
  }
  
  async storeMemory(key: string, value: string): Promise<void> {
    this.logger.info(`Stored memory: ${key}`);
  }
}

// Stub Mathematical Prompt Manager for demo
export interface MathematicalPromptManager {
  getStats(): any;
  reset(): void;
}

export function createMathematicalPromptManager(): MathematicalPromptManager {
  return {
    getStats: () => ({ templates: 0, cached: 0 }),
    reset: () => {}
  };
}

/**
 * Agent state enumeration for lifecycle management
 */
export enum AgentState {
  INITIALIZING = "initializing",
  IDLE = "idle", 
  WORKING = "working",
  ERROR = "error",
  SHUTDOWN = "shutdown"
}

/**
 * Agent capabilities that define what an agent can do
 */
export interface AgentCapabilities {
  canMonitor: boolean;
  canAnalyze: boolean; 
  canExecute: boolean;
  canCommunicate: boolean;
  canLearn: boolean;
}

/**
 * Agent performance metrics for monitoring and optimization
 */
export interface AgentMetrics {
  tasksCompleted: number;
  tasksErrors: number;
  averageResponseTime: number;
  lastActivity: Date;
  uptime: number;
  successRate: number;
}

/**
 * Agent configuration interface
 */
export interface AgentConfig {
  id: string;
  type: string;
  capabilities: AgentCapabilities;
  mcpServers?: MCPServerConfig[];
  updateInterval?: number;
  maxConcurrentTasks?: number;
}

/**
 * Task interface for agent work units
 */
export interface AgentTask {
  id: string;
  type: string;
  priority: "low" | "medium" | "high" | "critical";
  payload: any;
  deadline?: Date;
  retries?: number;
  maxRetries?: number;
}

/**
 * Agent communication message interface
 */
export interface AgentMessage {
  from: string;
  to: string;
  type: string;
  payload: any;
  timestamp: Date;
  correlationId?: string;
}

/**
 * Base Agent class providing core functionality for all specialized agents
 */
export abstract class BaseAgent {
  protected state: AgentState = AgentState.INITIALIZING;
  protected mcpClient: MCPClient;
  protected promptManager: MathematicalPromptManager;
  protected metrics: AgentMetrics;
  protected activeTasks: Map<string, AgentTask> = new Map();
  protected messageQueue: AgentMessage[] = [];
  
  constructor(
    protected config: AgentConfig,
    protected logger: {
      info: (msg: string) => void;
      warn: (msg: string) => void;
      error: (msg: string) => void;
    }
  ) {
    this.mcpClient = new MCPClient(logger);
    this.promptManager = createMathematicalPromptManager();
    
    this.metrics = {
      tasksCompleted: 0,
      tasksErrors: 0,
      averageResponseTime: 0,
      lastActivity: new Date(),
      uptime: 0,
      successRate: 1.0
    };
    
    this.logger.info(`🤖 Initializing agent: ${config.id} (${config.type})`);
  }

  /**
   * Start the agent - connect to MCP servers and begin operation
   */
  async start(): Promise<void> {
    this.logger.info(`🚀 Starting agent ${this.config.id}...`);
    
    try {
      // Connect to MCP servers if configured
      if (this.config.mcpServers && this.config.mcpServers.length > 0) {
        await this.connectToMCPServers();
      }
      
      // Initialize agent-specific resources
      await this.initialize();
      
      // Start main event loop
      this.startEventLoop();
      
      this.state = AgentState.IDLE;
      this.logger.info(`✅ Agent ${this.config.id} started successfully`);
      
    } catch (error) {
      this.state = AgentState.ERROR;
      this.logger.error(`❌ Failed to start agent ${this.config.id}: ${error}`);
      throw error;
    }
  }

  /**
   * Stop the agent gracefully
   */
  async stop(): Promise<void> {
    this.logger.info(`🛑 Stopping agent ${this.config.id}...`);
    
    this.state = AgentState.SHUTDOWN;
    
    // Complete active tasks
    await this.completeActiveTasks();
    
    // Disconnect from MCP servers
    await this.mcpClient.disconnect();
    
    // Cleanup agent-specific resources
    await this.cleanup();
    
    this.logger.info(`✅ Agent ${this.config.id} stopped`);
  }

  /**
   * Submit a task to the agent
   */
  async submitTask(task: AgentTask): Promise<void> {
    if (this.state !== AgentState.IDLE && this.state !== AgentState.WORKING) {
      throw new Error(`Agent ${this.config.id} is not available (state: ${this.state})`);
    }

    if (this.activeTasks.size >= (this.config.maxConcurrentTasks || 5)) {
      throw new Error(`Agent ${this.config.id} is at maximum task capacity`);
    }

    this.activeTasks.set(task.id, task);
    this.logger.info(`📝 Task submitted to agent ${this.config.id}: ${task.type} (${task.priority})`);
    
    // Process task asynchronously
    this.processTask(task).catch(error => {
      this.logger.error(`❌ Task ${task.id} failed: ${error}`);
      this.metrics.tasksErrors++;
      this.activeTasks.delete(task.id);
    });
  }

  /**
   * Send a message to another agent
   */
  async sendMessage(message: AgentMessage): Promise<void> {
    // For now, add to message queue - in production, this would use actual message broker
    this.messageQueue.push(message);
    this.logger.info(`📤 Message sent from ${message.from} to ${message.to}: ${message.type}`);
  }

  /**
   * Get agent status and metrics
   */
  getStatus() {
    return {
      id: this.config.id,
      type: this.config.type,
      state: this.state,
      capabilities: this.config.capabilities,
      metrics: this.metrics,
      activeTasks: this.activeTasks.size,
      messageQueue: this.messageQueue.length,
      mcpConnections: this.mcpClient.getConnectedServers()
    };
  }

  /**
   * Update agent metrics
   */
  protected updateMetrics(responseTime: number, success: boolean): void {
    this.metrics.lastActivity = new Date();
    
    if (success) {
      this.metrics.tasksCompleted++;
    } else {
      this.metrics.tasksErrors++;
    }
    
    // Update average response time using exponential moving average
    const alpha = 0.1;
    this.metrics.averageResponseTime = 
      alpha * responseTime + (1 - alpha) * this.metrics.averageResponseTime;
    
    // Update success rate
    const total = this.metrics.tasksCompleted + this.metrics.tasksErrors;
    this.metrics.successRate = total > 0 ? this.metrics.tasksCompleted / total : 1.0;
  }

  /**
   * Connect to configured MCP servers
   */
  private async connectToMCPServers(): Promise<void> {
    if (!this.config.mcpServers) return;
    
    this.logger.info(`🔌 Connecting to ${this.config.mcpServers.length} MCP servers...`);
    
    const connections = await Promise.allSettled(
      this.config.mcpServers.map(server => this.mcpClient.connectToServer(server))
    );
    
    const connected = connections.filter(r => r.status === "fulfilled" && r.value);
    this.logger.info(`✅ Connected to ${connected.length}/${this.config.mcpServers.length} MCP servers`);
  }

  /**
   * Start the main event loop for processing tasks and messages
   */
  private startEventLoop(): void {
    const interval = this.config.updateInterval || 1000;
    
    setInterval(async () => {
      if (this.state === AgentState.SHUTDOWN) return;
      
      try {
        // Process pending messages
        await this.processMessages();
        
        // Perform agent-specific periodic work
        await this.performPeriodicWork();
        
        // Update uptime
        this.metrics.uptime += interval;
        
      } catch (error) {
        this.logger.error(`⚠️ Event loop error in agent ${this.config.id}: ${error}`);
      }
    }, interval);
  }

  /**
   * Process pending messages
   */
  private async processMessages(): Promise<void> {
    while (this.messageQueue.length > 0) {
      const message = this.messageQueue.shift();
      if (message) {
        await this.handleMessage(message);
      }
    }
  }

  /**
   * Complete all active tasks before shutdown
   */
  private async completeActiveTasks(): Promise<void> {
    if (this.activeTasks.size === 0) return;
    
    this.logger.info(`⏳ Waiting for ${this.activeTasks.size} active tasks to complete...`);
    
    const timeout = 30000; // 30 seconds
    const start = Date.now();
    
    while (this.activeTasks.size > 0 && (Date.now() - start) < timeout) {
      await new Promise(resolve => setTimeout(resolve, 100));
    }
    
    if (this.activeTasks.size > 0) {
      this.logger.warn(`⚠️ ${this.activeTasks.size} tasks did not complete within timeout`);
    }
  }

  // Abstract methods that specialized agents must implement

  /**
   * Initialize agent-specific resources
   */
  protected abstract initialize(): Promise<void>;

  /**
   * Cleanup agent-specific resources
   */
  protected abstract cleanup(): Promise<void>;

  /**
   * Process a specific task
   */
  protected abstract processTask(task: AgentTask): Promise<void>;

  /**
   * Handle incoming messages from other agents
   */
  protected abstract handleMessage(message: AgentMessage): Promise<void>;

  /**
   * Perform agent-specific periodic work
   */
  protected abstract performPeriodicWork(): Promise<void>;
}