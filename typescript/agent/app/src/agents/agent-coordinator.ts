#!/usr/bin/env bun

/**
 * Agent Coordination Framework
 * 
 * Manages multiple agents, orchestrates workflows, and provides
 * communication and task distribution for the crypto trading platform.
 */

import { BaseAgent, AgentTask, AgentMessage, AgentState } from "./base-agent.js";

/**
 * Agent orchestration patterns
 */
export enum OrchestrationPattern {
  HIERARCHICAL = "hierarchical",    // Master-slave pattern
  COLLABORATIVE = "collaborative",  // Peer-to-peer coordination
  PIPELINE = "pipeline",           // Sequential processing
  SWARM = "swarm"                  // Distributed coordination
}

/**
 * Workflow definition for multi-agent coordination
 */
export interface AgentWorkflow {
  id: string;
  name: string;
  pattern: OrchestrationPattern;
  agents: string[];
  steps: WorkflowStep[];
  timeout?: number;
  retryPolicy?: {
    maxRetries: number;
    backoffMs: number;
  };
}

/**
 * Workflow step definition
 */
export interface WorkflowStep {
  id: string;
  agentType: string;
  taskType: string;
  dependencies?: string[];
  timeout?: number;
  continueOnError?: boolean;
}

/**
 * Workflow execution context
 */
export interface WorkflowContext {
  workflowId: string;
  stepId?: string;
  data: Record<string, any>;
  metadata: Record<string, any>;
  startTime: Date;
  currentStep: number;
}

/**
 * Agent performance metrics for coordination decisions
 */
export interface AgentPerformanceMetrics {
  agentId: string;
  averageResponseTime: number;
  successRate: number;
  currentLoad: number;
  specialization: string[];
}

/**
 * Agent Coordinator class for managing multi-agent workflows
 */
export class AgentCoordinator {
  private agents: Map<string, BaseAgent> = new Map();
  private workflows: Map<string, AgentWorkflow> = new Map();
  private activeWorkflows: Map<string, WorkflowContext> = new Map();
  private performanceMetrics: Map<string, AgentPerformanceMetrics> = new Map();
  
  constructor(
    private logger: {
      info: (msg: string) => void;
      warn: (msg: string) => void;
      error: (msg: string) => void;
    }
  ) {
    this.logger.info("🎭 Agent Coordinator initialized");
  }

  /**
   * Register an agent with the coordinator
   */
  registerAgent(agent: BaseAgent): void {
    const status = agent.getStatus();
    this.agents.set(status.id, agent);
    
    // Initialize performance metrics
    this.performanceMetrics.set(status.id, {
      agentId: status.id,
      averageResponseTime: 0,
      successRate: 1.0,
      currentLoad: 0,
      specialization: [status.type]
    });
    
    this.logger.info(`🤖 Registered agent: ${status.id} (${status.type})`);
  }

  /**
   * Unregister an agent
   */
  async unregisterAgent(agentId: string): Promise<void> {
    const agent = this.agents.get(agentId);
    if (agent) {
      await agent.stop();
      this.agents.delete(agentId);
      this.performanceMetrics.delete(agentId);
      this.logger.info(`🔚 Unregistered agent: ${agentId}`);
    }
  }

  /**
   * Register a workflow pattern
   */
  registerWorkflow(workflow: AgentWorkflow): void {
    this.workflows.set(workflow.id, workflow);
    this.logger.info(`📋 Registered workflow: ${workflow.name} (${workflow.pattern})`);
  }

  /**
   * Execute a workflow with the specified context
   */
  async executeWorkflow(workflowId: string, initialData: Record<string, any>): Promise<WorkflowContext> {
    const workflow = this.workflows.get(workflowId);
    if (!workflow) {
      throw new Error(`Workflow not found: ${workflowId}`);
    }

    const context: WorkflowContext = {
      workflowId,
      data: initialData,
      metadata: {},
      startTime: new Date(),
      currentStep: 0
    };

    this.activeWorkflows.set(workflowId, context);
    this.logger.info(`🚀 Starting workflow: ${workflow.name}`);

    try {
      switch (workflow.pattern) {
        case OrchestrationPattern.HIERARCHICAL:
          await this.executeHierarchicalWorkflow(workflow, context);
          break;
        case OrchestrationPattern.COLLABORATIVE:
          await this.executeCollaborativeWorkflow(workflow, context);
          break;
        case OrchestrationPattern.PIPELINE:
          await this.executePipelineWorkflow(workflow, context);
          break;
        case OrchestrationPattern.SWARM:
          await this.executeSwarmWorkflow(workflow, context);
          break;
        default:
          throw new Error(`Unsupported orchestration pattern: ${workflow.pattern}`);
      }

      this.logger.info(`✅ Workflow completed: ${workflow.name}`);
      return context;

    } catch (error) {
      this.logger.error(`❌ Workflow failed: ${workflow.name} - ${error}`);
      throw error;
    } finally {
      this.activeWorkflows.delete(workflowId);
    }
  }

  /**
   * Submit a task to the best available agent
   */
  async submitTaskToAgent(taskType: string, task: AgentTask): Promise<string> {
    const bestAgent = this.selectBestAgent(taskType);
    if (!bestAgent) {
      throw new Error(`No available agent for task type: ${taskType}`);
    }

    await bestAgent.submitTask(task);
    this.updateAgentLoad(bestAgent.getStatus().id, 1);
    
    this.logger.info(`📝 Task ${task.id} assigned to agent ${bestAgent.getStatus().id}`);
    return bestAgent.getStatus().id;
  }

  /**
   * Send a message between agents
   */
  async relayMessage(message: AgentMessage): Promise<void> {
    const targetAgent = this.agents.get(message.to);
    if (!targetAgent) {
      throw new Error(`Target agent not found: ${message.to}`);
    }

    await targetAgent.sendMessage(message);
    this.logger.info(`📬 Message relayed from ${message.from} to ${message.to}`);
  }

  /**
   * Get coordinator status and metrics
   */
  getStatus() {
    const agentStatuses = Array.from(this.agents.values()).map(agent => agent.getStatus());
    
    return {
      totalAgents: this.agents.size,
      registeredWorkflows: this.workflows.size,
      activeWorkflows: this.activeWorkflows.size,
      agents: agentStatuses,
      performanceMetrics: Array.from(this.performanceMetrics.values())
    };
  }

  /**
   * Execute hierarchical workflow (master-slave pattern)
   */
  private async executeHierarchicalWorkflow(workflow: AgentWorkflow, context: WorkflowContext): Promise<void> {
    // Master agent coordinates the workflow
    const masterAgent = this.selectBestAgent("coordinator");
    if (!masterAgent) {
      throw new Error("No coordinator agent available for hierarchical workflow");
    }

    for (const step of workflow.steps) {
      context.stepId = step.id;
      context.currentStep++;

      const task: AgentTask = {
        id: `${workflow.id}-${step.id}`,
        type: step.taskType,
        priority: "medium",
        payload: { context, step }
      };

      const agentId = await this.submitTaskToAgent(step.agentType, task);
      await this.waitForTaskCompletion(task.id, agentId);
    }
  }

  /**
   * Execute collaborative workflow (peer-to-peer)
   */
  private async executeCollaborativeWorkflow(workflow: AgentWorkflow, context: WorkflowContext): Promise<void> {
    // All agents work together as peers
    const collaboratingAgents = workflow.agents.map(agentType => 
      this.selectBestAgent(agentType)
    ).filter(agent => agent !== null);

    if (collaboratingAgents.length === 0) {
      throw new Error("No agents available for collaborative workflow");
    }

    // Execute steps in parallel where possible
    const independentSteps = workflow.steps.filter(step => !step.dependencies || step.dependencies.length === 0);
    
    await Promise.all(independentSteps.map(async (step) => {
      const task: AgentTask = {
        id: `${workflow.id}-${step.id}`,
        type: step.taskType,
        priority: "medium",
        payload: { context, step }
      };

      const agentId = await this.submitTaskToAgent(step.agentType, task);
      return this.waitForTaskCompletion(task.id, agentId);
    }));
  }

  /**
   * Execute pipeline workflow (sequential processing)
   */
  private async executePipelineWorkflow(workflow: AgentWorkflow, context: WorkflowContext): Promise<void> {
    // Process steps sequentially, passing data through the pipeline
    for (const step of workflow.steps) {
      context.stepId = step.id;
      context.currentStep++;

      const task: AgentTask = {
        id: `${workflow.id}-${step.id}`,
        type: step.taskType,
        priority: "medium",
        payload: { context, step }
      };

      const agentId = await this.submitTaskToAgent(step.agentType, task);
      const result = await this.waitForTaskCompletion(task.id, agentId);
      
      // Pass result to next step
      context.data[step.id] = result;
    }
  }

  /**
   * Execute swarm workflow (distributed coordination)
   */
  private async executeSwarmWorkflow(workflow: AgentWorkflow, context: WorkflowContext): Promise<void> {
    // Distribute work across multiple agents with dynamic coordination
    const availableAgents = Array.from(this.agents.values());
    const taskQueue = [...workflow.steps];

    while (taskQueue.length > 0) {
      const readyTasks = taskQueue.filter(step => 
        !step.dependencies || step.dependencies.every(dep => context.data[dep])
      );

      if (readyTasks.length === 0) {
        throw new Error("Workflow deadlock: no tasks can be executed");
      }

      // Execute ready tasks in parallel
      await Promise.all(readyTasks.map(async (step) => {
        const task: AgentTask = {
          id: `${workflow.id}-${step.id}`,
          type: step.taskType,
          priority: "medium",
          payload: { context, step }
        };

        const agentId = await this.submitTaskToAgent(step.agentType, task);
        const result = await this.waitForTaskCompletion(task.id, agentId);
        
        context.data[step.id] = result;
        
        // Remove completed task from queue
        const index = taskQueue.indexOf(step);
        if (index > -1) {
          taskQueue.splice(index, 1);
        }
      }));
    }
  }

  /**
   * Select the best agent for a given task type based on performance metrics
   */
  private selectBestAgent(taskType: string): BaseAgent | null {
    const availableAgents = Array.from(this.agents.values()).filter(agent => {
      const status = agent.getStatus();
      return status.state === AgentState.IDLE || status.state === AgentState.WORKING;
    });

    if (availableAgents.length === 0) {
      return null;
    }

    // Score agents based on performance metrics and current load
    let bestAgent = availableAgents[0];
    let bestScore = this.calculateAgentScore(bestAgent.getStatus().id, taskType);

    for (const agent of availableAgents.slice(1)) {
      const score = this.calculateAgentScore(agent.getStatus().id, taskType);
      if (score > bestScore) {
        bestAgent = agent;
        bestScore = score;
      }
    }

    return bestAgent;
  }

  /**
   * Calculate agent score for task assignment
   */
  private calculateAgentScore(agentId: string, taskType: string): number {
    const metrics = this.performanceMetrics.get(agentId);
    if (!metrics) return 0;

    // Factors: success rate, response time, current load, specialization
    const successScore = metrics.successRate;
    const speedScore = 1 / (1 + metrics.averageResponseTime / 1000); // Normalize to 0-1
    const loadScore = 1 - Math.min(metrics.currentLoad / 10, 1); // Penalize high load
    const specializationScore = metrics.specialization.includes(taskType) ? 1.2 : 1.0;

    return (successScore * 0.4 + speedScore * 0.3 + loadScore * 0.3) * specializationScore;
  }

  /**
   * Update agent load tracking
   */
  private updateAgentLoad(agentId: string, delta: number): void {
    const metrics = this.performanceMetrics.get(agentId);
    if (metrics) {
      metrics.currentLoad = Math.max(0, metrics.currentLoad + delta);
    }
  }

  /**
   * Wait for task completion (simplified implementation)
   */
  private async waitForTaskCompletion(taskId: string, agentId: string): Promise<any> {
    // In a real implementation, this would listen for task completion events
    // For now, simulate with a delay
    await new Promise(resolve => setTimeout(resolve, 1000));
    
    // Update agent metrics
    this.updateAgentLoad(agentId, -1);
    
    return { taskId, completed: true, timestamp: new Date() };
  }
}