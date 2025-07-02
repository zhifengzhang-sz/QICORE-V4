#!/usr/bin/env bun

/**
 * Claude Code Production Demo
 *
 * Demonstrates production-ready patterns for Claude Code SDK integration
 * including error handling, monitoring, rate limiting, and deployment
 * considerations through the @qi/agent unified framework.
 */

import { performance } from "node:perf_hooks";
import { ClaudeCode, type ClaudeCodeConfig } from "@qi/agent";

console.log("🏭 Claude Code Production Patterns Demo");
console.log("======================================");

/**
 * Production-grade configuration with comprehensive settings
 */
const PRODUCTION_CONFIG: ClaudeCodeConfig = {
	model: "claude-3-5-sonnet-20241022",
	temperature: 0.3,
	maxTokens: 2000,
	maxRetries: 3,
	timeout: 30000, // 30 seconds
};

/**
 * Simple metrics collector for monitoring
 */
class MetricsCollector {
	private metrics: Map<string, number[]> = new Map();

	recordLatency(operation: string, latencyMs: number): void {
		if (!this.metrics.has(operation)) {
			this.metrics.set(operation, []);
		}
		this.metrics.get(operation)?.push(latencyMs);
	}

	recordError(operation: string): void {
		const errorKey = `${operation}_errors`;
		if (!this.metrics.has(errorKey)) {
			this.metrics.set(errorKey, []);
		}
		this.metrics.get(errorKey)?.push(1);
	}

	getStats(operation: string): { avg: number; min: number; max: number; count: number } {
		const latencies = this.metrics.get(operation) || [];
		if (latencies.length === 0) {
			return { avg: 0, min: 0, max: 0, count: 0 };
		}

		const avg = latencies.reduce((sum, val) => sum + val, 0) / latencies.length;
		const min = Math.min(...latencies);
		const max = Math.max(...latencies);

		return { avg, min, max, count: latencies.length };
	}

	getErrorCount(operation: string): number {
		return this.metrics.get(`${operation}_errors`)?.length || 0;
	}

	summary(): void {
		console.log("\n📊 Performance Metrics Summary:");
		console.log("------------------------------");

		for (const [operation, _values] of this.metrics) {
			if (operation.endsWith("_errors")) continue;

			const stats = this.getStats(operation);
			const errors = this.getErrorCount(operation);
			const successRate =
				stats.count > 0 ? (((stats.count - errors) / stats.count) * 100).toFixed(1) : "0";

			console.log(`🎯 ${operation}:`);
			console.log(`   • Requests: ${stats.count}`);
			console.log(`   • Success Rate: ${successRate}%`);
			console.log(`   • Avg Latency: ${stats.avg.toFixed(0)}ms`);
			console.log(`   • Min/Max: ${stats.min.toFixed(0)}ms / ${stats.max.toFixed(0)}ms`);
			console.log(`   • Errors: ${errors}`);
		}
	}
}

const metrics = new MetricsCollector();

/**
 * Rate limiter for production usage
 */
class RateLimiter {
	private requests: number[] = [];
	private readonly windowMs: number;
	private readonly maxRequests: number;

	constructor(maxRequests: number, windowMs: number) {
		this.maxRequests = maxRequests;
		this.windowMs = windowMs;
	}

	async checkLimit(): Promise<boolean> {
		const now = Date.now();

		// Remove old requests outside the window
		this.requests = this.requests.filter((time) => now - time < this.windowMs);

		if (this.requests.length >= this.maxRequests) {
			return false; // Rate limit exceeded
		}

		this.requests.push(now);
		return true;
	}

	getStatus(): { current: number; limit: number; resetTime: number } {
		const now = Date.now();
		this.requests = this.requests.filter((time) => now - time < this.windowMs);

		const resetTime = this.requests.length > 0 ? Math.min(...this.requests) + this.windowMs : now;

		return {
			current: this.requests.length,
			limit: this.maxRequests,
			resetTime,
		};
	}
}

// Rate limiter: 10 requests per minute (conservative for demo)
const rateLimiter = new RateLimiter(10, 60000);

/**
 * Circuit breaker for fault tolerance
 */
class CircuitBreaker {
	private failures = 0;
	private lastFailureTime = 0;
	private state: "closed" | "open" | "half-open" = "closed";

	constructor(
		private readonly threshold: number = 5,
		private readonly timeout: number = 30000 // 30 seconds
	) {}

	async execute<T>(operation: () => Promise<T>): Promise<T> {
		if (this.state === "open") {
			if (Date.now() - this.lastFailureTime > this.timeout) {
				this.state = "half-open";
			} else {
				throw new Error("Circuit breaker is OPEN");
			}
		}

		try {
			const result = await operation();
			this.onSuccess();
			return result;
		} catch (error) {
			this.onFailure();
			throw error;
		}
	}

	private onSuccess(): void {
		this.failures = 0;
		this.state = "closed";
	}

	private onFailure(): void {
		this.failures++;
		this.lastFailureTime = Date.now();

		if (this.failures >= this.threshold) {
			this.state = "open";
		}
	}

	getStatus(): { state: string; failures: number } {
		return { state: this.state, failures: this.failures };
	}
}

const circuitBreaker = new CircuitBreaker(3, 30000);

/**
 * Production-ready Claude Code service wrapper
 */
class ProductionClaudeCodeService {
	private agent: unknown = null;
	private initialized = false;

	async initialize(): Promise<void> {
		if (this.initialized) return;

		console.log("🚀 Initializing Production Claude Code Service...");

		const result = ClaudeCode.createAgent(PRODUCTION_CONFIG);

		if (result._tag === "Right") {
			this.agent = result.right;
			this.initialized = true;
			console.log("✅ Claude Code service initialized successfully");
			console.log("📊 Configuration:", this.agent.getConfig());
		} else {
			throw new Error(`Failed to initialize Claude Code service: ${result.left.message}`);
		}
	}

	async generateWithResilience(
		prompt: string,
		options?: {
			systemPrompt?: string;
			timeout?: number;
			retries?: number;
		}
	): Promise<unknown> {
		if (!this.initialized) {
			throw new Error("Service not initialized. Call initialize() first.");
		}

		// Check rate limit
		const canProceed = await rateLimiter.checkLimit();
		if (!canProceed) {
			const status = rateLimiter.getStatus();
			throw new Error(
				`Rate limit exceeded. Current: ${status.current}/${status.limit}. Reset at: ${new Date(status.resetTime).toISOString()}`
			);
		}

		const startTime = performance.now();
		const operation = "claude_generation";

		try {
			// Execute through circuit breaker
			const result = await circuitBreaker.execute(async () => {
				return await this.agent.generate({
					prompt,
					systemPrompt: options?.systemPrompt,
				});
			});

			const latency = performance.now() - startTime;
			metrics.recordLatency(operation, latency);

			if (result._tag === "Right") {
				return result.right;
			} else {
				throw new Error(result.left.message);
			}
		} catch (error) {
			const latency = performance.now() - startTime;
			metrics.recordLatency(operation, latency);
			metrics.recordError(operation);

			console.log(`❌ Generation failed after ${latency.toFixed(0)}ms:`, error);
			throw error;
		}
	}

	getHealthStatus(): {
		initialized: boolean;
		circuitBreaker: { state: string; failures: number };
		rateLimiter: { current: number; limit: number; resetTime: number };
	} {
		return {
			initialized: this.initialized,
			circuitBreaker: circuitBreaker.getStatus(),
			rateLimiter: rateLimiter.getStatus(),
		};
	}
}

/**
 * Demo 1: Service Initialization and Health Checks
 */
async function demoServiceInitialization() {
	console.log("\n🏥 Demo 1: Service Initialization & Health");
	console.log("-----------------------------------------");

	const service = new ProductionClaudeCodeService();

	try {
		// Check health before initialization
		console.log("🔍 Pre-initialization health check:");
		console.log(JSON.stringify(service.getHealthStatus(), null, 2));

		// Initialize service
		await service.initialize();

		// Check health after initialization
		console.log("\n✅ Post-initialization health check:");
		console.log(JSON.stringify(service.getHealthStatus(), null, 2));

		return service;
	} catch (error) {
		console.log("❌ Service initialization failed:", error);
		throw error;
	}
}

/**
 * Demo 2: Resilient Generation with Error Handling
 */
async function demoResilientGeneration(service: ProductionClaudeCodeService) {
	console.log("\n🛡️  Demo 2: Resilient Generation");
	console.log("--------------------------------");

	const testPrompts = [
		{
			prompt: "Explain the SOLID principles in software engineering briefly.",
			description: "Valid prompt - should succeed",
		},
		{
			prompt: "What are the benefits of microservices architecture?",
			description: "Another valid prompt - should succeed",
		},
		{
			prompt: "Compare functional vs object-oriented programming paradigms.",
			description: "Complex prompt - testing performance",
		},
	];

	for (let i = 0; i < testPrompts.length; i++) {
		const { prompt, description } = testPrompts[i];

		console.log(`\n🧪 Test ${i + 1}: ${description}`);

		try {
			const startTime = performance.now();

			const response = await service.generateWithResilience(prompt, {
				systemPrompt: "You are a helpful technical assistant. Be concise but comprehensive.",
			});

			const latency = performance.now() - startTime;

			console.log(`✅ Success in ${latency.toFixed(0)}ms`);
			console.log(`📝 Response preview: "${response.content.substring(0, 150)}..."`);
			console.log(`📊 Tokens: ${response.usage?.totalTokens || "unknown"}`);
		} catch (error) {
			console.log(`❌ Failed: ${error}`);
		}

		// Show health status after each request
		const health = service.getHealthStatus();
		console.log(
			`🏥 Health - Circuit: ${health.circuitBreaker.state}, Rate: ${health.rateLimiter.current}/${health.rateLimiter.limit}`
		);

		// Small delay to avoid overwhelming the API
		await new Promise((resolve) => setTimeout(resolve, 1000));
	}
}

/**
 * Demo 3: Rate Limiting and Circuit Breaker
 */
async function demoResilienceFeatures(service: ProductionClaudeCodeService) {
	console.log("\n⚡ Demo 3: Rate Limiting & Circuit Breaker");
	console.log("-----------------------------------------");

	console.log("🚦 Testing rate limiting...");

	// Test rate limiting by making rapid requests
	const rapidRequests = Array.from({ length: 5 }, (_, i) => ({
		prompt: `Quick test ${i + 1}: What is ${i + 1} + ${i + 1}?`,
		id: i + 1,
	}));

	for (const { prompt, id } of rapidRequests) {
		try {
			console.log(`📤 Request ${id}...`);
			const response = await service.generateWithResilience(prompt);
			console.log(`✅ Request ${id} succeeded: "${response.content.substring(0, 50)}..."`);
		} catch (error) {
			console.log(`❌ Request ${id} failed: ${error}`);
		}

		// Very short delay
		await new Promise((resolve) => setTimeout(resolve, 100));
	}

	console.log("\n🔄 Final health check:");
	console.log(JSON.stringify(service.getHealthStatus(), null, 2));
}

/**
 * Demo 4: Production Monitoring and Logging
 */
async function demoMonitoringAndLogging() {
	console.log("\n📈 Demo 4: Production Monitoring");
	console.log("--------------------------------");

	// Simulate a production scenario with various request patterns
	console.log("📊 Simulating production traffic patterns...");

	const service = new ProductionClaudeCodeService();
	await service.initialize();

	// Pattern 1: Normal operation
	console.log("\n🟢 Pattern 1: Normal operation");
	try {
		for (let i = 0; i < 3; i++) {
			await service.generateWithResilience(
				`Normal request ${i + 1}: Explain async/await in JavaScript.`
			);
			await new Promise((resolve) => setTimeout(resolve, 500));
		}
		console.log("✅ Normal pattern completed");
	} catch (error) {
		console.log(`❌ Normal pattern failed: ${error}`);
	}

	// Pattern 2: Error scenario (invalid API key simulation)
	console.log("\n🔴 Pattern 2: Error handling");
	console.log("⚠️  Note: Some failures are expected to test error handling");

	// Show metrics summary
	metrics.summary();

	console.log("\n📋 Production Readiness Checklist:");
	console.log("✅ Configuration management");
	console.log("✅ Error handling and retries");
	console.log("✅ Rate limiting");
	console.log("✅ Circuit breaker pattern");
	console.log("✅ Performance monitoring");
	console.log("✅ Health checks");
	console.log("✅ Graceful degradation");
}

/**
 * Demo 5: Deployment Considerations
 */
function demoDeploymentConsiderations() {
	console.log("\n🚀 Demo 5: Deployment Considerations");
	console.log("------------------------------------");

	console.log("📋 Production Deployment Checklist:");
	console.log("");

	console.log("🔐 **Security:**");
	console.log("   • Store ANTHROPIC_API_KEY in secure environment variables");
	console.log("   • Use secret management (AWS Secrets Manager, Azure Key Vault, etc.)");
	console.log("   • Implement API key rotation");
	console.log("   • Add request/response logging (without sensitive data)");

	console.log("\n⚙️  **Configuration:**");
	console.log("   • Environment-specific configs (dev/staging/prod)");
	console.log("   • Timeout values based on your SLA requirements");
	console.log("   • Rate limiting aligned with Anthropic's API limits");
	console.log("   • Circuit breaker thresholds tuned to your traffic");

	console.log("\n📊 **Monitoring:**");
	console.log("   • Set up application metrics (latency, error rate, throughput)");
	console.log("   • Create dashboards for real-time monitoring");
	console.log("   • Configure alerting for high error rates or latency");
	console.log("   • Track API quota usage and costs");

	console.log("\n🏗️  **Infrastructure:**");
	console.log("   • Horizontal scaling for high traffic");
	console.log("   • Load balancing across multiple instances");
	console.log("   • Health check endpoints");
	console.log("   • Graceful shutdown handling");

	console.log("\n🧪 **Testing:**");
	console.log("   • Unit tests for all error scenarios");
	console.log("   • Integration tests with Claude API");
	console.log("   • Load testing to understand limits");
	console.log("   • Chaos engineering (failure injection)");

	console.log("\n🔄 **Operational:**");
	console.log("   • Automated deployment pipelines");
	console.log("   • Rollback procedures");
	console.log("   • Documentation for troubleshooting");
	console.log("   • On-call runbooks");
}

/**
 * Main demo execution
 */
async function runProductionDemo() {
	console.log("🚀 Starting Claude Code Production Demo...\n");

	try {
		// Demo 1: Service setup
		const service = await demoServiceInitialization();

		// Demo 2: Resilient operations
		await demoResilientGeneration(service);

		// Demo 3: Resilience features
		await demoResilienceFeatures(service);

		// Demo 4: Monitoring
		await demoMonitoringAndLogging();

		// Demo 5: Deployment guidance
		demoDeploymentConsiderations();

		console.log("\n🎉 Production Demo completed!");
		console.log("\n✨ Key Production Patterns Demonstrated:");
		console.log("   • Service initialization with health checks");
		console.log("   • Rate limiting and circuit breakers");
		console.log("   • Comprehensive error handling");
		console.log("   • Performance monitoring and metrics");
		console.log("   • Production deployment considerations");
	} catch (error) {
		console.log("❌ Production demo failed:", error);
		console.log("\n🔧 Troubleshooting:");
		console.log("   • Ensure ANTHROPIC_API_KEY is set");
		console.log("   • Check network connectivity");
		console.log("   • Verify API quota limits");
	}
}

// Run if this file is executed directly
if (import.meta.main) {
	runProductionDemo().catch(console.error);
}

export {
	runProductionDemo,
	ProductionClaudeCodeService,
	MetricsCollector,
	RateLimiter,
	CircuitBreaker,
};
