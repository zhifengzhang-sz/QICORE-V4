#!/usr/bin/env bun

/**
 * QiCore v4.0 MCP Client - Functional MCP Server Integration
 *
 * Mathematical Foundation:
 * - Client Monad: MCPClient<T> = Config → Promise<Result<T>>
 * - Connection Pool: Pool = Map<ServerName, Client>
 * - Performance Tier: TypeScript (interpreted) = 100× baseline, target < 500ms connection time
 *
 * Handles connections to external MCP servers (memory, filesystem, etc.) using
 * functional programming principles and qi/core Result<T> = Either<QiError, T> pattern.
 *
 * Derived from:
 * - QiCore v4.0 mathematical specifications
 * - Model Context Protocol (MCP) standards
 * - Functional composition and error handling patterns
 */

import { Client } from "@modelcontextprotocol/sdk/client/index.js";
import { StdioClientTransport } from "@modelcontextprotocol/sdk/client/stdio.js";
import {
	createQiError,
	failure,
	getError,
	isFailure,
	match,
	type QiError,
	type ResultType as Result,
	success,
} from "@qi/core/base";

// ============================================================================
// Core Types and Mathematical Structures
// ============================================================================

/**
 * MCP Server Configuration (Product Type)
 * Mathematical Structure: Config = Name × Command × Args × Env?
 * Performance: Configuration validation < 10μs (TypeScript interpreted tier)
 */
export interface MCPServerConfig {
	readonly name: string; // Unique server identifier
	readonly command: string; // Executable command path
	readonly args: string[]; // Command line arguments
	readonly env?: Record<string, string>; // Optional environment variables
	readonly timeout?: number; // Connection timeout (default: 30000ms)
	readonly maxRetries?: number; // Max connection retries (default: 3)
}

/**
 * MCP Tool Result (Product Type)
 * Mathematical Structure: ToolResult = Content × Metadata?
 * Performance: Result construction < 50μs (TypeScript interpreted tier)
 */
export interface MCPToolResult {
	readonly content: Array<{
		readonly type: string;
		readonly text?: string;
		readonly [key: string]: unknown;
	}>;
	readonly metadata?: {
		readonly serverName: string;
		readonly toolName: string;
		readonly timestamp: number;
		readonly duration?: number;
	};
}

/**
 * MCP Connection State (Sum Type)
 * Mathematical Structure: ConnectionState = Connected | Disconnected | Error
 */
export type MCPConnectionState =
	| { status: "connected"; client: Client; connectedAt: number }
	| { status: "disconnected" }
	| { status: "error"; error: QiError; attemptedAt: number };

/**
 * Logger Interface (Function Type)
 * Mathematical Structure: Logger = (Level × Message) → void
 */
export interface Logger {
	readonly info: (msg: string) => void;
	readonly warn: (msg: string) => void;
	readonly error: (msg: string) => void;
	readonly debug?: (msg: string) => void;
}

// ============================================================================
// Functional Utilities and Helpers
// ============================================================================

/**
 * Validate MCP server configuration
 * validateConfig: Config → Result<Config>
 * Performance: < 50μs (TypeScript interpreted tier)
 */
const validateConfig = (config: MCPServerConfig): Result<Required<MCPServerConfig>> => {
	if (!config.name || config.name.trim() === "") {
		return failure(
			createQiError("INVALID_CONFIG", "Server name is required and cannot be empty", "VALIDATION", {
				config,
			})
		);
	}

	if (!config.command || config.command.trim() === "") {
		return failure(
			createQiError("INVALID_CONFIG", "Command is required and cannot be empty", "VALIDATION", {
				config,
			})
		);
	}

	const validated: Required<MCPServerConfig> = {
		name: config.name.trim(),
		command: config.command.trim(),
		args: config.args || [],
		env: config.env || {},
		timeout: config.timeout || 30000,
		maxRetries: config.maxRetries || 3,
	};

	return success(validated);
};

/**
 * Create safe environment variables
 * createSafeEnv: (ProcessEnv, ConfigEnv?) → Record<string, string>
 * Performance: < 100μs (TypeScript interpreted tier)
 */
const createSafeEnv = (
	processEnv: NodeJS.ProcessEnv,
	configEnv: Record<string, string> = {}
): Record<string, string> => {
	const safeEnv: Record<string, string> = {};
	for (const [key, value] of Object.entries(processEnv)) {
		if (value !== undefined) {
			safeEnv[key] = value;
		}
	}
	return { ...safeEnv, ...configEnv };
};

/**
 * Map MCP connection errors to QiError
 * mapMCPError: (unknown, Context) → QiError
 * Performance: < 100μs (TypeScript interpreted tier)
 */
const mapMCPError = (
	error: unknown,
	context: { serverName: string; operation: string }
): QiError => {
	const errorMessage = error instanceof Error ? error.message : String(error);

	if (errorMessage.includes("timeout") || errorMessage.includes("ETIMEDOUT")) {
		return createQiError(
			"CONNECTION_TIMEOUT",
			`MCP server connection timeout: ${context.serverName}`,
			"TIMEOUT",
			{ ...context, originalError: errorMessage }
		);
	}

	if (errorMessage.includes("ECONNREFUSED") || errorMessage.includes("connection refused")) {
		return createQiError(
			"CONNECTION_REFUSED",
			`MCP server connection refused: ${context.serverName}`,
			"NETWORK",
			{ ...context, originalError: errorMessage }
		);
	}

	return createQiError(
		"MCP_ERROR",
		`MCP ${context.operation} failed for ${context.serverName}: ${errorMessage}`,
		"SYSTEM",
		{ ...context, originalError: errorMessage }
	);
};

// ============================================================================
// MCP Client Implementation
// ============================================================================

/**
 * MCP Client with functional programming patterns and structured error handling
 */
export class MCPClient {
	private readonly connections: Map<string, MCPConnectionState> = new Map();

	constructor(private readonly logger: Logger) {}

	/**
	 * Connect to an external MCP server (Functional)
	 * connectToServer: Config → Promise<Result<Client>>
	 * Performance: < 500ms typical connection time (network dependent)
	 */
	async connectToServer(config: MCPServerConfig): Promise<Result<Client>> {
		try {
			// Validate configuration
			const validationResult = validateConfig(config);
			if (isFailure(validationResult)) {
				const error = getError(validationResult);
				return failure(error || createQiError("VALIDATION_FAILED", "Configuration validation failed", "VALIDATION"));
			}

			const validatedConfig = match(
				(cfg: Required<MCPServerConfig>) => cfg,
				() => { throw new Error("Configuration validation failed"); }
			)(validationResult);

			this.logger.info(`🔌 Connecting to MCP server: ${validatedConfig.name}`);

			// Create environment variables safely
			const safeEnv = createSafeEnv(process.env, validatedConfig.env);

			// Create transport to external server
			const transport = new StdioClientTransport({
				command: validatedConfig.command,
				args: validatedConfig.args,
				env: safeEnv,
			});

			// Create client
			const client = new Client({
				name: "qicore-agent",
				version: "1.0.0",
			});

			// Connect with timeout
			const connectPromise = client.connect(transport);
			const timeoutPromise = new Promise((_, reject) =>
				setTimeout(() => reject(new Error("Connection timeout")), validatedConfig.timeout)
			);

			await Promise.race([connectPromise, timeoutPromise]);

			// Update connection state
			this.connections.set(validatedConfig.name, {
				status: "connected",
				client,
				connectedAt: Date.now(),
			});

			// Test connection
			await this.testServerConnection(validatedConfig.name, client);

			this.logger.info(`✅ Connected to ${validatedConfig.name} MCP server`);
			return success(client);
		} catch (error: unknown) {
			const qiError = mapMCPError(error, { serverName: config.name, operation: "connect" });
			this.connections.set(config.name, {
				status: "error",
				error: qiError,
				attemptedAt: Date.now(),
			});
			this.logger.error(`❌ Failed to connect to ${config.name}: ${qiError.message}`);
			return failure(qiError);
		}
	}

	/**
	 * Test if server connection is working
	 */
	private async testServerConnection(serverName: string, client: Client): Promise<void> {
		try {
			const tools = await client.listTools();
			this.logger.info(
				`🔧 ${serverName} tools: ${tools.tools?.map((t) => t.name).join(", ") || "none"}`
			);

			// Test specific server types
			if (serverName === "memory") {
				await this.testMemoryServer(client);
			} else if (serverName === "filesystem") {
				await this.testFilesystemServer(client);
			}
		} catch (error) {
			this.logger.warn(`⚠️  ${serverName} test failed: ${error}`);
		}
	}

	private async testMemoryServer(client: Client): Promise<void> {
		try {
			await client.callTool({
				name: "create_memory",
				arguments: {
					key: "test-connection",
					value: "MCP connection successful",
				},
			});
			this.logger.info(`✅ Memory server operational`);
		} catch (error) {
			this.logger.warn(`⚠️  Memory tool test failed: ${error}`);
		}
	}

	private async testFilesystemServer(client: Client): Promise<void> {
		try {
			await client.callTool({
				name: "list_directory",
				arguments: { path: "." },
			});
			this.logger.info(`✅ Filesystem server operational`);
		} catch (error) {
			this.logger.warn(`⚠️  Filesystem tool test failed: ${error}`);
		}
	}

	/**
	 * Call tool on specific server (Functional)
	 * callTool: (ServerName, ToolName, Args) → Promise<Result<MCPToolResult>>
	 * Performance: < 1s typical tool execution time (server dependent)
	 */
	async callTool(
		serverName: string,
		toolName: string,
		args: Record<string, unknown>
	): Promise<Result<MCPToolResult>> {
		const connectionState = this.connections.get(serverName);

		if (!connectionState || connectionState.status !== "connected") {
			const error = createQiError(
				"SERVER_NOT_CONNECTED",
				`MCP server ${serverName} not connected`,
				"BUSINESS",
				{ serverName, toolName, connectionStatus: connectionState?.status || "not_found" }
			);
			this.logger.warn(error.message);
			return failure(error);
		}

		const startTime = Date.now();
		try {
			const result = await connectionState.client.callTool({
				name: toolName,
				arguments: args,
			});

			const duration = Date.now() - startTime;
			this.logger.info(`Tool ${toolName} called successfully on ${serverName} (${duration}ms)`);

			const toolResult: MCPToolResult = {
				content: (result.content || []) as Array<{ readonly [key: string]: unknown; readonly type: string; readonly text?: string; }>,
				metadata: {
					serverName,
					toolName,
					timestamp: startTime,
					duration,
				},
			};

			return success(toolResult);
		} catch (error: unknown) {
			const qiError = mapMCPError(error, { serverName, operation: `callTool:${toolName}` });
			this.logger.error(`Tool call failed on ${serverName}: ${qiError.message}`);
			return failure(qiError);
		}
	}

	/**
	 * Get available tools from a server
	 */
	async getTools(serverName: string): Promise<string[]> {
		const connectionState = this.connections.get(serverName);
		if (!connectionState || connectionState.status !== "connected") {
			return [];
		}

		try {
			const result = await connectionState.client.listTools();
			return result.tools?.map((t: { name: string }) => t.name) || [];
		} catch (error) {
			this.logger.error(`❌ Failed to list tools for ${serverName}: ${error}`);
			return [];
		}
	}

	/**
	 * Save file using filesystem server
	 */
	async saveFile(filePath: string, content: string): Promise<boolean> {
		const result = await this.callTool("filesystem", "write_file", {
			path: filePath,
			contents: content,
		});

		if (result) {
			this.logger.info(`💾 Saved file via MCP: ${filePath}`);
			return true;
		}
		return false;
	}

	/**
	 * Store memory using memory server
	 */
	async storeMemory(key: string, value: string): Promise<boolean> {
		const result = await this.callTool("memory", "create_memory", {
			key,
			value,
		});

		if (result) {
			this.logger.info(`🧠 Stored memory via MCP: ${key}`);
			return true;
		}
		return false;
	}

	/**
	 * Check if a server is connected
	 */
	isConnected(serverName: string): boolean {
		const connectionState = this.connections.get(serverName);
		return connectionState?.status === "connected";
	}

	/**
	 * Get list of connected servers
	 */
	getConnectedServers(): string[] {
		return Array.from(this.connections.entries())
			.filter(([, state]) => state.status === "connected")
			.map(([name]) => name);
	}

	/**
	 * Disconnect from all servers
	 */
	async disconnect(): Promise<void> {
		for (const [name, connectionState] of this.connections) {
			if (connectionState.status === "connected") {
				try {
					await connectionState.client.close();
					this.logger.info(`🔌 Disconnected from ${name}`);
				} catch (error) {
					this.logger.warn(`⚠️  Error disconnecting from ${name}: ${error}`);
				}
			}
		}
		this.connections.clear();
	}
}
