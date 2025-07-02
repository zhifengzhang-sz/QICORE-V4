#!/usr/bin/env bun

/**
 * MCP Memory Tools - Memory Management for Analysis Results
 *
 * Provides high-level memory operations using MCP memory server
 */

import type { MCPClient } from "../client.js";

// Type for JSON-serializable data used in MCP
type MCPData = string | number | boolean | null | { [key: string]: MCPData } | MCPData[];

export interface MemoryResult {
	key: string;
	value: string;
	timestamp: string;
}

export interface AnalysisMemoryManager {
	storeAnalysisResult(key: string, data: MCPData): Promise<boolean>;
	retrieveAnalysisResult(key: string): Promise<MemoryResult | null>;
	storeMetrics(componentName: string, metrics: Record<string, MCPData>): Promise<boolean>;
	getAnalysisHistory(componentName: string): Promise<MemoryResult[]>;
}

export class MCPMemoryManager implements AnalysisMemoryManager {
	constructor(
		private mcpClient: MCPClient,
		private logger: {
			info: (msg: string) => void;
			warn: (msg: string) => void;
			error: (msg: string) => void;
		}
	) {}

	async storeAnalysisResult(key: string, data: MCPData): Promise<boolean> {
		if (!this.mcpClient.isConnected("memory")) {
			this.logger.warn("Memory server not connected");
			return false;
		}

		try {
			const dataObject =
				typeof data === "object" && data !== null && !Array.isArray(data) ? data : { value: data };

			const value = JSON.stringify({
				...dataObject,
				timestamp: new Date().toISOString(),
				type: "analysis_result",
			});

			const success = await this.mcpClient.storeMemory(key, value);
			if (success) {
				this.logger.info(`🧠 Stored analysis result: ${key}`);
			}
			return success;
		} catch (error) {
			this.logger.error(`Failed to store analysis result ${key}: ${error}`);
			return false;
		}
	}

	async retrieveAnalysisResult(key: string): Promise<MemoryResult | null> {
		if (!this.mcpClient.isConnected("memory")) {
			this.logger.warn("Memory server not connected");
			return null;
		}

		try {
			const result = await this.mcpClient.callTool("memory", "get_memory", { key });

			// Type guard for the result structure
			if (
				result &&
				typeof result === "object" &&
				"content" in result &&
				Array.isArray((result as { content: unknown }).content) &&
				(result as { content: { text?: string }[] }).content.length > 0 &&
				typeof (result as { content: { text?: string }[] }).content[0]?.text === "string"
			) {
				const content = (result as { content: { text: string }[] }).content[0];
				const data = JSON.parse(content.text);
				return {
					key,
					value: content.text,
					timestamp: data.timestamp || new Date().toISOString(),
				};
			}
			return null;
		} catch (error) {
			this.logger.error(`Failed to retrieve analysis result ${key}: ${error}`);
			return null;
		}
	}

	async storeMetrics(componentName: string, metrics: Record<string, MCPData>): Promise<boolean> {
		const key = `metrics:${componentName}:${Date.now()}`;
		return this.storeAnalysisResult(key, {
			component: componentName,
			metrics,
			type: "metrics",
		});
	}

	async getAnalysisHistory(componentName: string): Promise<MemoryResult[]> {
		// This would require the memory server to support querying by prefix
		// For now, return empty array as placeholder
		this.logger.info(`Getting analysis history for ${componentName}`);
		return [];
	}
}
