import { beforeEach, describe, expect, it } from "vitest";
import { MCPClient } from "../../../lib/src/qimcp/client.js";

describe("MCPClient", () => {
	let client: MCPClient;
	let mockLogger: {
		info: (msg: string) => void;
		warn: (msg: string) => void;
		error: (msg: string) => void;
	};

	beforeEach(() => {
		mockLogger = {
			info: () => {},
			warn: () => {},
			error: () => {},
		};
		client = new MCPClient(mockLogger);
	});

	it("should create a client instance", () => {
		expect(client).toBeInstanceOf(MCPClient);
	});

	it("should have empty clients map initially", () => {
		expect(client.getConnectedServers()).toEqual([]);
	});

	it("should check if server is connected", () => {
		expect(client.isConnected("test-server")).toBe(false);
	});

	it("should return empty tools list for non-connected server", async () => {
		const tools = await client.getTools("non-existent");
		expect(tools).toEqual([]);
	});
});
