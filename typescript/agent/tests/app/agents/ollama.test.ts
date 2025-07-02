import { beforeEach, describe, expect, it } from "vitest";
import { MathematicalOllamaAgent } from "../../../app/src/agents/ollama.ts";

describe("MathematicalOllamaAgent", () => {
	let agent: MathematicalOllamaAgent;

	beforeEach(() => {
		agent = new MathematicalOllamaAgent();
	});

	it("should create an agent instance", () => {
		expect(agent).toBeInstanceOf(MathematicalOllamaAgent);
	});

	it("should have analyzeMathematicalContracts method", () => {
		expect(typeof agent.analyzeMathematicalContracts).toBe("function");
	});

	it("should have verifyAlgebraicLaws method", () => {
		expect(typeof agent.verifyAlgebraicLaws).toBe("function");
	});

	it("should be properly configured for mathematical analysis", () => {
		// Test that the agent is properly initialized
		expect(agent).toBeDefined();
		expect(agent.analyzeMathematicalContracts).toBeDefined();
		expect(agent.verifyAlgebraicLaws).toBeDefined();
	});
});
