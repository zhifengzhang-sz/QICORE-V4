import { promises as fs } from "node:fs";
import { afterEach, beforeEach, describe, expect, it } from "vitest";
import { createQiError } from "../../../src/qicore/base/error.js";
import {
	failure,
	isFailure,
	isSuccess,
	success,
} from "../../../src/qicore/base/result.js";
import {
	empty,
	fromEnvironment,
	fromFile,
	fromObject,
	fromString,
	get,
	has,
	merge,
	mergeAll,
	set,
	validateCustom,
	validateRequired,
	validateType,
} from "../../../src/qicore/core/config.js";

describe("Configuration", () => {
	const testFilePath = "/tmp/test-config.json";

	afterEach(async () => {
		try {
			await fs.unlink(testFilePath);
		} catch {
			// Ignore if file doesn't exist
		}
	});

	describe("fromObject", () => {
		it("should create config from object", () => {
			const obj = { name: "test", port: 8080 };
			const result = fromObject(obj);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.get("name")).toBe("test");
				expect(result.right.data.get("port")).toBe(8080);
			}
		});

		it("should handle nested objects", () => {
			const obj = {
				server: { host: "localhost", port: 8080 },
				database: { url: "postgres://localhost" },
			};
			const result = fromObject(obj);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				const server = result.right.data.get("server") as Record<
					string,
					unknown
				>;
				expect(server.host).toBe("localhost");
				expect(server.port).toBe(8080);
			}
		});

		it("should handle empty object", () => {
			const result = fromObject({});

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.size).toBe(0);
			}
		});
	});

	describe("fromFile", () => {
		it("should load config from JSON file", async () => {
			const config = { name: "test", port: 8080 };
			await fs.writeFile(testFilePath, JSON.stringify(config));

			const result = await fromFile(testFilePath);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.get("name")).toBe("test");
				expect(result.right.data.get("port")).toBe(8080);
			}
		});

		it("should fail for non-existent file", async () => {
			const result = await fromFile("/non/existent/file.json");

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("CONFIG_FILE_ERROR");
				expect(result.left.category).toBe("FILESYSTEM");
			}
		});

		it("should fail for invalid JSON", async () => {
			await fs.writeFile(testFilePath, "{ invalid json }");

			const result = await fromFile(testFilePath);

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("CONFIG_FILE_ERROR");
			}
		});

		it("should fail for non-object JSON", async () => {
			await fs.writeFile(
				testFilePath,
				JSON.stringify(["array", "not", "object"]),
			);

			const result = await fromFile(testFilePath);

			expect(isFailure(result)).toBe(true);
		});
	});

	describe("fromString", () => {
		it("should parse JSON string", () => {
			const json = JSON.stringify({ name: "test", port: 8080 });
			const result = fromString(json);

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.get("name")).toBe("test");
				expect(result.right.data.get("port")).toBe(8080);
			}
		});

		it("should fail for invalid JSON string", () => {
			const result = fromString("{ invalid json }");

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("CONFIG_PARSE_ERROR");
			}
		});

		it("should fail for non-object JSON string", () => {
			const result = fromString(JSON.stringify(["array"]));

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.code).toBe("CONFIG_PARSE_ERROR");
			}
		});

		it("should reject YAML format (not implemented)", () => {
			const result = fromString("name: test", "yaml");

			expect(isFailure(result)).toBe(true);
			if (result._tag === "Left") {
				expect(result.left.message).toContain("YAML support not implemented");
			}
		});
	});

	describe("fromEnvironment", () => {
		beforeEach(() => {
			// Clean up environment
			process.env.TEST_NAME = undefined;
			process.env.TEST_PORT = undefined;
			process.env.TEST_DEBUG = undefined;
		});

		it("should load from environment variables with prefix", () => {
			process.env.TEST_NAME = "myapp";
			process.env.TEST_PORT = "8080";
			process.env.TEST_DEBUG = "true";

			const result = fromEnvironment("TEST_");

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.get("NAME")).toBe("myapp");
				expect(result.right.data.get("PORT")).toBe(8080); // JSON parsed as number
				expect(result.right.data.get("DEBUG")).toBe(true); // JSON parsed as boolean
			}
		});

		it("should parse JSON values from environment", () => {
			process.env.TEST_CONFIG = JSON.stringify({ nested: true });

			const result = fromEnvironment("TEST_");

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				const config = result.right.data.get("CONFIG") as Record<
					string,
					unknown
				>;
				expect(config.nested).toBe(true);
			}
		});

		it("should work without prefix", () => {
			process.env.NAME = "test";

			const result = fromEnvironment();

			expect(isSuccess(result)).toBe(true);
			if (result._tag === "Right") {
				expect(result.right.data.get("NAME")).toBe("test");
			}
		});
	});

	describe("merge (monoid operations)", () => {
		it("should merge two configs with right bias", () => {
			const config1 = fromObject({ name: "app1", port: 8080, debug: true });
			const config2 = fromObject({ name: "app2", host: "localhost" });

			if (config1._tag === "Right" && config2._tag === "Right") {
				const merged = merge(config1.right, config2.right);

				expect(merged.data.get("name")).toBe("app2"); // Right bias
				expect(merged.data.get("port")).toBe(8080); // From left
				expect(merged.data.get("host")).toBe("localhost"); // From right
				expect(merged.data.get("debug")).toBe(true); // From left
			}
		});

		it("should deep merge nested objects", () => {
			const config1 = fromObject({
				server: { host: "localhost", port: 8080 },
			});
			const config2 = fromObject({
				server: { port: 3000, ssl: true },
			});

			if (config1._tag === "Right" && config2._tag === "Right") {
				const merged = merge(config1.right, config2.right);
				const server = merged.data.get("server") as Record<string, unknown>;

				expect(server.host).toBe("localhost"); // From left
				expect(server.port).toBe(3000); // Right bias
				expect(server.ssl).toBe(true); // From right
			}
		});

		it("should handle empty config as identity (left)", () => {
			const config = fromObject({ name: "test" });

			if (config._tag === "Right") {
				const merged = merge(empty, config.right);

				expect(merged.data.get("name")).toBe("test");
				expect(merged.data.size).toBe(1);
			}
		});

		it("should handle empty config as identity (right)", () => {
			const config = fromObject({ name: "test" });

			if (config._tag === "Right") {
				const merged = merge(config.right, empty);

				expect(merged.data.get("name")).toBe("test");
				expect(merged.data.size).toBe(1);
			}
		});

		it("should be associative", () => {
			const config1 = fromObject({ a: 1 });
			const config2 = fromObject({ b: 2 });
			const config3 = fromObject({ c: 3 });

			if (
				config1._tag === "Right" &&
				config2._tag === "Right" &&
				config3._tag === "Right"
			) {
				const left = merge(merge(config1.right, config2.right), config3.right);
				const right = merge(config1.right, merge(config2.right, config3.right));

				expect(left.data.get("a")).toBe(right.data.get("a"));
				expect(left.data.get("b")).toBe(right.data.get("b"));
				expect(left.data.get("c")).toBe(right.data.get("c"));
			}
		});
	});

	describe("mergeAll", () => {
		it("should merge multiple configs", () => {
			const configs = [
				fromObject({ a: 1, shared: "first" }),
				fromObject({ b: 2, shared: "second" }),
				fromObject({ c: 3, shared: "third" }),
			];

			if (configs.every((c) => c._tag === "Right")) {
				const configData = configs.map((c) =>
					c._tag === "Right" ? c.right : empty,
				);
				const merged = mergeAll(configData);

				expect(merged.data.get("a")).toBe(1);
				expect(merged.data.get("b")).toBe(2);
				expect(merged.data.get("c")).toBe(3);
				expect(merged.data.get("shared")).toBe("third"); // Last wins
			}
		});

		it("should handle empty array", () => {
			const merged = mergeAll([]);

			expect(merged.data.size).toBe(0);
			expect(merged).toEqual(empty);
		});
	});

	describe("get", () => {
		it("should get top-level value", () => {
			const config = fromObject({ name: "test", port: 8080 });

			if (config._tag === "Right") {
				const nameResult = get(config.right, "name");
				const portResult = get(config.right, "port");

				expect(isSuccess(nameResult)).toBe(true);
				expect(isSuccess(portResult)).toBe(true);

				if (nameResult._tag === "Right" && portResult._tag === "Right") {
					expect(nameResult.right).toBe("test");
					expect(portResult.right).toBe(8080);
				}
			}
		});

		it("should get nested value with dot notation", () => {
			const config = fromObject({
				server: { host: "localhost", port: 8080 },
			});

			if (config._tag === "Right") {
				const hostResult = get(config.right, "server.host");
				const portResult = get(config.right, "server.port");

				expect(isSuccess(hostResult)).toBe(true);
				expect(isSuccess(portResult)).toBe(true);

				if (hostResult._tag === "Right" && portResult._tag === "Right") {
					expect(hostResult.right).toBe("localhost");
					expect(portResult.right).toBe(8080);
				}
			}
		});

		it("should fail for non-existent key", () => {
			const config = fromObject({ name: "test" });

			if (config._tag === "Right") {
				const result = get(config.right, "nonexistent");

				expect(isFailure(result)).toBe(true);
				if (result._tag === "Left") {
					expect(result.left.code).toBe("CONFIG_KEY_NOT_FOUND");
				}
			}
		});

		it("should fail for non-existent nested key", () => {
			const config = fromObject({ server: {} });

			if (config._tag === "Right") {
				const result = get(config.right, "server.nonexistent");

				expect(isFailure(result)).toBe(true);
				if (result._tag === "Left") {
					expect(result.left.code).toBe("CONFIG_KEY_NOT_FOUND");
				}
			}
		});
	});

	describe("has", () => {
		it("should return true for existing keys", () => {
			const config = fromObject({
				name: "test",
				server: { host: "localhost" },
			});

			if (config._tag === "Right") {
				expect(has(config.right, "name")).toBe(true);
				expect(has(config.right, "server.host")).toBe(true);
			}
		});

		it("should return false for non-existent keys", () => {
			const config = fromObject({ name: "test" });

			if (config._tag === "Right") {
				expect(has(config.right, "nonexistent")).toBe(false);
				expect(has(config.right, "name.nested")).toBe(false);
			}
		});
	});

	describe("set", () => {
		it("should set top-level value", () => {
			const config = fromObject({ name: "test" });

			if (config._tag === "Right") {
				const updated = set(config.right, "port", 8080);

				expect(has(updated, "name")).toBe(true);
				expect(has(updated, "port")).toBe(true);

				const portResult = get(updated, "port");
				if (portResult._tag === "Right") {
					expect(portResult.right).toBe(8080);
				}
			}
		});

		it("should set nested value", () => {
			const config = fromObject({ server: {} });

			if (config._tag === "Right") {
				const updated = set(config.right, "server.host", "localhost");

				const hostResult = get(updated, "server.host");
				expect(isSuccess(hostResult)).toBe(true);
				if (hostResult._tag === "Right") {
					expect(hostResult.right).toBe("localhost");
				}
			}
		});

		it("should create nested structure if needed", () => {
			const config = fromObject({});

			if (config._tag === "Right") {
				const updated = set(config.right, "server.database.host", "localhost");

				const hostResult = get(updated, "server.database.host");
				expect(isSuccess(hostResult)).toBe(true);
				if (hostResult._tag === "Right") {
					expect(hostResult.right).toBe("localhost");
				}
			}
		});
	});

	describe("validation", () => {
		describe("validateRequired", () => {
			it("should pass when all required keys exist", () => {
				const config = fromObject({ name: "test", port: 8080 });

				if (config._tag === "Right") {
					const result = validateRequired(config.right, ["name", "port"]);

					expect(isSuccess(result)).toBe(true);
				}
			});

			it("should fail when required keys are missing", () => {
				const config = fromObject({ name: "test" });

				if (config._tag === "Right") {
					const result = validateRequired(config.right, [
						"name",
						"port",
						"host",
					]);

					expect(isFailure(result)).toBe(true);
					if (result._tag === "Left") {
						expect(result.left.code).toBe("CONFIG_VALIDATION_ERROR");
						expect(result.left.message).toContain("port");
						expect(result.left.message).toContain("host");
					}
				}
			});

			it("should validate nested keys", () => {
				const config = fromObject({ server: { host: "localhost" } });

				if (config._tag === "Right") {
					const result = validateRequired(config.right, [
						"server.host",
						"server.port",
					]);

					expect(isFailure(result)).toBe(true);
					if (result._tag === "Left") {
						expect(result.left.message).toContain("server.port");
					}
				}
			});
		});

		describe("validateType", () => {
			it("should pass when types match", () => {
				const config = fromObject({
					name: "test",
					port: 8080,
					enabled: true,
					tags: ["a", "b"],
					server: { host: "localhost" },
				});

				if (config._tag === "Right") {
					const result = validateType(config.right, {
						name: "string",
						port: "number",
						enabled: "boolean",
						tags: "array",
						server: "object",
					});

					expect(isSuccess(result)).toBe(true);
				}
			});

			it("should fail when types don't match", () => {
				const config = fromObject({ name: "test", port: "8080" });

				if (config._tag === "Right") {
					const result = validateType(config.right, {
						name: "string",
						port: "number",
					});

					expect(isFailure(result)).toBe(true);
					if (result._tag === "Left") {
						expect(result.left.code).toBe("CONFIG_TYPE_ERROR");
						expect(result.left.message).toContain(
							"port: expected number, got string",
						);
					}
				}
			});
		});

		describe("validateCustom", () => {
			it("should pass custom validation", () => {
				const config = fromObject({ port: 8080 });

				if (config._tag === "Right") {
					const validator = (cfg: typeof config.right) => {
						const portResult = get(cfg, "port");
						if (
							portResult._tag === "Right" &&
							typeof portResult.right === "number" &&
							portResult.right > 1024
						) {
							return success(cfg);
						}
						return failure(
							createQiError(
								"INVALID_PORT",
								"Port must be > 1024",
								ErrorCategory.VALIDATION,
							),
						);
					};

					const result = validateCustom(config.right, validator);

					expect(isSuccess(result)).toBe(true);
				}
			});
		});
	});
});
