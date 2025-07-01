import { promises as fs } from "node:fs";
import { ErrorCategory, type QiError, createQiError } from "../base/error.js";
import {
	type Result,
	failure,
	fromAsyncTryCatch,
	success,
} from "../base/result.js";

export interface ConfigData {
	readonly data: Map<string, unknown>;
}

class ConfigDataImpl implements ConfigData {
	public readonly data: Map<string, unknown>;

	constructor(data: Map<string, unknown>) {
		this.data = new Map(data);
	}
}

// Monoid implementation for ConfigData
export const empty: ConfigData = new ConfigDataImpl(new Map());

// Right-biased merge operation (monoid)
export const merge = (a: ConfigData, b: ConfigData): ConfigData => {
	const merged = new Map(a.data);

	for (const [key, value] of b.data) {
		if (
			merged.has(key) &&
			typeof merged.get(key) === "object" &&
			typeof value === "object" &&
			merged.get(key) !== null &&
			value !== null &&
			!Array.isArray(merged.get(key)) &&
			!Array.isArray(value)
		) {
			// Deep merge objects
			const existingValue = merged.get(key) as Record<string, unknown>;
			const newValue = value as Record<string, unknown>;
			const subConfigA = fromObject(existingValue);
			const subConfigB = fromObject(newValue);

			if (subConfigA._tag === "Right" && subConfigB._tag === "Right") {
				const mergedSub = merge(subConfigA.right, subConfigB.right);
				merged.set(key, Object.fromEntries(mergedSub.data));
			} else {
				// If can't parse as configs, right-bias wins
				merged.set(key, value);
			}
		} else {
			// Right-bias: b overwrites a
			merged.set(key, value);
		}
	}

	return new ConfigDataImpl(merged);
};

// Merge multiple configs (associative operation)
export const mergeAll = (configs: ConfigData[]): ConfigData => {
	return configs.reduce(merge, empty);
};

export const fromObject = (
	obj: Record<string, unknown>,
): Result<ConfigData> => {
	try {
		const data = new Map(Object.entries(obj));
		return success(new ConfigDataImpl(data));
	} catch (error) {
		return failure(
			createQiError(
				"CONFIG_PARSE_ERROR",
				`Failed to parse object as configuration: ${error}`,
				ErrorCategory.CONFIGURATION,
				{ originalError: error },
			),
		);
	}
};

export const fromFile = async (
	filePath: string,
): Promise<Result<ConfigData>> => {
	return fromAsyncTryCatch(
		async () => {
			const content = await fs.readFile(filePath, "utf-8");
			let parsed: unknown;

			if (filePath.endsWith(".json")) {
				parsed = JSON.parse(content);
			} else if (filePath.endsWith(".yaml") || filePath.endsWith(".yml")) {
				// For now, just support JSON. YAML would require additional dependency
				throw new Error("YAML support not implemented - use JSON format");
			} else {
				// Try to parse as JSON by default
				parsed = JSON.parse(content);
			}

			if (
				typeof parsed !== "object" ||
				parsed === null ||
				Array.isArray(parsed)
			) {
				throw new Error("Configuration file must contain an object");
			}

			const result = fromObject(parsed as Record<string, unknown>);
			if (result._tag === "Left") {
				throw new Error(result.left.message);
			}
			return result.right;
		},
		(error) =>
			createQiError(
				"CONFIG_FILE_ERROR",
				`Failed to load configuration from file: ${filePath}`,
				ErrorCategory.FILESYSTEM,
				{ filePath, originalError: error },
			),
	);
};

export const fromString = (
	content: string,
	format: "json" | "yaml" = "json",
): Result<ConfigData> => {
	try {
		let parsed: unknown;

		if (format === "json") {
			parsed = JSON.parse(content);
		} else {
			throw new Error("YAML support not implemented - use JSON format");
		}

		if (
			typeof parsed !== "object" ||
			parsed === null ||
			Array.isArray(parsed)
		) {
			return failure(
				createQiError(
					"CONFIG_PARSE_ERROR",
					"Configuration string must contain an object",
					ErrorCategory.CONFIGURATION,
				),
			);
		}

		return fromObject(parsed as Record<string, unknown>);
	} catch (error) {
		return failure(
			createQiError(
				"CONFIG_PARSE_ERROR",
				`Failed to parse configuration string: ${error}`,
				ErrorCategory.CONFIGURATION,
				{ format, originalError: error },
			),
		);
	}
};

export const fromEnvironment = (prefix = ""): Result<ConfigData> => {
	try {
		const config: Record<string, unknown> = {};

		for (const [key, value] of Object.entries(process.env)) {
			if (key.startsWith(prefix)) {
				const configKey = prefix ? key.slice(prefix.length) : key;

				// Try to parse as JSON first, fall back to string
				try {
					const parsed = JSON.parse(value || "");
					config[configKey] = parsed;
				} catch {
					// Fall back to string value
					config[configKey] = value;
				}
			}
		}

		return fromObject(config);
	} catch (error) {
		return failure(
			createQiError(
				"CONFIG_ENV_ERROR",
				`Failed to load configuration from environment: ${error}`,
				ErrorCategory.CONFIGURATION,
				{ prefix, originalError: error },
			),
		);
	}
};

export const get = <T = unknown>(
	config: ConfigData,
	key: string,
): Result<T> => {
	const keys = key.split(".");
	let current: unknown = Object.fromEntries(config.data);

	for (const k of keys) {
		if (
			typeof current !== "object" ||
			current === null ||
			Array.isArray(current)
		) {
			return failure(
				createQiError(
					"CONFIG_KEY_NOT_FOUND",
					`Configuration key not found: ${key}`,
					ErrorCategory.CONFIGURATION,
					{ key, path: keys },
				),
			);
		}

		const obj = current as Record<string, unknown>;
		if (!(k in obj)) {
			return failure(
				createQiError(
					"CONFIG_KEY_NOT_FOUND",
					`Configuration key not found: ${key}`,
					ErrorCategory.CONFIGURATION,
					{ key, path: keys },
				),
			);
		}

		current = obj[k];
	}

	return success(current as T);
};

export const has = (config: ConfigData, key: string): boolean => {
	const result = get(config, key);
	return result._tag === "Right";
};

export const set = (
	config: ConfigData,
	key: string,
	value: unknown,
): ConfigData => {
	const keys = key.split(".");
	const obj = Object.fromEntries(config.data);

	let current = obj;
	for (let i = 0; i < keys.length - 1; i++) {
		const k = keys[i];
		if (
			!(k in current) ||
			typeof current[k] !== "object" ||
			current[k] === null ||
			Array.isArray(current[k])
		) {
			current[k] = {};
		}
		current = current[k] as Record<string, unknown>;
	}

	current[keys[keys.length - 1]] = value;

	const result = fromObject(obj);
	return result._tag === "Right" ? result.right : config;
};

// Validation helpers
export const validateRequired = (
	config: ConfigData,
	requiredKeys: string[],
): Result<ConfigData> => {
	const missing: string[] = [];

	for (const key of requiredKeys) {
		if (!has(config, key)) {
			missing.push(key);
		}
	}

	if (missing.length > 0) {
		return failure(
			createQiError(
				"CONFIG_VALIDATION_ERROR",
				`Missing required configuration keys: ${missing.join(", ")}`,
				ErrorCategory.VALIDATION,
				{ missingKeys: missing },
			),
		);
	}

	return success(config);
};

export const validateType = (
	config: ConfigData,
	typeRules: Record<
		string,
		"string" | "number" | "boolean" | "object" | "array"
	>,
): Result<ConfigData> => {
	const errors: string[] = [];

	for (const [key, expectedType] of Object.entries(typeRules)) {
		const valueResult = get(config, key);
		if (valueResult._tag === "Right") {
			const value = valueResult.right;
			let actualType = typeof value;

			if (actualType === "object" && Array.isArray(value)) {
				actualType = "array";
			}

			if (actualType !== expectedType) {
				errors.push(`${key}: expected ${expectedType}, got ${actualType}`);
			}
		}
	}

	if (errors.length > 0) {
		return failure(
			createQiError(
				"CONFIG_TYPE_ERROR",
				`Configuration type validation failed: ${errors.join(", ")}`,
				ErrorCategory.VALIDATION,
				{ errors },
			),
		);
	}

	return success(config);
};

export const validateCustom = (
	config: ConfigData,
	validator: (config: ConfigData) => Result<ConfigData>,
): Result<ConfigData> => {
	return validator(config);
};
