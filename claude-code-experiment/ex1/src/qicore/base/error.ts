export enum ErrorCategory {
	VALIDATION = "VALIDATION",
	NETWORK = "NETWORK",
	FILESYSTEM = "FILESYSTEM",
	CONFIGURATION = "CONFIGURATION",
	CACHE = "CACHE",
	TIMEOUT = "TIMEOUT",
	PERMISSION = "PERMISSION",
	UNKNOWN = "UNKNOWN",
}

export interface QiError {
	readonly code: string;
	readonly message: string;
	readonly category: ErrorCategory;
	readonly context: Map<string, unknown>;
	readonly cause?: QiError;
	readonly timestamp: number;
}

export class QiErrorImpl implements QiError {
	public readonly code: string;
	public readonly message: string;
	public readonly category: ErrorCategory;
	public readonly context: Map<string, unknown>;
	public readonly cause?: QiError;
	public readonly timestamp: number;

	constructor(
		code: string,
		message: string,
		category: ErrorCategory,
		context?: Map<string, unknown> | Record<string, unknown>,
		cause?: QiError,
	) {
		if (!code || code.trim() === "") {
			throw new Error("Error code must be non-empty string");
		}

		this.code = code;
		this.message = message;
		this.category = category;
		this.context =
			context instanceof Map ? context : new Map(Object.entries(context || {}));
		this.cause = cause;
		this.timestamp = Date.now();

		// Prevent circular cause chains
		if (cause && this.getCauseChainDepth(cause) >= 10) {
			throw new Error("Cause chain depth exceeds maximum of 10");
		}
	}

	private getCauseChainDepth(error: QiError): number {
		let depth = 1;
		let current = error.cause;
		const seen = new Set<QiError>([this]);

		while (current && depth < 10) {
			if (seen.has(current)) {
				throw new Error("Circular cause chain detected");
			}
			seen.add(current);
			current = current.cause;
			depth++;
		}
		return depth;
	}

	toString(): string {
		const contextStr =
			this.context.size > 0
				? ` (${Array.from(this.context.entries())
						.map(([k, v]) => `${k}=${v}`)
						.join(", ")})`
				: "";

		const causeStr = this.cause ? ` [Caused by: ${this.cause.toString()}]` : "";

		return `[${this.category}] ${this.code}: ${this.message}${contextStr}${causeStr}`;
	}

	toStructuredData(): Record<string, unknown> {
		return {
			code: this.code,
			message: this.message,
			category: this.category,
			context: Object.fromEntries(this.context),
			cause: this.cause?.toStructuredData(),
			timestamp: this.timestamp,
		};
	}

	getCategory(): ErrorCategory {
		return this.category;
	}

	withContext(
		additionalContext: Map<string, unknown> | Record<string, unknown>,
	): QiError {
		const newContext = new Map(this.context);
		if (additionalContext instanceof Map) {
			for (const [key, value] of additionalContext) {
				newContext.set(key, value);
			}
		} else {
			for (const [key, value] of Object.entries(additionalContext)) {
				newContext.set(key, value);
			}
		}

		return new QiErrorImpl(
			this.code,
			this.message,
			this.category,
			newContext,
			this.cause,
		);
	}

	withCause(causeError: QiError): QiError {
		// Check for circular reference before creating
		let current = causeError;
		while (current) {
			if (current === this) {
				throw new Error("Circular cause chain detected");
			}
			current = current.cause;
		}

		return new QiErrorImpl(
			this.code,
			this.message,
			this.category,
			this.context,
			causeError,
		);
	}
}

export function createQiError(
	code: string,
	message: string,
	category: ErrorCategory,
	context?: Map<string, unknown> | Record<string, unknown>,
	cause?: QiError,
): QiError {
	return new QiErrorImpl(code, message, category, context, cause);
}
