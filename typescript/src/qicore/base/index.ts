/**
 * QiCore v4.0 - Base Components
 * 
 * Mathematical Contract-Based TypeScript Library
 * Base layer: Fundamental building blocks for functional programming
 */

export { Result } from "./result.js";
export { ErrorCollector } from "./error.js";
export * from "./error.js";

// Re-export commonly used types
export type { QiError as QiErrorType } from "./error.js";