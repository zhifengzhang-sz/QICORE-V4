/**
 * QiCore v4.0 - Main Entry Point
 */

export * from "./qicore/index.js";
export * from "./qiagent/index.js";
export * from "./qiprompt/index.js";

// ============================================================================
// Version and Metadata
// ============================================================================

/**
 * QiCore version constant
 */
export const version = "4.0.1";

/**
 * Hello function returning greeting with version
 */
export const hello = (): string => `Hello from QiCore TypeScript v${version}!`;
