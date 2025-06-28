/**
 * QiCore v4.0 - TypeScript Implementation
 *
 * Mathematical Contract-Based TypeScript Library
 * Modern, High-Performance, Type-Safe
 */

// Base layer - Fundamental building blocks
export * from "./base/index.js";

// Core layer - Infrastructure components
export * from "./core/index.js";

// Application layer - High-level components
export * from "./application/index.js";

// Library metadata
export const version = "4.0.1";
export const author = "QiCore Team";
export const email = "team@qicore.dev";

/**
 * QiCore hello function for basic verification
 */
export function hello(): string {
  return "Hello from QiCore TypeScript v4.0.1!";
}
