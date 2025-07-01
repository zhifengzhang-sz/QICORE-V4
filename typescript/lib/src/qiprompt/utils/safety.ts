/**
 * @fileoverview QiPrompt v4.0 - Safety and Security Utilities
 * @purpose Prompt injection detection and content filtering
 * @dependencies QiCore Base
 * @version 4.0
 * @date December 30, 2025
 * @status Production-ready implementation
 */

import { createQiError } from "../../qicore/base/error";
import { type Result, failure, success } from "../../qicore/base/result";
import type { SafetyCheckResult, SafetyConfig } from "../core/types";

// ============================================================================
// PROMPT INJECTION DETECTION
// ============================================================================

/**
 * Advanced prompt injection patterns based on 2025 security research
 */
const INJECTION_PATTERNS = [
  // Direct instruction override
  /ignore\s+(?:all\s+)?(?:previous\s+)?instructions?/i,
  /forget\s+(?:all\s+)?(?:previous\s+)?instructions?/i,
  /disregard\s+(?:all\s+)?(?:previous\s+)?instructions?/i,

  // System message manipulation
  /system\s*:\s*you\s+are\s+now/i,
  /new\s+system\s+prompt/i,
  /override\s+system\s+settings/i,

  // Role switching attempts
  /you\s+are\s+no\s+longer/i,
  /pretend\s+to\s+be/i,
  /act\s+as\s+if\s+you\s+are/i,

  // Data extraction attempts
  /repeat\s+your\s+instructions/i,
  /show\s+me\s+your\s+prompt/i,
  /what\s+were\s+you\s+told/i,

  // Jailbreak attempts
  /developer\s+mode/i,
  /god\s+mode/i,
  /admin\s+mode/i,
  /debug\s+mode/i,

  // Encoding/obfuscation attempts
  /base64/i,
  /rot13/i,
  /unicode/i,
  /hex\s+encoded/i,
] as const;

/**
 * Content patterns that may indicate inappropriate use
 */
const CONTENT_RISK_PATTERNS = [
  // Personal information requests
  /(?:social\s+security|ssn|credit\s+card|password|api\s+key)/i,

  // Harmful content generation
  /(?:how\s+to\s+(?:make|create|build)\s+(?:bomb|weapon|drug))/i,

  // Manipulation attempts
  /(?:emotional\s+manipulation|psychological\s+pressure)/i,

  // Phishing indicators
  /(?:click\s+here|urgent\s+action|verify\s+account)/i,
] as const;

// ============================================================================
// SAFETY CHECKER CLASS
// ============================================================================

/**
 * Advanced safety checker with configurable policies
 */
export class PromptSafetyChecker {
  private readonly config: SafetyConfig;
  private readonly customPatterns: RegExp[];

  constructor(config: SafetyConfig, customPatterns: RegExp[] = []) {
    this.config = config;
    this.customPatterns = customPatterns;
  }

  /**
   * Perform comprehensive safety check on prompt content
   * @pure
   */
  check(content: string): Result<SafetyCheckResult> {
    try {
      const violations: Array<{
        readonly type: "content" | "injection" | "length" | "rate" | "domain";
        readonly severity: "low" | "medium" | "high" | "critical";
        readonly message: string;
        readonly suggestion?: string;
      }> = [];

      // Length validation
      if (content.length > this.config.maxPromptLength) {
        violations.push({
          type: "length",
          severity: "high",
          message: `Content exceeds maximum length of ${this.config.maxPromptLength} characters`,
          suggestion: "Break content into smaller parts or reduce verbosity",
        });
      }

      // Injection detection
      if (this.config.enableInjectionDetection) {
        const injectionResults = this.detectInjection(content);
        violations.push(...injectionResults);
      }

      // Content filtering
      if (this.config.enableContentFilter) {
        const contentResults = this.filterContent(content);
        violations.push(...contentResults);
      }

      // Custom pattern checks
      const customResults = this.checkCustomPatterns(content);
      violations.push(...customResults);

      // Calculate confidence score
      const confidence = this.calculateConfidence(violations, content);

      const result: SafetyCheckResult = {
        safe:
          violations.filter((v) => v.severity === "critical" || v.severity === "high").length === 0,
        violations: violations as readonly (typeof violations)[0][],
        confidence,
      };

      return success(result);
    } catch (error) {
      return failure(
        createQiError("SAFETY_CHECK_ERROR", `Safety check failed: ${error}`, "SYSTEM", {
          error: String(error),
        })
      );
    }
  }

  /**
   * Detect prompt injection attempts
   * @private
   */
  private detectInjection(content: string): SafetyCheckResult["violations"] {
    const violations: Array<{
      readonly type: "content" | "injection" | "length" | "rate" | "domain";
      readonly severity: "low" | "medium" | "high" | "critical";
      readonly message: string;
      readonly suggestion?: string;
    }> = [];

    for (const pattern of INJECTION_PATTERNS) {
      if (pattern.test(content)) {
        violations.push({
          type: "injection",
          severity: "critical",
          message: "Potential prompt injection detected",
          suggestion: "Review content for instruction override attempts",
        });
        break; // One injection detection is enough
      }
    }

    // Check for encoded content that might hide injections
    if (this.detectEncodedContent(content)) {
      violations.push({
        type: "injection",
        severity: "high",
        message: "Encoded content detected - potential obfuscation",
        suggestion: "Verify that encoded content is legitimate",
      });
    }

    return violations as readonly (typeof violations)[0][];
  }

  /**
   * Filter potentially harmful content
   * @private
   */
  private filterContent(content: string): SafetyCheckResult["violations"] {
    const violations: Array<{
      readonly type: "content" | "injection" | "length" | "rate" | "domain";
      readonly severity: "low" | "medium" | "high" | "critical";
      readonly message: string;
      readonly suggestion?: string;
    }> = [];

    // Check risk patterns
    for (const pattern of CONTENT_RISK_PATTERNS) {
      if (pattern.test(content)) {
        violations.push({
          type: "content",
          severity: "high",
          message: "Potentially harmful content detected",
          suggestion: "Review content for sensitive or inappropriate material",
        });
        break;
      }
    }

    // Check blocked terms
    if (this.config.blockedTerms) {
      for (const term of this.config.blockedTerms) {
        if (content.toLowerCase().includes(term.toLowerCase())) {
          violations.push({
            type: "content",
            severity: "medium",
            message: `Blocked term detected: ${term}`,
            suggestion: "Remove or replace the flagged content",
          });
        }
      }
    }

    // Check domain restrictions
    if (this.config.allowedDomains && this.config.allowedDomains.length > 0) {
      const urlPattern = /https?:\/\/([^/\s]+)/gi;
      const urls = content.match(urlPattern) || [];

      for (const url of urls) {
        const domain = new URL(url).hostname;
        if (!this.config.allowedDomains.includes(domain)) {
          violations.push({
            type: "domain",
            severity: "medium",
            message: `Disallowed domain detected: ${domain}`,
            suggestion: "Only use approved domains in prompts",
          });
        }
      }
    }

    return violations as readonly (typeof violations)[0][];
  }

  /**
   * Check custom security patterns
   * @private
   */
  private checkCustomPatterns(content: string): SafetyCheckResult["violations"] {
    const violations: Array<{
      readonly type: "content" | "injection" | "length" | "rate" | "domain";
      readonly severity: "low" | "medium" | "high" | "critical";
      readonly message: string;
      readonly suggestion?: string;
    }> = [];

    for (const pattern of this.customPatterns) {
      if (pattern.test(content)) {
        violations.push({
          type: "content",
          severity: "medium",
          message: "Custom security pattern matched",
          suggestion: "Review content against custom security policies",
        });
      }
    }

    return violations as readonly (typeof violations)[0][];
  }

  /**
   * Detect potentially encoded content
   * @private
   */
  private detectEncodedContent(content: string): boolean {
    // Base64 detection (common padding and character set)
    const base64Pattern = /[A-Za-z0-9+/]{20,}={0,2}/;

    // Hex encoding detection
    const hexPattern = /(?:0x)?[0-9a-fA-F]{16,}/;

    // Unicode escape sequences
    const unicodePattern = /\\u[0-9a-fA-F]{4}/;

    return base64Pattern.test(content) || hexPattern.test(content) || unicodePattern.test(content);
  }

  /**
   * Calculate confidence score for safety assessment
   * @private
   */
  private calculateConfidence(
    violations: SafetyCheckResult["violations"],
    content: string
  ): number {
    let confidence = 1.0;

    // Reduce confidence based on violations
    for (const violation of violations) {
      switch (violation.severity) {
        case "critical":
          confidence -= 0.5;
          break;
        case "high":
          confidence -= 0.3;
          break;
        case "medium":
          confidence -= 0.1;
          break;
        case "low":
          confidence -= 0.05;
          break;
      }
    }

    // Adjust for content characteristics
    if (content.length > this.config.maxPromptLength * 0.8) {
      confidence -= 0.1; // Long content is riskier
    }

    if (this.hasUnusualCharacters(content)) {
      confidence -= 0.1; // Unusual characters increase risk
    }

    return Math.max(0.0, Math.min(1.0, confidence));
  }

  /**
   * Check for unusual character patterns
   * @private
   */
  private hasUnusualCharacters(content: string): boolean {
    // High ratio of non-ASCII characters
    const nonAsciiCount = content.split("").filter((char) => char.charCodeAt(0) > 127).length;
    const nonAsciiRatio = nonAsciiCount / content.length;

    // High ratio of special characters
    const specialCharCount = content
      .split("")
      .filter((char) => /[^a-zA-Z0-9\s.,!?;:]/.test(char)).length;
    const specialCharRatio = specialCharCount / content.length;

    return nonAsciiRatio > 0.3 || specialCharRatio > 0.2;
  }
}

// ============================================================================
// UTILITY FUNCTIONS
// ============================================================================

/**
 * Quick safety check with default configuration
 * @pure
 */
export const quickSafetyCheck = (content: string): Result<boolean> => {
  const defaultConfig: SafetyConfig = {
    enableContentFilter: true,
    enableInjectionDetection: true,
    maxPromptLength: 50000,
    blockedTerms: ["password", "secret", "api_key"],
  };

  const checker = new PromptSafetyChecker(defaultConfig);
  const result = checker.check(content);

  if (result._tag === "Left") {
    return result;
  }

  return success(result.right.safe);
};

/**
 * Sanitize prompt content by removing potentially dangerous elements
 * @pure
 */
export const sanitizePrompt = (content: string): Result<string> => {
  try {
    let sanitized = content;

    // Remove potential injection patterns (be conservative)
    for (const pattern of INJECTION_PATTERNS) {
      sanitized = sanitized.replace(pattern, "[FILTERED]");
    }

    // Remove suspicious URLs
    const suspiciousUrlPattern =
      /https?:\/\/(?!(?:openai\.com|anthropic\.com|google\.com))[^\s]+/gi;
    sanitized = sanitized.replace(suspiciousUrlPattern, "[URL_FILTERED]");

    // Remove potential code execution attempts
    const codePattern = /```[\s\S]*?```/g;
    sanitized = sanitized.replace(codePattern, "[CODE_BLOCK_FILTERED]");

    // Limit consecutive special characters
    sanitized = sanitized.replace(/[^a-zA-Z0-9\s]{4,}/g, "[SPECIAL_CHARS_FILTERED]");

    return success(sanitized.trim());
  } catch (error) {
    return failure(
      createQiError("SANITIZATION_ERROR", `Failed to sanitize prompt: ${error}`, "SYSTEM", {
        error: String(error),
      })
    );
  }
};

/**
 * Rate limiting utility for prompt execution
 */
export class PromptRateLimiter {
  private readonly requests: Map<string, number[]>;
  private readonly config: NonNullable<SafetyConfig["rateLimit"]>;

  constructor(config: NonNullable<SafetyConfig["rateLimit"]>) {
    this.requests = new Map();
    this.config = config;
  }

  /**
   * Check if request is within rate limits
   */
  checkLimit(identifier: string): Result<boolean> {
    try {
      const now = Date.now();
      const windowStart = now - this.config.window;

      // Get existing requests for this identifier
      const existingRequests = this.requests.get(identifier) || [];

      // Filter out requests outside the current window
      const validRequests = existingRequests.filter((timestamp) => timestamp > windowStart);

      // Check if adding this request would exceed the limit
      if (validRequests.length >= this.config.requests) {
        return success(false);
      }

      // Add current request and update the map
      validRequests.push(now);
      this.requests.set(identifier, validRequests);

      return success(true);
    } catch (error) {
      return failure(
        createQiError("RATE_LIMIT_ERROR", `Rate limit check failed: ${error}`, "SYSTEM", {
          error: String(error),
          identifier,
        })
      );
    }
  }

  /**
   * Get remaining requests for identifier
   */
  getRemainingRequests(identifier: string): number {
    const now = Date.now();
    const windowStart = now - this.config.window;
    const existingRequests = this.requests.get(identifier) || [];
    const validRequests = existingRequests.filter((timestamp) => timestamp > windowStart);

    return Math.max(0, this.config.requests - validRequests.length);
  }

  /**
   * Clear old entries to prevent memory leaks
   */
  cleanup(): void {
    const now = Date.now();
    const cutoff = now - this.config.window * 2; // Keep some history

    for (const [identifier, requests] of this.requests.entries()) {
      const validRequests = requests.filter((timestamp) => timestamp > cutoff);
      if (validRequests.length === 0) {
        this.requests.delete(identifier);
      } else {
        this.requests.set(identifier, validRequests);
      }
    }
  }
}
