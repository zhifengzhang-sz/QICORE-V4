/**
 * QiCore v4.0 - QiError Implementation
 * 
 * Mathematical Contract-Based TypeScript Library
 * Component 2: QiError - Structured error system with 8 categories (6 operations)
 */

/**
 * QiError represents a structured error with context and chaining support.
 * Provides 8 error categories for comprehensive error classification.
 */
export interface QiError {
  readonly category: string;
  readonly message: string;
  readonly context: Record<string, unknown>;
  readonly timestamp: number;
  readonly cause?: QiError;
  readonly stackTrace?: string;
}

/**
 * QiError namespace containing error creation and manipulation functions
 */
export namespace QiError {
  /**
   * Operation 1: Create validation error (Category 1)
   */
  export function validationError(
      message: string,
      field: string,
      value: unknown,
    ): QiError {
      return {
        category: "ValidationError",
        message,
        context: { field, value: String(value) },
        timestamp: Date.now(),
        stackTrace: new Error().stack,
      };
    }

  /**
   * Operation 2: Create network error (Category 2)
   */
  export function networkError(
      message: string,
      url: string,
      statusCode?: number,
    ): QiError {
      return {
        category: "NetworkError",
        message,
        context: { url, statusCode },
        timestamp: Date.now(),
      };
    }

  /**
   * Operation 3: Create timeout error (Category 3)
   */
  export function timeoutError(
      message: string,
      operation: string,
      timeoutSeconds: number,
    ): QiError {
      return {
        category: "TimeoutError",
        message,
        context: { operation, timeoutSeconds },
        timestamp: Date.now(),
      };
    }

  /**
   * Operation 4: Create permission error (Category 4)
   */
  export function permissionError(
      message: string,
      resource: string,
      requiredPermission: string,
    ): QiError {
      return {
        category: "PermissionError",
        message,
        context: { resource, requiredPermission },
        timestamp: Date.now(),
      };
    }

  /**
   * Operation 5: Create configuration error (Category 5)
   */
  export function configurationError(
      message: string,
      key: string,
      expectedType: string,
    ): QiError {
      return {
        category: "ConfigurationError",
        message,
        context: { key, expectedType },
        timestamp: Date.now(),
      };
    }

  /**
   * Category 6: Create state error
   */
  export function stateError(
      message: string,
      currentState: string,
      expectedState: string,
    ): QiError {
      return {
        category: "StateError",
        message,
        context: { currentState, expectedState },
        timestamp: Date.now(),
      };
    }

  /**
   * Category 7: Create resource error
   */
  export function resourceError(
      message: string,
      resourceType: string,
      resourceId: string,
    ): QiError {
      return {
        category: "ResourceError",
        message,
        context: { resourceType, resourceId },
        timestamp: Date.now(),
      };
    }

  /**
   * Category 8: Create integration error
   */
  export function integrationError(
      message: string,
      service: string,
      operation: string,
    ): QiError {
      return {
        category: "IntegrationError",
        message,
        context: { service, operation },
        timestamp: Date.now(),
      };
    }

  /**
   * Operation 6: Chain errors for better debugging
   */
  export function chain(error: QiError, cause: QiError): QiError {
      return {
        ...error,
        cause,
      };
    }

  /**
   * Format error for display
   */
  export function format(error: QiError): string {
      let result = `[${error.category}] ${error.message}`;
      
      if (Object.keys(error.context).length > 0) {
        result += `\\nContext: ${JSON.stringify(error.context, null, 2)}`;
      }
      
      if (error.cause) {
        result += `\\nCaused by: ${format(error.cause)}`;
      }
      
      return result;
    }

  /**
   * Get error chain as array
   */
  export function getChain(error: QiError): QiError[] {
      const chain: QiError[] = [error];
      let current = error.cause;
      
      while (current) {
        chain.push(current);
        current = current.cause;
      }
      
      return chain;
    }

  /**
   * Check if error is of specific category
   */
  export function isCategory(error: QiError, category: string): boolean {
      return error.category === category;
    }

  /**
   * Find first error in chain matching category
   */
  export function findInChain(error: QiError, category: string): QiError | undefined {
      const chain = getChain(error);
      return chain.find((e) => isCategory(e, category));
    }

  /**
   * Convert Error to QiError
   */
  export function fromError(error: Error): QiError {
      return {
        category: "RuntimeError",
        message: error.message,
        context: { name: error.name },
        timestamp: Date.now(),
        stackTrace: error.stack,
      };
    }

  /**
   * Convert QiError to Error
   */
  export function toError(qiError: QiError): Error {
      const error = new Error(qiError.message);
      error.name = qiError.category;
      error.stack = qiError.stackTrace;
      return error;
    }
}

/**
 * QiErrorGuards namespace containing type guard functions for error categories
 */
export namespace QiErrorGuards {
  export function isValidationError(error: QiError): boolean {
      return error.category === "ValidationError";
    }

  export function isNetworkError(error: QiError): boolean {
      return error.category === "NetworkError";
    }

  export function isTimeoutError(error: QiError): boolean {
      return error.category === "TimeoutError";
    }

  export function isPermissionError(error: QiError): boolean {
      return error.category === "PermissionError";
    }

  export function isConfigurationError(error: QiError): boolean {
      return error.category === "ConfigurationError";
    }

  export function isStateError(error: QiError): boolean {
      return error.category === "StateError";
    }

  export function isResourceError(error: QiError): boolean {
      return error.category === "ResourceError";
    }

  export function isIntegrationError(error: QiError): boolean {
      return error.category === "IntegrationError";
    }
}

/**
 * Error collection utilities
 */
export class ErrorCollector {
  private errors: QiError[] = [];

  add(error: QiError): void {
    this.errors.push(error);
  }

  addValidation(field: string, value: unknown, message: string): void {
    this.add(QiError.validationError(message, field, value));
  }

  hasErrors(): boolean {
    return this.errors.length > 0;
  }

  getErrors(): readonly QiError[] {
    return this.errors;
  }

  clear(): void {
    this.errors = [];
  }

  getByCategory(category: string): QiError[] {
    return this.errors.filter((error) => error.category === category);
  }

  getFirstError(): QiError | undefined {
    return this.errors[0];
  }

  /**
   * Combine all errors into a single error
   */
  combine(message = "Multiple errors occurred"): QiError | undefined {
    if (this.errors.length === 0) {
      return undefined;
    }

    if (this.errors.length === 1) {
      return this.errors[0];
    }

    return {
      category: "ValidationError",
      message,
      context: {
        errorCount: this.errors.length,
        categories: Array.from(new Set(this.errors.map((e) => e.category))),
      },
      timestamp: Date.now(),
      cause: this.errors[0],
    };
  }
}