/**
 * QiCore v4.0 - HTTP Client Component
 *
 * Mathematical Contract-Based TypeScript Library
 * Component 6: HTTPClient - HTTP client with circuit breaker (7 operations)
 */

import { QiError } from "../../base/error.js";
import { Result } from "../../base/result.js";

/**
 * HTTP method types
 */
export type HttpMethod = "GET" | "POST" | "PUT" | "DELETE" | "PATCH" | "HEAD" | "OPTIONS";

/**
 * HTTP request configuration
 */
export interface HttpRequest {
  url: string;
  method: HttpMethod;
  headers?: Record<string, string>;
  body?: unknown;
  timeout?: number;
  retries?: number;
}

/**
 * HTTP response
 */
export interface HttpResponse<T = unknown> {
  status: number;
  statusText: string;
  headers: Record<string, string>;
  data: T;
  url: string;
  duration: number;
}

/**
 * Circuit breaker state
 */
enum CircuitState {
  CLOSED = "closed",
  OPEN = "open",
  HALF_OPEN = "half-open",
}

/**
 * Circuit breaker configuration
 */
interface CircuitBreakerConfig {
  failureThreshold: number;
  recoveryTimeout: number;
  monitoringPeriod: number;
}

/**
 * HTTPClient provides robust HTTP communication with circuit breaker pattern,
 * retry logic, and comprehensive error handling
 */
export class HTTPClient {
  private circuitState = CircuitState.CLOSED;
  private failureCount = 0;
  private lastFailureTime = 0;
  private defaultTimeout = 30000; // 30 seconds
  private defaultRetries = 3;
  private circuitConfig: CircuitBreakerConfig = {
    failureThreshold: 5,
    recoveryTimeout: 60000, // 1 minute
    monitoringPeriod: 10000, // 10 seconds
  };

  constructor(
    private baseUrl = "",
    private defaultHeaders: Record<string, string> = {}
  ) {}

  /**
   * Operation 1: GET request
   */
  async get<T = unknown>(
    url: string,
    headers?: Record<string, string>
  ): Promise<Result<HttpResponse<T>>> {
    return this.request<T>({
      url,
      method: "GET",
      headers,
    });
  }

  /**
   * Operation 2: POST request
   */
  async post<T = unknown>(
    url: string,
    body?: unknown,
    headers?: Record<string, string>
  ): Promise<Result<HttpResponse<T>>> {
    return this.request<T>({
      url,
      method: "POST",
      body,
      headers,
    });
  }

  /**
   * Operation 3: PUT request
   */
  async put<T = unknown>(
    url: string,
    body?: unknown,
    headers?: Record<string, string>
  ): Promise<Result<HttpResponse<T>>> {
    return this.request<T>({
      url,
      method: "PUT",
      body,
      headers,
    });
  }

  /**
   * Operation 4: DELETE request
   */
  async delete<T = unknown>(
    url: string,
    headers?: Record<string, string>
  ): Promise<Result<HttpResponse<T>>> {
    return this.request<T>({
      url,
      method: "DELETE",
      headers,
    });
  }

  /**
   * Operation 5: PATCH request
   */
  async patch<T = unknown>(
    url: string,
    body?: unknown,
    headers?: Record<string, string>
  ): Promise<Result<HttpResponse<T>>> {
    return this.request<T>({
      url,
      method: "PATCH",
      body,
      headers,
    });
  }

  /**
   * Operation 6: Generic request with circuit breaker
   */
  async request<T = unknown>(config: HttpRequest): Promise<Result<HttpResponse<T>>> {
    // Check circuit breaker
    if (this.circuitState === CircuitState.OPEN) {
      if (Date.now() - this.lastFailureTime > this.circuitConfig.recoveryTimeout) {
        this.circuitState = CircuitState.HALF_OPEN;
      } else {
        return Result.failure(QiError.networkError("Circuit breaker is open", config.url));
      }
    }

    const retries = config.retries ?? this.defaultRetries;
    let lastError: QiError | null = null;

    for (let attempt = 0; attempt <= retries; attempt++) {
      const result = await this.performRequest<T>(config);

      if (result.isSuccess()) {
        this.onSuccess();
        return result;
      }

      lastError = result.error();

      // Don't retry on client errors (4xx)
      if (this.isClientError(lastError)) {
        this.onFailure();
        return result;
      }

      // Wait before retry (exponential backoff)
      if (attempt < retries) {
        await this.delay(2 ** attempt * 1000);
      }
    }

    this.onFailure();
    return Result.failure(lastError || ({ message: "All retry attempts failed" } as QiError));
  }

  /**
   * Operation 7: Configure client
   */
  configure(config: {
    baseUrl?: string;
    defaultHeaders?: Record<string, string>;
    timeout?: number;
    retries?: number;
    circuitBreaker?: Partial<CircuitBreakerConfig>;
  }): Result<void> {
    try {
      if (config.baseUrl !== undefined) {
        this.baseUrl = config.baseUrl;
      }
      if (config.defaultHeaders !== undefined) {
        this.defaultHeaders = { ...this.defaultHeaders, ...config.defaultHeaders };
      }
      if (config.timeout !== undefined) {
        this.defaultTimeout = config.timeout;
      }
      if (config.retries !== undefined) {
        this.defaultRetries = config.retries;
      }
      if (config.circuitBreaker !== undefined) {
        this.circuitConfig = { ...this.circuitConfig, ...config.circuitBreaker };
      }

      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.configurationError(
          `HTTP client configuration failed: ${error}`,
          "httpClient",
          "HttpClientConfig"
        )
      );
    }
  }

  /**
   * Get circuit breaker status
   */
  getCircuitStatus(): {
    state: CircuitState;
    failureCount: number;
    lastFailureTime: number;
  } {
    return {
      state: this.circuitState,
      failureCount: this.failureCount,
      lastFailureTime: this.lastFailureTime,
    };
  }

  /**
   * Reset circuit breaker
   */
  resetCircuit(): void {
    this.circuitState = CircuitState.CLOSED;
    this.failureCount = 0;
    this.lastFailureTime = 0;
  }

  /**
   * Perform the actual HTTP request
   */
  private async performRequest<T>(config: HttpRequest): Promise<Result<HttpResponse<T>>> {
    const startTime = Date.now();
    const url = this.buildUrl(config.url);
    const timeout = config.timeout ?? this.defaultTimeout;

    try {
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), timeout);

      const headers = {
        ...this.defaultHeaders,
        ...config.headers,
      };

      // Add Content-Type for requests with body
      if (config.body && !headers["Content-Type"]) {
        headers["Content-Type"] = "application/json";
      }

      const fetchConfig: RequestInit = {
        method: config.method,
        headers,
        signal: controller.signal,
      };

      if (config.body) {
        fetchConfig.body =
          typeof config.body === "string" ? config.body : JSON.stringify(config.body);
      }

      const response = await fetch(url, fetchConfig);
      clearTimeout(timeoutId);

      const duration = Date.now() - startTime;

      // Extract response headers
      const responseHeaders: Record<string, string> = {};
      response.headers.forEach((value, key) => {
        responseHeaders[key] = value;
      });

      // Parse response body
      let data: T;
      const contentType = response.headers.get("content-type");

      if (contentType?.includes("application/json")) {
        data = (await response.json()) as T;
      } else {
        data = (await response.text()) as T;
      }

      const httpResponse: HttpResponse<T> = {
        status: response.status,
        statusText: response.statusText,
        headers: responseHeaders,
        data,
        url,
        duration,
      };

      if (!response.ok) {
        return Result.failure(
          QiError.networkError(
            `HTTP ${response.status}: ${response.statusText}`,
            url,
            response.status
          )
        );
      }

      return Result.success(httpResponse);
    } catch (error) {
      // Duration could be logged here for monitoring: Date.now() - startTime

      if (error instanceof Error) {
        if (error.name === "AbortError") {
          return Result.failure(
            QiError.timeoutError(
              `Request timeout after ${timeout}ms`,
              "http_request",
              timeout / 1000
            )
          );
        }

        return Result.failure(QiError.networkError(`Request failed: ${error.message}`, url));
      }

      return Result.failure(QiError.networkError(`Unknown request error: ${error}`, url));
    }
  }

  /**
   * Build full URL
   */
  private buildUrl(path: string): string {
    if (path.startsWith("http://") || path.startsWith("https://")) {
      return path;
    }

    const base = this.baseUrl.endsWith("/") ? this.baseUrl.slice(0, -1) : this.baseUrl;
    const url = path.startsWith("/") ? path : `/${path}`;

    return `${base}${url}`;
  }

  /**
   * Handle successful request
   */
  private onSuccess(): void {
    this.failureCount = 0;
    if (this.circuitState === CircuitState.HALF_OPEN) {
      this.circuitState = CircuitState.CLOSED;
    }
  }

  /**
   * Handle failed request
   */
  private onFailure(): void {
    this.failureCount++;
    this.lastFailureTime = Date.now();

    if (this.failureCount >= this.circuitConfig.failureThreshold) {
      this.circuitState = CircuitState.OPEN;
    }
  }

  /**
   * Check if error is a client error (4xx)
   */
  private isClientError(error: QiError): boolean {
    if (error.category === "NetworkError" && error.context.statusCode) {
      const status = error.context.statusCode as number;
      return status >= 400 && status < 500;
    }
    return false;
  }

  /**
   * Delay helper for retries
   */
  private delay(ms: number): Promise<void> {
    return new Promise((resolve) => setTimeout(resolve, ms));
  }
}

/**
 * HTTP utility functions
 */
/**
 * Create HTTP client with base configuration
 */
export function createClient(baseUrl: string, headers?: Record<string, string>): HTTPClient {
  return new HTTPClient(baseUrl, headers);
}

/**
 * Parse URL query parameters
 */
export function parseQuery(url: string): Record<string, string> {
  const urlObj = new URL(url);
  const params: Record<string, string> = {};

  urlObj.searchParams.forEach((value, key) => {
    params[key] = value;
  });

  return params;
}

/**
 * Build URL with query parameters
 */
export function buildUrl(base: string, params?: Record<string, unknown>): string {
  if (!params || Object.keys(params).length === 0) {
    return base;
  }

  const url = new URL(base);

  for (const [key, value] of Object.entries(params)) {
    if (value !== undefined && value !== null) {
      url.searchParams.set(key, String(value));
    }
  }

  return url.toString();
}

/**
 * Check if response is successful
 */
export function isSuccess(response: HttpResponse): boolean {
  return response.status >= 200 && response.status < 300;
}

/**
 * Extract error message from response
 */
export function extractErrorMessage(response: HttpResponse): string {
  if (typeof response.data === "object" && response.data !== null) {
    const data = response.data as Record<string, unknown>;
    return (data.message as string) || (data.error as string) || response.statusText;
  }
  return response.statusText;
}
