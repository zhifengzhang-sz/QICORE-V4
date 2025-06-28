/**
 * QiCore v4.0 - QiError Tests
 */

import { describe, expect, test } from "vitest";
import { ErrorCollector, QiError, QiErrorGuards } from "../../src/qicore/base/index.js";

describe("QiError", () => {
  test("should create validation error", () => {
    const error = QiError.validationError("Invalid input", "username", "");

    expect(error.category).toBe("ValidationError");
    expect(error.message).toBe("Invalid input");
    expect(error.context.field).toBe("username");
    expect(error.context.value).toBe("");
    expect(error.timestamp).toBeDefined();
  });

  test("should create network error", () => {
    const error = QiError.networkError("Connection failed", "https://api.example.com", 500);

    expect(error.category).toBe("NetworkError");
    expect(error.message).toBe("Connection failed");
    expect(error.context.url).toBe("https://api.example.com");
    expect(error.context.statusCode).toBe(500);
  });

  test("should create timeout error", () => {
    const error = QiError.timeoutError("Request timeout", "api_call", 30);

    expect(error.category).toBe("TimeoutError");
    expect(error.message).toBe("Request timeout");
    expect(error.context.operation).toBe("api_call");
    expect(error.context.timeoutSeconds).toBe(30);
  });

  test("should create permission error", () => {
    const error = QiError.permissionError("Access denied", "/admin", "admin");

    expect(error.category).toBe("PermissionError");
    expect(error.message).toBe("Access denied");
    expect(error.context.resource).toBe("/admin");
    expect(error.context.requiredPermission).toBe("admin");
  });

  test("should create configuration error", () => {
    const error = QiError.configurationError("Invalid config", "port", "number");

    expect(error.category).toBe("ConfigurationError");
    expect(error.message).toBe("Invalid config");
    expect(error.context.key).toBe("port");
    expect(error.context.expectedType).toBe("number");
  });

  test("should create state error", () => {
    const error = QiError.stateError("Invalid state", "closed", "open");

    expect(error.category).toBe("StateError");
    expect(error.message).toBe("Invalid state");
    expect(error.context.currentState).toBe("closed");
    expect(error.context.expectedState).toBe("open");
  });

  test("should create resource error", () => {
    const error = QiError.resourceError("Resource not found", "file", "config.json");

    expect(error.category).toBe("ResourceError");
    expect(error.message).toBe("Resource not found");
    expect(error.context.resourceType).toBe("file");
    expect(error.context.resourceId).toBe("config.json");
  });

  test("should create integration error", () => {
    const error = QiError.integrationError("Service unavailable", "payment", "charge");

    expect(error.category).toBe("IntegrationError");
    expect(error.message).toBe("Service unavailable");
    expect(error.context.service).toBe("payment");
    expect(error.context.operation).toBe("charge");
  });

  test("should chain errors", () => {
    const cause = QiError.validationError("Input error", "field", "value");
    const error = QiError.networkError("Network error", "url");
    const chained = QiError.chain(error, cause);

    expect(chained.cause).toEqual(cause);
    expect(chained.category).toBe("NetworkError");
  });

  test("should format error messages", () => {
    const error = QiError.validationError("Invalid input", "username", "");
    const formatted = QiError.format(error);

    expect(formatted).toContain("[ValidationError] Invalid input");
    expect(formatted).toContain("Context:");
    expect(formatted).toContain("username");
  });

  test("should get error chain", () => {
    const error1 = QiError.validationError("Error 1", "field1", "value1");
    const error2 = QiError.chain(QiError.networkError("Error 2", "url"), error1);
    const error3 = QiError.chain(QiError.timeoutError("Error 3", "op", 30), error2);

    const chain = QiError.getChain(error3);

    expect(chain).toHaveLength(3);
    expect(chain[0].category).toBe("TimeoutError");
    expect(chain[1].category).toBe("NetworkError");
    expect(chain[2].category).toBe("ValidationError");
  });

  test("should check error categories", () => {
    const validationError = QiError.validationError("Error", "field", "value");
    const networkError = QiError.networkError("Error", "url");

    expect(QiError.isCategory(validationError, "ValidationError")).toBe(true);
    expect(QiError.isCategory(validationError, "NetworkError")).toBe(false);
    expect(QiError.isCategory(networkError, "NetworkError")).toBe(true);
  });

  test("should find error in chain", () => {
    const validation = QiError.validationError("Validation", "field", "value");
    const network = QiError.chain(QiError.networkError("Network", "url"), validation);

    const found = QiError.findInChain(network, "ValidationError");
    expect(found).toEqual(validation);

    const notFound = QiError.findInChain(network, "TimeoutError");
    expect(notFound).toBeUndefined();
  });

  test("should convert from/to Error", () => {
    const jsError = new Error("JavaScript error");
    const qiError = QiError.fromError(jsError);

    expect(qiError.category).toBe("RuntimeError");
    expect(qiError.message).toBe("JavaScript error");

    const backToError = QiError.toError(qiError);
    expect(backToError.message).toBe("JavaScript error");
    expect(backToError.name).toBe("RuntimeError");
  });
});

describe("QiErrorGuards", () => {
  test("should identify error types", () => {
    const validation = QiError.validationError("Error", "field", "value");
    const network = QiError.networkError("Error", "url");
    const timeout = QiError.timeoutError("Error", "op", 30);

    expect(QiErrorGuards.isValidationError(validation)).toBe(true);
    expect(QiErrorGuards.isValidationError(network)).toBe(false);

    expect(QiErrorGuards.isNetworkError(network)).toBe(true);
    expect(QiErrorGuards.isNetworkError(timeout)).toBe(false);

    expect(QiErrorGuards.isTimeoutError(timeout)).toBe(true);
    expect(QiErrorGuards.isTimeoutError(validation)).toBe(false);
  });
});

describe("ErrorCollector", () => {
  test("should collect errors", () => {
    const collector = new ErrorCollector();

    expect(collector.hasErrors()).toBe(false);

    collector.add(QiError.validationError("Error 1", "field1", "value1"));
    collector.addValidation("field2", "value2", "Error 2");

    expect(collector.hasErrors()).toBe(true);
    expect(collector.getErrors()).toHaveLength(2);
  });

  test("should get errors by category", () => {
    const collector = new ErrorCollector();

    collector.add(QiError.validationError("Error 1", "field1", "value1"));
    collector.add(QiError.networkError("Error 2", "url"));
    collector.add(QiError.validationError("Error 3", "field3", "value3"));

    const validationErrors = collector.getByCategory("ValidationError");
    expect(validationErrors).toHaveLength(2);

    const networkErrors = collector.getByCategory("NetworkError");
    expect(networkErrors).toHaveLength(1);
  });

  test("should combine errors", () => {
    const collector = new ErrorCollector();

    // No errors
    expect(collector.combine()).toBeUndefined();

    // Single error
    collector.add(QiError.validationError("Error 1", "field1", "value1"));
    const single = collector.combine();
    expect(single?.message).toBe("Error 1");

    // Multiple errors
    collector.add(QiError.networkError("Error 2", "url"));
    const combined = collector.combine();
    expect(combined?.message).toBe("Multiple errors occurred");
    expect(combined?.context.errorCount).toBe(2);
  });

  test("should clear errors", () => {
    const collector = new ErrorCollector();

    collector.add(QiError.validationError("Error", "field", "value"));
    expect(collector.hasErrors()).toBe(true);

    collector.clear();
    expect(collector.hasErrors()).toBe(false);
    expect(collector.getErrors()).toHaveLength(0);
  });
});
