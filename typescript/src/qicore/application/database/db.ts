/**
 * QiCore v4.0 - Database Component
 *
 * Mathematical Contract-Based TypeScript Library
 * Component 12: Database - Database operations with transactions (5 operations)
 */

import { QiError } from "../../base/error.js";
import { Result } from "../../base/result.js";

/**
 * Database query result
 */
export interface QueryResult<T = unknown> {
  rows: T[];
  rowCount: number;
  duration: number;
  query: string;
}

/**
 * Database configuration
 */
export interface DatabaseConfig {
  type: "sqlite" | "memory";
  path?: string;
  poolSize?: number;
  timeout?: number;
}

/**
 * Transaction interface
 */
export interface Transaction {
  query<T = unknown>(sql: string, params?: unknown[]): Promise<Result<QueryResult<T>>>;
  commit(): Promise<Result<void>>;
  rollback(): Promise<Result<void>>;
}

/**
 * Simple in-memory database implementation for demonstration
 * In production, you would integrate with a real database driver
 */
export class Database {
  private tables = new Map<string, unknown[]>();
  private isConnected = false;
  private config: DatabaseConfig;

  constructor(config: DatabaseConfig) {
    this.config = config;
  }

  /**
   * Operation 1: Connect to database
   */
  async connect(): Promise<Result<void>> {
    try {
      // Simulate connection logic
      if (this.config.type === "memory") {
        this.isConnected = true;
        return Result.success(undefined);
      }
      if (this.config.type === "sqlite") {
        // In real implementation, use Bun's built-in SQLite or a driver
        this.isConnected = true;
        return Result.success(undefined);
      }
      return Result.failure(
        QiError.configurationError(
          `Unsupported database type: ${this.config.type}`,
          "type",
          "sqlite | memory"
        )
      );
    } catch (error) {
      return Result.failure(
        QiError.resourceError(
          `Database connection failed: ${error}`,
          "database",
          this.config.path || "memory"
        )
      );
    }
  }

  /**
   * Operation 2: Execute query
   */
  async query<T = unknown>(sql: string, params?: unknown[]): Promise<Result<QueryResult<T>>> {
    if (!this.isConnected) {
      return Result.failure(
        QiError.stateError("Database not connected", "disconnected", "connected")
      );
    }

    const startTime = Date.now();

    try {
      // Simple SQL parsing for demo (in real implementation, use proper SQL driver)
      const result = await this.executeQuery<T>(sql, params);
      const duration = Date.now() - startTime;

      return Result.success({
        rows: result,
        rowCount: result.length,
        duration,
        query: sql,
      });
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Query execution failed: ${error}`, "query", sql)
      );
    }
  }

  /**
   * Operation 3: Start transaction
   */
  async transaction(): Promise<Result<Transaction>> {
    if (!this.isConnected) {
      return Result.failure(
        QiError.stateError("Database not connected", "disconnected", "connected")
      );
    }

    try {
      const txn = new DatabaseTransaction(this);
      return Result.success(txn);
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Transaction start failed: ${error}`, "transaction", "start")
      );
    }
  }

  /**
   * Operation 4: Close connection
   */
  async close(): Promise<Result<void>> {
    try {
      this.isConnected = false;
      this.tables.clear();
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.resourceError(
          `Database close failed: ${error}`,
          "database",
          this.config.path || "memory"
        )
      );
    }
  }

  /**
   * Operation 5: Check health
   */
  async healthCheck(): Promise<Result<{ status: string; duration: number }>> {
    const startTime = Date.now();

    try {
      if (!this.isConnected) {
        return Result.failure(
          QiError.stateError("Database not connected", "disconnected", "connected")
        );
      }

      // Simple health check query
      const result = await this.query("SELECT 1 as health");
      if (result.isFailure()) {
        return Result.failure(result.error());
      }

      const duration = Date.now() - startTime;
      return Result.success({
        status: "healthy",
        duration,
      });
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Health check failed: ${error}`, "database", "health")
      );
    }
  }

  /**
   * Get connection status
   */
  isReady(): boolean {
    return this.isConnected;
  }

  /**
   * Execute query implementation (simplified for demo)
   */
  private async executeQuery<T>(sql: string, params?: unknown[]): Promise<T[]> {
    // Simple SQL parsing for demo purposes
    const normalizedSql = sql.trim().toLowerCase();

    if (normalizedSql.startsWith("select")) {
      return this.handleSelect<T>(sql);
    }
    if (normalizedSql.startsWith("insert")) {
      return this.handleInsert<T>(sql, params);
    }
    if (normalizedSql.startsWith("update")) {
      return this.handleUpdate<T>(sql, params);
    }
    if (normalizedSql.startsWith("delete")) {
      return this.handleDelete<T>(sql);
    }
    if (normalizedSql.startsWith("create")) {
      return this.handleCreate<T>(sql);
    }
    throw new Error(`Unsupported SQL operation: ${sql}`);
  }

  /**
   * Handle SELECT queries
   */
  private handleSelect<T>(sql: string): T[] {
    if (sql.includes("1 as health")) {
      return [{ health: 1 } as T];
    }

    // Extract table name (very basic parsing)
    const match = sql.match(/from\\s+(\\w+)/i);
    if (!match) {
      return [];
    }

    const tableName = match[1];
    const table = this.tables.get(tableName) || [];
    return table as T[];
  }

  /**
   * Handle INSERT queries
   */
  private handleInsert<T>(sql: string, params?: unknown[]): T[] {
    const match = sql.match(/insert\\s+into\\s+(\\w+)/i);
    if (!match) {
      throw new Error("Invalid INSERT syntax");
    }

    const tableName = match[1];
    if (!this.tables.has(tableName)) {
      this.tables.set(tableName, []);
    }

    const table = this.tables.get(tableName);
    if (!table) {
      return Result.failure({ message: `Table ${tableName} not found` } as QiError);
    }

    // Simplified: just add the params as a record
    if (params && params.length > 0) {
      table.push(params[0]);
    }

    return [{ insertId: table.length, affectedRows: 1 } as T];
  }

  /**
   * Handle UPDATE queries
   */
  private handleUpdate<T>(_sql: string, _params?: unknown[]): T[] {
    return [{ affectedRows: 1 } as T];
  }

  /**
   * Handle DELETE queries
   */
  private handleDelete<T>(_sql: string): T[] {
    return [{ affectedRows: 1 } as T];
  }

  /**
   * Handle CREATE queries
   */
  private handleCreate<T>(sql: string): T[] {
    const match = sql.match(/create\\s+table\\s+(\\w+)/i);
    if (match) {
      const tableName = match[1];
      this.tables.set(tableName, []);
    }
    return [{ success: true } as T];
  }
}

/**
 * Database transaction implementation
 */
class DatabaseTransaction implements Transaction {
  private operations: Array<{ sql: string; params?: unknown[] }> = [];
  private committed = false;
  private rolledBack = false;

  constructor(private db: Database) {}

  async query<T = unknown>(sql: string, params?: unknown[]): Promise<Result<QueryResult<T>>> {
    if (this.committed || this.rolledBack) {
      return Result.failure(
        QiError.stateError("Transaction already completed", "completed", "active")
      );
    }

    // Store operation for potential rollback
    this.operations.push({ sql, params });

    // Execute query within transaction context
    return this.db.query<T>(sql, params);
  }

  async commit(): Promise<Result<void>> {
    if (this.committed || this.rolledBack) {
      return Result.failure(
        QiError.stateError("Transaction already completed", "completed", "active")
      );
    }

    try {
      // In real implementation, commit to database
      this.committed = true;
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Transaction commit failed: ${error}`, "transaction", "commit")
      );
    }
  }

  async rollback(): Promise<Result<void>> {
    if (this.committed || this.rolledBack) {
      return Result.failure(
        QiError.stateError("Transaction already completed", "completed", "active")
      );
    }

    try {
      // In real implementation, rollback database changes
      this.rolledBack = true;
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Transaction rollback failed: ${error}`, "transaction", "rollback")
      );
    }
  }
}

/**
 * Database connection pool
 */
export class DatabasePool {
  private connections: Database[] = [];
  private available: Database[] = [];
  private config: DatabaseConfig;

  constructor(config: DatabaseConfig & { poolSize: number }) {
    this.config = config;
  }

  async initialize(): Promise<Result<void>> {
    try {
      for (let i = 0; i < (this.config.poolSize || 5); i++) {
        const db = new Database(this.config);
        const connectResult = await db.connect();

        if (connectResult.isFailure()) {
          return connectResult;
        }

        this.connections.push(db);
        this.available.push(db);
      }

      return Result.success(undefined);
    } catch (error) {
      return Result.failure(
        QiError.resourceError(`Pool initialization failed: ${error}`, "pool", "initialize")
      );
    }
  }

  async getConnection(): Promise<Result<Database>> {
    if (this.available.length === 0) {
      return Result.failure(
        QiError.resourceError("No available connections in pool", "pool", "connection")
      );
    }

    const connection = this.available.pop();
    if (!connection) {
      return Result.failure(
        QiError.resourceError("Failed to get connection from pool", "pool", "connection")
      );
    }
    return Result.success(connection);
  }

  releaseConnection(connection: Database): void {
    if (this.connections.includes(connection)) {
      this.available.push(connection);
    }
  }

  async close(): Promise<Result<void>> {
    try {
      for (const connection of this.connections) {
        await connection.close();
      }
      this.connections = [];
      this.available = [];
      return Result.success(undefined);
    } catch (error) {
      return Result.failure(QiError.resourceError(`Pool close failed: ${error}`, "pool", "close"));
    }
  }

  getStats(): {
    total: number;
    available: number;
    active: number;
  } {
    return {
      total: this.connections.length,
      available: this.available.length,
      active: this.connections.length - this.available.length,
    };
  }
}

/**
 * Database utilities
 */
/**
 * Create database connection
 */
export function createDatabase(config: DatabaseConfig): Database {
  return new Database(config);
}

/**
 * Create database pool
 */
export function createPool(config: DatabaseConfig & { poolSize: number }): DatabasePool {
  return new DatabasePool(config);
}

/**
 * Execute multiple queries in transaction
 */
export async function withTransaction<T>(
  db: Database,
  operations: (txn: Transaction) => Promise<Result<T>>
): Promise<Result<T>> {
  const txnResult = await db.transaction();
  if (txnResult.isFailure()) {
    return txnResult as Result<T>;
  }

  const txn = txnResult.unwrap();

  try {
    const result = await operations(txn);

    if (result.isSuccess()) {
      const commitResult = await txn.commit();
      if (commitResult.isFailure()) {
        return commitResult as Result<T>;
      }
      return result;
    }
    await txn.rollback();
    return result;
  } catch (error) {
    await txn.rollback();
    return Result.failure(
      QiError.resourceError(`Transaction failed: ${error}`, "transaction", "execute")
    );
  }
}

/**
 * Escape SQL string (basic implementation)
 */
export function escapeSql(value: string): string {
  return value.replace(/'/g, "''");
}

/**
 * Build parameterized query
 */
export function buildQuery(
  template: string,
  params: Record<string, unknown>
): {
  sql: string;
  values: unknown[];
} {
  const values: unknown[] = [];
  let paramIndex = 0;

  const sql = template.replace(/:(\w+)/g, (match, paramName) => {
    if (paramName in params) {
      values.push(params[paramName]);
      return `$${++paramIndex}`;
    }
    return match;
  });

  return { sql, values };
}
