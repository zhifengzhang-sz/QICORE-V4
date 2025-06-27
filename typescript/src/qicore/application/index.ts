/**
 * QiCore v4.0 - Application Components
 * 
 * Mathematical Contract-Based TypeScript Library
 * Application layer: High-level components for real-world applications
 */

export { 
  HTTPClient, 
  type HttpRequest,
  type HttpResponse,
  type HttpMethod 
} from "./http/client.js";
export * from "./http/client.js";

export { 
  Database, 
  DatabasePool,
  type DatabaseConfig,
  type QueryResult,
  type Transaction 
} from "./database/db.js";
export * from "./database/db.js";

// Note: Additional application components would be exported here
// - WebApplication (FastAPI equivalent)
// - CLIApplication (Click equivalent) 
// - AIClient (LLM integration)
// - DocumentGenerator (PDF/template generation)
// - ASGIServer (Uvicorn equivalent)
// - MCPClient (Model Context Protocol)