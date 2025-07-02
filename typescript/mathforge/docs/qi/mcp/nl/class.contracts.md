# QiMCP Class Contracts

> **Stage 1: Enhanced Property Specifications**  
> **Package**: `@modelcontextprotocol/sdk@1.13.1` (Official TypeScript SDK)  
> **Purpose**: Mathematical properties for MCP client class composition  
> **Audience**: AI mathematical verification and pattern discovery  

## Contract Philosophy

QiMCP provides **client-side classes** for PE/Agents to consume the massive ecosystem of existing MCP servers. We do **NOT** build servers - we connect to the hundreds of existing servers.

**Key Insight**: MCP has a thriving ecosystem with servers for every need:
- **Official servers**: Filesystem, GitHub, PostgreSQL, Slack, Memory, Time
- **Community servers**: Weather, Docker, MySQL, Stripe, YouTube, WhatsApp, Notion, Excel
- **Enterprise servers**: Cloudflare, MongoDB, Atlassian, PayPal

**Our Focus**: Build sophisticated MCP client classes that can discover, connect, and execute tools from any MCP server.

**Mathematical Foundation**: Each class implements algebraic properties that ensure composability, reliability, and ecosystem integration.

---

## MCP Client Class Architecture

### **Existing MCP Servers** (We consume these)
```
Available Servers:
├── Official Servers (by Anthropic/companies)
│   ├── @modelcontextprotocol/server-filesystem
│   ├── @modelcontextprotocol/server-github  
│   ├── @modelcontextprotocol/server-postgres
│   ├── @modelcontextprotocol/server-slack
│   └── @modelcontextprotocol/server-memory
├── Community Servers (hundreds available)
│   ├── Weather APIs
│   ├── Docker management
│   ├── Database connectors
│   ├── Social media APIs
│   └── Productivity tools
└── Enterprise Servers
    ├── Cloudflare (13 servers)
    ├── MongoDB MCP
    ├── Atlassian MCP
    └── PayPal MCP
```

### **QiMCP Client Classes** (What we build)
```
PE/Agent → MCPClientManager → [Existing MCP Servers]
         ↓
    MCPClientManager (orchestrator)
    ├── MCPServerRegistry (server discovery)
    ├── MCPConnection[] (multiple server connections)
    │   └── MCPTransport (transport abstraction)
    └── MCPToolRegistry (aggregated capabilities)
```

---

## QiMCP Client Classes

### MCPClientManager Class

**Purpose**: Orchestrate connections to multiple existing MCP servers for unified tool execution

**Class Interface**:
```typescript
class MCPClientManager {
  // Server discovery and configuration
  async discoverAvailableServers(): Promise<Result<MCPServerInfo[]>>;
  addServerConfig(name: string, config: MCPServerConfig): Result<void>;
  removeServerConfig(name: string): Result<boolean>;
  listConfiguredServers(): MCPServerInfo[];
  
  // Connection management
  async connectToServer(name: string): Promise<Result<MCPConnection>>;
  async disconnectFromServer(name: string): Promise<Result<void>>;
  async connectToAllServers(): Promise<Result<Map<string, MCPConnectionStatus>>>;
  getConnectionStatus(): Map<string, MCPConnectionStatus>;
  
  // Tool discovery across all connected servers
  async discoverAllTools(): Promise<Result<MCPToolRegistry>>;
  async discoverToolsFromServer(serverName: string): Promise<Result<MCPToolInfo[]>>;
  async findToolByName(toolName: string): Promise<Result<MCPToolLocation>>;
  async searchTools(query: string): Promise<Result<MCPToolInfo[]>>;
  
  // Unified tool execution
  async executeTool(toolName: string, args: Record<string, unknown>): Promise<Result<MCPToolResponse>>;
  async executeToolOnServer(serverName: string, toolName: string, args: Record<string, unknown>): Promise<Result<MCPToolResponse>>;
  async executeToolBatch(requests: MCPToolRequest[]): Promise<Result<MCPToolResponse[]>>;
  
  // Resource discovery and access
  async discoverAllResources(): Promise<Result<MCPResourceRegistry>>;
  async readResource(uri: string): Promise<Result<MCPResourceContent>>;
  async subscribeToResource(uri: string, handler: MCPResourceChangeHandler): Promise<Result<void>>;
  async unsubscribeFromResource(uri: string): Promise<Result<void>>;
  
  // Server ecosystem integration
  async installServer(serverPackage: string): Promise<Result<void>>;
  async updateServer(serverName: string): Promise<Result<void>>;
  async listAvailableServerPackages(): Promise<Result<MCPServerPackageInfo[]>>;
}
```

**Mathematical Properties**:

**Server Management Laws**:
```typescript
// Identity Law: Adding and removing same server returns to original state
∀ name, config: addServerConfig(name, config) → removeServerConfig(name) ≡ identity

// Idempotence Law: Adding same server config multiple times has same effect
∀ name, config: addServerConfig(name, config) → addServerConfig(name, config) ≡ addServerConfig(name, config)

// Connection Consistency: Connected servers must be in configured servers
∀ name: connectToServer(name).isOk() → name ∈ listConfiguredServers()

// Isolation Law: Server connection failures don't affect other connections
∀ name1, name2: connectToServer(name1).isError() → connectToServer(name2) independent
```

**Tool Discovery Laws**:
```typescript
// Aggregation Law: All tools equal union of server tools
discoverAllTools() ≡ ⋃(server ∈ connectedServers) discoverToolsFromServer(server)

// Search Consistency: Search results subset of all tools
∀ query: searchTools(query) ⊆ discoverAllTools()

// Tool Location Uniqueness: Each tool has unique location
∀ toolName: findToolByName(toolName) → unique(serverName, toolName)

// Discovery Monotonicity: Adding servers only increases available tools
∀ server: connectToServer(server) → |discoverAllTools()| ≥ |previous_tools|
```

**Execution Laws**:
```typescript
// Routing Determinism: Same tool call routes to same server
∀ toolName, args: executeTool(toolName, args) routes to same server consistently

// Batch Consistency: Batch execution equivalent to individual executions
executeToolBatch([req1, req2]) ≡ [executeTool(req1), executeTool(req2)]

// Error Isolation: Tool execution errors don't affect other tools
∀ tool1, tool2: executeTool(tool1).isError() → executeTool(tool2) independent

// Server Delegation: Execution delegated to appropriate server
∀ toolName, args: executeTool(toolName, args) ≡ executeToolOnServer(findServer(toolName), toolName, args)
```

**Resource Management Laws**:
```typescript
// Resource Aggregation: All resources equal union of server resources
discoverAllResources() ≡ ⋃(server ∈ connectedServers) server.listResources()

// URI Uniqueness: Each resource URI maps to unique server
∀ uri: readResource(uri) → unique(server) where server.hasResource(uri)

// Subscription Consistency: Subscriptions maintained across reconnections
∀ uri, handler: subscribeToResource(uri, handler) → maintained across reconnections

// Resource Caching: Resource content cached for performance
∀ uri: readResource(uri) → cached until invalidated
```

**Ecosystem Integration Laws**:
```typescript
// Package Management: Install/update operations are atomic
∀ package: installServer(package) → atomic(download, configure, validate)

// Version Consistency: Updates maintain compatibility
∀ server: updateServer(server) → maintains(API_compatibility)

// Discovery Freshness: Available packages reflect current ecosystem
listAvailableServerPackages() → reflects(current_ecosystem_state)

// Configuration Generation: Installed servers have valid configurations
∀ package: installServer(package).isOk() → hasValidConfig(package)
```

### MCPConnection Class

**Purpose**: Manage individual connection to a single existing MCP server

**Class Interface**:
```typescript
class MCPConnection {
  // Connection lifecycle
  constructor(config: MCPServerConfig);
  async connect(): Promise<Result<void>>;
  async disconnect(): Promise<Result<void>>;
  async reconnect(): Promise<Result<void>>;
  isConnected(): boolean;
  
  // Server capabilities discovery
  async getCapabilities(): Promise<Result<MCPCapabilities>>;
  async listTools(): Promise<Result<MCPToolInfo[]>>;
  async listResources(): Promise<Result<MCPResourceInfo[]>>;
  async listPrompts(): Promise<Result<MCPPromptInfo[]>>;
  
  // Tool operations
  async callTool(name: string, args: Record<string, unknown>): Promise<Result<MCPToolResponse>>;
  validateToolArgs(name: string, args: unknown): Result<Record<string, unknown>>;
  getToolSchema(name: string): Result<MCPToolSchema>;
  
  // Resource operations
  async readResource(uri: string): Promise<Result<MCPResourceContent>>;
  async subscribeToResource(uri: string): Promise<Result<void>>;
  async unsubscribeFromResource(uri: string): Promise<Result<void>>;
  
  // Prompt operations
  async getPrompt(name: string, args: Record<string, unknown>): Promise<Result<MCPPromptResponse>>;
  
  // Connection monitoring
  getServerInfo(): MCPServerInfo;
  getConnectionMetrics(): MCPConnectionMetrics;
  getSubscriptions(): string[];
}
```

**Mathematical Properties**:

**Connection State Laws**:
```typescript
// State Transitions: Connection follows valid state machine
connect() → isConnected() ≡ true
disconnect() → isConnected() ≡ false
reconnect() ≡ disconnect() → connect()

// Connection Idempotence: Multiple connects/disconnects are safe
∀ connection: connect() → connect() ≡ connect()
∀ connection: disconnect() → disconnect() ≡ disconnect()

// Capability Consistency: Capabilities stable while connected
∀ connection: isConnected() → getCapabilities() stable

// Reconnection Recovery: Reconnection restores full functionality
∀ connection: reconnect().isOk() → all_operations_available()
```

**Tool Operation Laws**:
```typescript
// Schema Validation: Tool args validate against schema
∀ name, args: validateToolArgs(name, args).isOk() → callTool(name, args) valid

// Tool Determinism: Same tool call produces consistent results
∀ name, args: callTool(name, args) deterministic for same server state

// Schema Consistency: Tool schemas remain stable
∀ name: getToolSchema(name) stable while connected

// Error Propagation: Tool errors properly wrapped in Result<T>
∀ name, args: callTool(name, args) → Result<MCPToolResponse>
```

**Resource Operation Laws**:
```typescript
// URI Validation: Resource URIs validate before operations
∀ uri: readResource(uri) → valid_uri(uri) ∨ Result.error()

// Subscription Persistence: Subscriptions persist until unsubscribed
∀ uri: subscribeToResource(uri) → uri ∈ getSubscriptions() until unsubscribeFromResource(uri)

// Resource Consistency: Resource content consistent within session
∀ uri: readResource(uri) consistent within connection session

// Subscription Cleanup: Disconnection cleans up subscriptions
disconnect() → getSubscriptions() ≡ []
```

**Monitoring Laws**:
```typescript
// Metrics Monotonicity: Connection metrics only increase
∀ t1, t2: t1 < t2 → getConnectionMetrics(t2) ≥ getConnectionMetrics(t1)

// Server Info Stability: Server info stable while connected
∀ connection: isConnected() → getServerInfo() stable

// Subscription Tracking: Subscriptions accurately tracked
∀ uri: subscribeToResource(uri).isOk() → uri ∈ getSubscriptions()
```

### MCPServerRegistry Class

**Purpose**: Discover and manage available MCP servers in the ecosystem

**Class Interface**:
```typescript
class MCPServerRegistry {
  // Server discovery
  async discoverOfficialServers(): Promise<Result<MCPServerPackageInfo[]>>;
  async discoverCommunityServers(): Promise<Result<MCPServerPackageInfo[]>>;
  async searchServers(query: string): Promise<Result<MCPServerPackageInfo[]>>;
  async getServerByName(name: string): Promise<Result<MCPServerPackageInfo>>;
  
  // Server installation
  async installServer(packageName: string): Promise<Result<MCPServerConfig>>;
  async uninstallServer(serverName: string): Promise<Result<void>>;
  async updateServer(serverName: string): Promise<Result<void>>;
  listInstalledServers(): MCPServerConfig[];
  
  // Server configuration
  createServerConfig(packageInfo: MCPServerPackageInfo, options?: MCPServerOptions): Result<MCPServerConfig>;
  validateServerConfig(config: MCPServerConfig): Result<void>;
  async getServerRequirements(packageName: string): Promise<Result<MCPServerRequirements>>;
  
  // Server ecosystem integration
  async checkForUpdates(): Promise<Result<MCPServerUpdateInfo[]>>;
  async getServerDocumentation(packageName: string): Promise<Result<string>>;
  async getServerExamples(packageName: string): Promise<Result<MCPServerExample[]>>;
}
```

**Mathematical Properties**:

**Discovery Laws**:
```typescript
// Discovery Completeness: All servers discoverable through some method
∀ server ∈ ecosystem: server ∈ (discoverOfficialServers() ∪ discoverCommunityServers())

// Search Consistency: Search results subset of discoverable servers
∀ query: searchServers(query) ⊆ (discoverOfficialServers() ∪ discoverCommunityServers())

// Name Resolution: Server names uniquely resolve to packages
∀ name: getServerByName(name) → unique(MCPServerPackageInfo)

// Discovery Freshness: Discovery reflects current ecosystem state
discoverOfficialServers() → reflects(current_official_ecosystem)
```

**Installation Laws**:
```typescript
// Installation Atomicity: Install operations are atomic
∀ package: installServer(package) → atomic(download, configure, validate)

// Installation Idempotence: Installing same package multiple times safe
∀ package: installServer(package) → installServer(package) ≡ installServer(package)

// Uninstall Cleanup: Uninstall removes all traces
∀ server: uninstallServer(server) → ¬∃ traces(server)

// Installation Consistency: Installed servers appear in list
∀ package: installServer(package).isOk() → package ∈ listInstalledServers()
```

**Configuration Laws**:
```typescript
// Configuration Validity: Generated configurations are valid
∀ packageInfo, options: createServerConfig(packageInfo, options) → validateServerConfig(result).isOk()

// Configuration Completeness: Configurations include all requirements
∀ config: validateServerConfig(config).isOk() → satisfies(getServerRequirements(config.package))

// Configuration Determinism: Same inputs produce same configuration
∀ packageInfo, options: createServerConfig(packageInfo, options) deterministic

// Requirements Consistency: Requirements stable for package versions
∀ package, version: getServerRequirements(package@version) stable
```

**Update Management Laws**:
```typescript
// Update Detection: Updates detected for outdated servers
∀ server: hasUpdate(server) → server ∈ checkForUpdates()

// Update Atomicity: Updates are atomic operations
∀ server: updateServer(server) → atomic(backup, update, validate, rollback_on_failure)

// Update Compatibility: Updates maintain compatibility
∀ server: updateServer(server) → maintains(API_compatibility)

// Documentation Consistency: Documentation matches server version
∀ package: getServerDocumentation(package) → matches(installed_version(package))
```

### MCPTransport Class

**Purpose**: Abstract transport layer for connecting to existing MCP servers

**Class Interface**:
```typescript
class MCPTransport {
  // Transport lifecycle
  constructor(config: MCPTransportConfig);
  async start(): Promise<Result<void>>;
  async stop(): Promise<Result<void>>;
  isActive(): boolean;
  
  // Message handling
  async sendMessage(message: MCPMessage): Promise<Result<MCPResponse>>;
  subscribeToMessages(handler: MCPMessageHandler): Result<void>;
  unsubscribeFromMessages(handler: MCPMessageHandler): Result<void>;
  
  // Transport monitoring
  getTransportInfo(): MCPTransportInfo;
  getTransportMetrics(): MCPTransportMetrics;
  
  // Connection management
  async reconnect(): Promise<Result<void>>;
  getConnectionState(): MCPConnectionState;
}
```

**Mathematical Properties**:

**Transport State Laws**:
```typescript
// State Machine: Transport follows valid state transitions
start() → isActive() ≡ true
stop() → isActive() ≡ false
¬isActive() → sendMessage() ≡ Result.error()

// Lifecycle Idempotence: Multiple start/stop operations safe
∀ transport: start() → start() ≡ start()
∀ transport: stop() → stop() ≡ stop()

// Activity Consistency: Active state reflects transport capability
isActive() ≡ can_send_messages()
```

**Message Handling Laws**:
```typescript
// Message Delivery: Messages delivered reliably when active
∀ message: isActive() → sendMessage(message) eventually_delivered

// Handler Registration: Handlers properly registered and called
∀ handler: subscribeToMessages(handler) → handler ∈ active_handlers
∀ handler: unsubscribeFromMessages(handler) → handler ∉ active_handlers

// Message Ordering: Messages delivered in order for same connection
∀ msg1, msg2: send_order(msg1, msg2) → delivery_order(msg1, msg2)

// Handler Isolation: Handler errors don't affect other handlers
∀ handler1, handler2: handler1.error() → handler2 unaffected
```

**Connection Management Laws**:
```typescript
// Reconnection Recovery: Reconnection restores full functionality
∀ transport: reconnect().isOk() → isActive() ∧ all_operations_available()

// State Consistency: Connection state reflects actual connection
getConnectionState() ≡ actual_connection_state()

// Metrics Monotonicity: Transport metrics only increase
∀ t1, t2: t1 < t2 → getTransportMetrics(t2) ≥ getTransportMetrics(t1)

// Transport Info Stability: Transport info stable while active
∀ transport: isActive() → getTransportInfo() stable
```

### MCPToolRegistry Class

**Purpose**: Aggregate and manage tools from multiple MCP servers

**Class Interface**:
```typescript
class MCPToolRegistry {
  // Tool registration from servers
  registerServerTools(serverName: string, tools: MCPToolInfo[]): Result<void>;
  unregisterServerTools(serverName: string): Result<void>;
  updateServerTools(serverName: string, tools: MCPToolInfo[]): Result<void>;
  
  // Tool discovery
  getAllTools(): MCPToolInfo[];
  getToolsByServer(serverName: string): MCPToolInfo[];
  findToolByName(toolName: string): Result<MCPToolLocation>;
  searchTools(query: string): MCPToolInfo[];
  
  // Tool metadata
  getToolSchema(toolName: string): Result<MCPToolSchema>;
  getToolDocumentation(toolName: string): Result<string>;
  getToolExamples(toolName: string): Result<MCPToolExample[]>;
  
  // Conflict resolution
  resolveToolConflicts(): Result<MCPConflictResolution[]>;
  setConflictResolutionStrategy(strategy: MCPConflictStrategy): Result<void>;
  getConflictedTools(): MCPToolConflict[];
}
```

**Mathematical Properties**:

**Tool Registration Laws**:
```typescript
// Registration Consistency: Registered tools appear in listings
∀ serverName, tools: registerServerTools(serverName, tools) → tools ⊆ getToolsByServer(serverName)

// Unregistration Cleanup: Unregistration removes all server tools
∀ serverName: unregisterServerTools(serverName) → getToolsByServer(serverName) ≡ []

// Update Atomicity: Tool updates are atomic
∀ serverName, tools: updateServerTools(serverName, tools) → atomic(remove_old, add_new)

// Registration Idempotence: Same registration has same effect
∀ serverName, tools: registerServerTools(serverName, tools) → registerServerTools(serverName, tools) ≡ identity
```

**Tool Discovery Laws**:
```typescript
// Aggregation Law: All tools equal union of server tools
getAllTools() ≡ ⋃(server ∈ registeredServers) getToolsByServer(server)

// Search Consistency: Search results subset of all tools
∀ query: searchTools(query) ⊆ getAllTools()

// Tool Location Uniqueness: Each tool has unique location
∀ toolName: findToolByName(toolName) → unique(serverName, toolName) ∨ conflict_resolution

// Server Isolation: Server tools properly isolated
∀ server1, server2: getToolsByServer(server1) ∩ getToolsByServer(server2) ≡ conflicts_only
```

**Conflict Resolution Laws**:
```typescript
// Conflict Detection: Conflicts detected when tools have same name
∀ tool1, tool2: tool1.name ≡ tool2.name ∧ tool1.server ≠ tool2.server → conflict(tool1, tool2)

// Resolution Consistency: Conflict resolution follows strategy
∀ conflict: resolveToolConflicts() → follows(conflict_resolution_strategy)

// Resolution Completeness: All conflicts have resolutions
∀ conflict ∈ getConflictedTools(): ∃ resolution ∈ resolveToolConflicts()

// Strategy Effectiveness: Resolution strategy eliminates conflicts
setConflictResolutionStrategy(strategy) → resolveToolConflicts() → getConflictedTools() ≡ []
```

---

## Cross-Class Composition Laws

### Client Architecture Laws

**Component Hierarchy**:
```typescript
// Hierarchical Composition: Manager orchestrates all components
MCPClientManager contains MCPServerRegistry ∧ MCPConnection[] ∧ MCPToolRegistry

// Connection Management: Manager manages connection lifecycle
∀ connection ∈ connections: MCPClientManager manages connection.lifecycle

// Registry Integration: Manager uses registry for server discovery
MCPClientManager.discoverServers() → MCPServerRegistry.discoverOfficialServers() ∪ MCPServerRegistry.discoverCommunityServers()

// Tool Aggregation: Manager aggregates tools from all connections
MCPClientManager.discoverAllTools() → MCPToolRegistry.aggregateFrom(all_connections)
```

### Error Propagation Laws

**Result Pattern Consistency**:
```typescript
// All operations return Result<T>
∀ operation: operation() → Result<T> ∨ T (for synchronous operations)

// Error chaining maintains context
∀ operation1, operation2: operation1().flatMap(operation2) maintains error_context

// Error isolation prevents cascading failures
∀ class1, class2: class1.error() → class2 unaffected

// Error recovery through Result pattern
∀ operation: operation().mapError(recovery_strategy) enables recovery
```

### Performance Laws

**Caching Consistency**:
```typescript
// Capability caching: Server capabilities cached until invalidated
∀ connection: connection.getCapabilities() cached until reconnection

// Tool schema caching: Tool schemas cached for performance
∀ toolName: getToolSchema(toolName) cached until server update

// Resource caching: Resource content cached with TTL
∀ uri: readResource(uri) cached with appropriate TTL

// Discovery caching: Server discovery results cached
discoverAvailableServers() cached with ecosystem refresh interval
```

**Connection Efficiency**:
```typescript
// Connection pooling: Connections reused efficiently
∀ server: multiple_requests(server) → reuse(connection)

// Batch operations: Multiple operations batched for efficiency
∀ requests: batch(requests) → single_transport_round_trip

// Lazy loading: Capabilities loaded on demand
∀ server: connect(server) → capabilities loaded on first access

// Resource cleanup: All resources properly cleaned up
∀ class: class.destroy() → all_resources_cleaned_up()
``` 