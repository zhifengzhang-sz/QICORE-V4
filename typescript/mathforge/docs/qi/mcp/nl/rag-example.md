# RAG with MCP: Concrete Example

> **Real-world example of using existing RAG MCP servers with QiMCP client**

## The Problem: You Want RAG Ability

You want your PE/Agent to have RAG (Retrieval-Augmented Generation) capabilities:
- **Semantic search** over documents
- **Vector storage** and retrieval  
- **Context injection** for LLM prompts

## The Solution: Existing RAG MCP Servers

**You DON'T build a RAG server** - you use existing ones!

### **Available RAG MCP Servers**:

#### **1. Official Vector Database Servers**:
```bash
# Qdrant (Official)
uvx mcp-server-qdrant

# Memory (Official by Anthropic)  
npx @modelcontextprotocol/server-memory

# Chroma (Community)
npx mcp-server-chroma

# Pinecone (Community)
npx mcp-server-pinecone
```

#### **2. Specialized RAG Servers**:
```bash
# RAG with FAISS
uvx mcp-server-rag

# Needle (Production RAG)
npx mcp-server-needle

# Vectorize (Advanced RAG)
npx mcp-server-vectorize

# Local RAG (No APIs)
npx mcp-local-rag
```

## Concrete Example: Using Qdrant MCP Server

### **Step 1: Install & Configure Qdrant MCP Server**

```bash
# Install Qdrant server locally (or use cloud)
docker run -p 6333:6333 qdrant/qdrant

# Run Qdrant MCP Server
QDRANT_URL="http://localhost:6333" \
COLLECTION_NAME="my-documents" \
EMBEDDING_MODEL="sentence-transformers/all-MiniLM-L6-v2" \
uvx mcp-server-qdrant
```

### **Step 2: QiMCP Client Connects to Qdrant Server**

```typescript
// QiMCP discovers and connects to the Qdrant MCP server
const qiMCP = new MCPClientManager();

// Add Qdrant server configuration
await qiMCP.addServerConfig("qdrant", {
  command: "uvx",
  args: ["mcp-server-qdrant"],
  env: {
    QDRANT_URL: "http://localhost:6333",
    COLLECTION_NAME: "my-documents",
    EMBEDDING_MODEL: "sentence-transformers/all-MiniLM-L6-v2"
  }
});

// Connect to server
await qiMCP.connectToServer("qdrant");

// Discover available tools
const tools = await qiMCP.discoverToolsFromServer("qdrant");
console.log(tools);
// Output: ["qdrant-store", "qdrant-find"]
```

### **Step 3: PE/Agent Uses RAG Tools**

```typescript
// PE workflow using RAG
class ProcessExecutor {
  constructor(private mcpClient: MCPClientManager) {}
  
  async executeRAGWorkflow(query: string): Promise<string> {
    // 1. Store documents in vector database
    await this.storeDocuments();
    
    // 2. Retrieve relevant context
    const context = await this.retrieveContext(query);
    
    // 3. Generate response with context
    const response = await this.generateWithContext(query, context);
    
    return response;
  }
  
  private async storeDocuments(): Promise<void> {
    const documents = [
      "Machine learning is a subset of artificial intelligence...",
      "Vector databases store high-dimensional embeddings...",
      "RAG combines retrieval with generation for better responses..."
    ];
    
    // Store each document using Qdrant MCP server
    for (const doc of documents) {
      await this.mcpClient.executeTool("qdrant-store", {
        information: doc,
        metadata: { source: "knowledge_base", timestamp: Date.now() }
      });
    }
  }
  
  private async retrieveContext(query: string): Promise<string> {
    // Retrieve relevant documents using semantic search
    const result = await this.mcpClient.executeTool("qdrant-find", {
      query: query
    });
    
    return result.content; // Retrieved context
  }
  
  private async generateWithContext(query: string, context: string): Promise<string> {
    // Use QiPrompt to generate response with retrieved context
    const prompt = `
    Context: ${context}
    
    Question: ${query}
    
    Please answer the question using the provided context.
    `;
    
    return await this.llm.generate(prompt);
  }
}
```

### **Step 4: Complete RAG Workflow**

```typescript
// Example usage
const pe = new ProcessExecutor(qiMCP);

// Query with RAG
const response = await pe.executeRAGWorkflow(
  "What is the relationship between machine learning and vector databases?"
);

console.log(response);
// Output: "Based on the context, machine learning models generate embeddings 
// that are stored in vector databases for efficient similarity search..."
```

## Advanced Example: Multiple Vector Databases

```typescript
// Connect to multiple RAG servers simultaneously
const qiMCP = new MCPClientManager();

// Add multiple vector database servers
await qiMCP.addServerConfig("qdrant", { /* Qdrant config */ });
await qiMCP.addServerConfig("chroma", { /* Chroma config */ });
await qiMCP.addServerConfig("pinecone", { /* Pinecone config */ });

// Connect to all servers
await qiMCP.connectToAllServers();

// Route different document types to different databases
class MultiRAGProcessor {
  async storeDocument(doc: Document): Promise<void> {
    if (doc.type === "code") {
      // Store code in Qdrant (optimized for code search)
      await qiMCP.executeToolOnServer("qdrant", "qdrant-store", {
        information: doc.content,
        metadata: { type: "code", language: doc.language }
      });
    } else if (doc.type === "research") {
      // Store research papers in Chroma (optimized for academic content)
      await qiMCP.executeToolOnServer("chroma", "chroma-store", {
        content: doc.content,
        metadata: { type: "research", domain: doc.domain }
      });
    }
  }
  
  async hybridSearch(query: string): Promise<string> {
    // Search across multiple vector databases
    const [codeResults, researchResults] = await Promise.all([
      qiMCP.executeToolOnServer("qdrant", "qdrant-find", { query }),
      qiMCP.executeToolOnServer("chroma", "chroma-search", { query })
    ]);
    
    // Combine and rank results
    return this.combineResults(codeResults, researchResults);
  }
}
```

## Real Production Example: Cursor/Windsurf Integration

**Cursor and Windsurf IDEs** already use this pattern:

```bash
# Configure Qdrant MCP server for code search
QDRANT_URL="http://localhost:6333" \
COLLECTION_NAME="code-snippets" \
TOOL_STORE_DESCRIPTION="Store reusable code snippets for later retrieval" \
TOOL_FIND_DESCRIPTION="Search for relevant code snippets based on natural language" \
uvx mcp-server-qdrant --transport sse

# Cursor connects to this server at: http://localhost:8000/sse
```

**Result**: Cursor can now:
1. **Store code snippets** with natural language descriptions
2. **Search code semantically** ("find React hooks for data fetching")
3. **Retrieve relevant examples** for code generation

## Framework Integration Examples

### **Dify Integration**:
```typescript
// Dify workflow can use MCP servers through QiMCP
const difyWorkflow = {
  nodes: [
    {
      type: "rag_retrieval",
      implementation: async (query: string) => {
        return await qiMCP.executeTool("qdrant-find", { query });
      }
    },
    {
      type: "llm_generation", 
      implementation: async (context: string, query: string) => {
        return await qiPrompt.execute("rag_prompt", { context, query });
      }
    }
  ]
};
```

### **LangChain Integration**:
```typescript
// LangChain retriever using MCP
class MCPRetriever extends BaseRetriever {
  constructor(private mcpClient: MCPClientManager) {
    super();
  }
  
  async getRelevantDocuments(query: string): Promise<Document[]> {
    const result = await this.mcpClient.executeTool("qdrant-find", { query });
    return this.parseDocuments(result);
  }
}

// Use in LangChain RAG chain
const ragChain = new RetrievalQAChain({
  retriever: new MCPRetriever(qiMCP),
  llm: new ChatOpenAI()
});
```

### **LlamaIndex Integration**:
```typescript
// LlamaIndex with MCP backend
class MCPVectorStore extends VectorStore {
  constructor(private mcpClient: MCPClientManager) {
    super();
  }
  
  async add(documents: Document[]): Promise<void> {
    for (const doc of documents) {
      await this.mcpClient.executeTool("qdrant-store", {
        information: doc.content,
        metadata: doc.metadata
      });
    }
  }
  
  async query(query: string): Promise<QueryResult> {
    const result = await this.mcpClient.executeTool("qdrant-find", { query });
    return this.parseQueryResult(result);
  }
}
```

## Key Benefits of This Approach

### **✅ No Server Building Required**:
- **Leverage existing ecosystem**: 50+ vector database MCP servers
- **Production-ready**: Servers like Qdrant, Chroma, Pinecone are battle-tested
- **Maintained by experts**: Official servers maintained by database companies

### **✅ Sophisticated Client Capabilities**:
- **Multi-database support**: Connect to multiple vector databases simultaneously
- **Tool aggregation**: Unified interface across different RAG systems
- **Automatic routing**: Route queries to appropriate databases
- **Error handling**: Robust error handling and fallback strategies

### **✅ Framework Integration**:
- **Works with any framework**: Dify, LangChain, LlamaIndex, custom solutions
- **Standardized interface**: Same MCP tools across all frameworks
- **Easy switching**: Change vector databases without rewriting code

### **✅ Production Scalability**:
- **Horizontal scaling**: Add more vector database servers as needed
- **Load balancing**: Distribute queries across multiple servers
- **High availability**: Fallback to other servers if one fails

## Summary: The MCP RAG Pattern

```
Your PE/Agent → QiMCP Client → Existing RAG MCP Servers
             ↓
    1. Discover available RAG servers (Qdrant, Chroma, Pinecone, etc.)
    2. Connect to multiple servers simultaneously
    3. Store documents using "store" tools
    4. Retrieve context using "find/search" tools  
    5. Generate responses with retrieved context
```

**The magic**: You get enterprise-grade RAG capabilities without building any servers - just configure and connect to the existing ecosystem! 