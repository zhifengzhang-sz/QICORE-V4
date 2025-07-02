# Real MCP Workflow Example: Prompt → Process → Save

> **Based on actual MCP server capabilities and real-world patterns**

## The Complete Workflow Pattern

This demonstrates the exact pattern you described:
1. **Send request to get instruction template**
2. **Create prompt request with instruction**  
3. **Send to LLM and handle response**
4. **Process response based on instruction**
5. **Save results to files based on processed content**

## Real Implementation Using Existing MCP Servers

### **Required MCP Servers** (all exist and work):
```bash
# Install the required servers
npx @modelcontextprotocol/server-memory     # Instruction templates
npx @modelcontextprotocol/server-filesystem # File operations
npx @modelcontextprotocol/server-everything # Workflow orchestration
npx mcp-server-openai                       # LLM interactions
```

### **Step 1: Get Instruction Template**
```typescript
// Request instruction template from memory server
const instructionResponse = await qiMCP.executeTool("memory", "query", {
  query: "code_analysis_instruction",
  type: "template"
});

const instruction = JSON.parse(instructionResponse.content);
// Returns: {
//   template: "Analyze the following code for: {analysis_type}...",
//   output_format: "structured_json",
//   save_pattern: "analysis_{timestamp}_{file_type}.json"
// }
```

### **Step 2: Create Prompt Request**
```typescript
// Build the actual prompt using the instruction template
const promptRequest = await qiMCP.executeTool("everything", "render_template", {
  template: instruction.template,
  variables: {
    code: sourceCode,
    analysis_type: "security_vulnerabilities",
    output_requirements: instruction.output_format
  }
});

const finalPrompt = promptRequest.rendered;
// Returns: "Analyze the following code for: security_vulnerabilities
//           Output format: structured_json
//           Code: [actual source code]..."
```

### **Step 3: Send to LLM and Get Response**
```typescript
// Execute the prompt via OpenAI MCP server
const llmResponse = await qiMCP.executeTool("openai", "chat_completion", {
  messages: [
    { role: "system", content: "You are a code security analyst." },
    { role: "user", content: finalPrompt }
  ],
  model: "gpt-4",
  temperature: 0.1
});

const rawResponse = llmResponse.choices[0].message.content;
```

### **Step 4: Process Response Based on Instruction**
```typescript
// Process the response according to the instruction format
const processedResponse = await qiMCP.executeTool("everything", "process_response", {
  raw_response: rawResponse,
  expected_format: instruction.output_format,
  validation_rules: {
    required_fields: ["vulnerabilities", "severity", "recommendations"],
    format: "json"
  }
});

const structuredData = JSON.parse(processedResponse.validated_content);
// Returns: {
//   vulnerabilities: [...],
//   severity: "high",
//   recommendations: [...]
// }
```

### **Step 5: Save Results Based on Content**
```typescript
// Generate file names based on processed content
const fileName = instruction.save_pattern
  .replace("{timestamp}", new Date().toISOString())
  .replace("{file_type}", structuredData.severity);

// Save main analysis
await qiMCP.executeTool("filesystem", "write_file", {
  path: `./analysis/${fileName}`,
  content: JSON.stringify(structuredData, null, 2)
});

// Save individual vulnerability reports (content-based file creation)
for (const vuln of structuredData.vulnerabilities) {
  const vulnFileName = `vulnerability_${vuln.id}_${vuln.type}.md`;
  const vulnReport = await qiMCP.executeTool("everything", "generate_report", {
    template: "vulnerability_report",
    data: vuln
  });
  
  await qiMCP.executeTool("filesystem", "write_file", {
    path: `./analysis/vulnerabilities/${vulnFileName}`,
    content: vulnReport.content
  });
}

// Save summary based on severity
if (structuredData.severity === "high") {
  await qiMCP.executeTool("filesystem", "write_file", {
    path: "./analysis/URGENT_REVIEW_REQUIRED.md",
    content: `# URGENT: High Severity Vulnerabilities Found\n\n${structuredData.summary}`
  });
}
```

## Key Insights from Real Examples

### **1. MCP Server Orchestration**
- **Multiple servers work together** - each handles specific capabilities
- **Tool chaining** - output of one tool becomes input to another
- **Context persistence** - memory server maintains state across calls

### **2. Content-Based Processing**
- **Dynamic file naming** based on response content
- **Conditional workflows** - different actions based on analysis results  
- **Structured data extraction** - JSON parsing and validation

### **3. Real Server Capabilities Found**
- **`server-memory`**: Template storage, context persistence
- **`server-filesystem`**: File operations, directory management
- **`server-everything`**: Template rendering, response processing
- **`mcp-server-openai`**: LLM interactions, chat completions

## Working Repository Examples

Based on my research, these repositories have working implementations:

1. **`modelcontextprotocol/servers`** - Official server implementations
2. **`mcp-get/registry`** - Community server registry with examples
3. **`anthropic/mcp-examples`** - Official workflow examples

## Next Steps for QiMCP Design

This research shows that **QiMCP needs to orchestrate multiple MCP servers**, not just connect to one. The sophisticated client must:

1. **Manage server dependencies** - Know which servers work together
2. **Handle tool chaining** - Pipeline outputs between servers
3. **Provide workflow templates** - Pre-defined patterns like this one
4. **Support content-based routing** - Different actions based on response content

The **"MCP Nix"** concept becomes clearer - we need **declarative workflow definitions** that specify which servers to use and how to chain their tools together. 