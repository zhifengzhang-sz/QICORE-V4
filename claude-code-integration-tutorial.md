# Claude Code Integration Tutorial: Building MCP Apps with Claude Code

This tutorial covers two approaches for integrating your MCP CLI application with Claude Code, enabling powerful agent delegation capabilities.

## Table of Contents
1. [Overview](#overview)
2. [Approach 1: MCP Server → Claude Code](#approach-1-mcp-server--claude-code)
3. [Approach 2: Your CLI → Claude Code SDK](#approach-2-your-cli--claude-code-sdk)
4. [Comparison and Use Cases](#comparison-and-use-cases)
5. [Resources and Links](#resources-and-links)

## Overview

Claude Code supports two integration patterns for MCP applications:

1. **MCP Server Pattern**: Your CLI acts as an MCP server that Claude Code connects to
2. **SDK Pattern**: Your CLI uses Claude Code's SDK to delegate agent work programmatically

## Approach 1: MCP Server → Claude Code

This is the standard approach where your CLI application acts as an MCP server that Claude Code can connect to and use.

### How It Works

```
User → Claude Code → Your MCP Server → Your Tools/Services
```

### Setting Up Your MCP Server

#### 1. Implement the MCP Protocol

Your CLI needs to implement the MCP protocol. Here's a basic structure:

```python
# Example MCP server in Python
import json
import sys
from typing import Any, Dict

class MCPServer:
    def __init__(self):
        self.tools = {
            "my_custom_tool": {
                "description": "Does something custom",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "input": {"type": "string"}
                    },
                    "required": ["input"]
                }
            }
        }
    
    def handle_request(self, request: Dict[str, Any]) -> Dict[str, Any]:
        method = request.get("method")
        
        if method == "tools/list":
            return {
                "tools": list(self.tools.values())
            }
        elif method == "tools/call":
            tool_name = request["params"]["name"]
            args = request["params"]["arguments"]
            return self.execute_tool(tool_name, args)
    
    def execute_tool(self, tool_name: str, args: Dict[str, Any]):
        # Implement your tool logic here
        if tool_name == "my_custom_tool":
            return {"result": f"Processed: {args['input']}"}
```

#### 2. Configure Claude Code

Add your server to Claude Code's configuration file:

**Location**: 
- macOS: `~/Library/Application Support/Claude/claude_desktop_config.json`
- Windows: `%APPDATA%\Claude\claude_desktop_config.json`
- Linux: `~/.config/Claude/claude_desktop_config.json`

```json
{
  "mcpServers": {
    "my-mcp-cli": {
      "command": "/path/to/your/mcp-cli",
      "args": ["server", "--mode=stdio"],
      "env": {
        "API_KEY": "your-api-key-if-needed"
      }
    }
  }
}
```

#### 3. Communication Protocol

MCP uses JSON-RPC over stdio. Your server should:
- Read requests from stdin
- Write responses to stdout
- Use stderr for logging

```python
# Reading and writing JSON-RPC
while True:
    line = sys.stdin.readline()
    if not line:
        break
    
    request = json.loads(line)
    response = server.handle_request(request)
    
    sys.stdout.write(json.dumps(response) + "\n")
    sys.stdout.flush()
```

### Example: Complete MCP Server

```python
#!/usr/bin/env python3
import json
import sys
import logging

# Set up logging to stderr
logging.basicConfig(stream=sys.stderr, level=logging.INFO)

class MyMCPServer:
    def __init__(self):
        self.tools = [
            {
                "name": "execute_agent_task",
                "description": "Delegates a task to a specialized agent",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "task": {"type": "string"},
                        "context": {"type": "object"}
                    },
                    "required": ["task"]
                }
            },
            {
                "name": "query_database",
                "description": "Queries your custom database",
                "inputSchema": {
                    "type": "object",
                    "properties": {
                        "query": {"type": "string"}
                    },
                    "required": ["query"]
                }
            }
        ]
    
    def run(self):
        while True:
            try:
                line = sys.stdin.readline()
                if not line:
                    break
                
                request = json.loads(line)
                response = self.handle_request(request)
                
                sys.stdout.write(json.dumps({
                    "jsonrpc": "2.0",
                    "id": request.get("id"),
                    "result": response
                }) + "\n")
                sys.stdout.flush()
                
            except Exception as e:
                logging.error(f"Error: {e}")
    
    def handle_request(self, request):
        method = request.get("method")
        
        if method == "initialize":
            return {
                "protocolVersion": "2024-11-05",
                "capabilities": {
                    "tools": {}
                },
                "serverInfo": {
                    "name": "my-mcp-cli",
                    "version": "1.0.0"
                }
            }
        
        elif method == "tools/list":
            return {"tools": self.tools}
        
        elif method == "tools/call":
            tool_name = request["params"]["name"]
            args = request["params"]["arguments"]
            
            if tool_name == "execute_agent_task":
                # Your agent delegation logic here
                result = self.delegate_to_agent(args["task"], args.get("context", {}))
                return {"content": [{"type": "text", "text": result}]}
            
            elif tool_name == "query_database":
                # Your database query logic here
                result = self.query_db(args["query"])
                return {"content": [{"type": "text", "text": json.dumps(result)}]}
    
    def delegate_to_agent(self, task, context):
        # Implement your agent delegation logic
        return f"Agent completed task: {task}"
    
    def query_db(self, query):
        # Implement your database query logic
        return {"result": "mock data"}

if __name__ == "__main__":
    server = MyMCPServer()
    server.run()
```

## Approach 2: Your CLI → Claude Code SDK

This approach allows your CLI to programmatically control Claude Code, delegating complex agent tasks.

### How It Works

```
User → Your CLI → Claude Code SDK → Claude Code Agent
```

### Using the Claude Code SDK

#### 1. Install the SDK

```bash
npm install @anthropic/claude-code-sdk
# or
pip install claude-code-sdk
```

#### 2. Initialize and Use

```typescript
// TypeScript/JavaScript example
import { ClaudeCode } from '@anthropic/claude-code-sdk';

class MyCLIApp {
    private claude: ClaudeCode;
    
    constructor() {
        this.claude = new ClaudeCode({
            // Configuration options
        });
    }
    
    async delegateToClaudeCode(task: string, context: any) {
        // Create a new conversation
        const conversation = await this.claude.createConversation({
            workingDirectory: process.cwd(),
            tools: ['bash', 'read', 'write', 'edit']
        });
        
        // Send the task
        const response = await conversation.sendMessage(task);
        
        // Process the response
        return response;
    }
    
    async runComplexTask() {
        // Your CLI logic
        console.log("Starting complex task...");
        
        // Delegate to Claude Code when needed
        const result = await this.delegateToClaudeCode(
            "Analyze this codebase and find all API endpoints",
            { directory: "./src" }
        );
        
        // Continue with your CLI logic
        console.log("Task completed:", result);
    }
}
```

```python
# Python example
from claude_code_sdk import ClaudeCode

class MyCLIApp:
    def __init__(self):
        self.claude = ClaudeCode()
    
    def delegate_to_claude_code(self, task: str, context: dict):
        # Create a conversation
        conversation = self.claude.create_conversation(
            working_directory=".",
            tools=["bash", "read", "write", "edit"]
        )
        
        # Send the task
        response = conversation.send_message(task)
        
        return response
    
    def run_complex_task(self):
        print("Starting complex task...")
        
        # Delegate to Claude Code
        result = self.delegate_to_claude_code(
            "Refactor this function to use async/await",
            {"file": "src/main.py", "function": "process_data"}
        )
        
        print(f"Task completed: {result}")
```

### Advanced SDK Usage

#### Streaming Responses

```typescript
const stream = await conversation.streamMessage(task);

for await (const chunk of stream) {
    if (chunk.type === 'text') {
        process.stdout.write(chunk.text);
    } else if (chunk.type === 'tool_use') {
        console.log(`Tool: ${chunk.tool}, Args: ${chunk.args}`);
    }
}
```

#### Tool Control

```typescript
// Restrict tools for specific tasks
const conversation = await claude.createConversation({
    tools: ['read', 'grep'],  // Only allow read-only operations
    toolPolicy: {
        bash: 'deny',  // Explicitly deny bash access
        write: 'confirm'  // Require confirmation for writes
    }
});
```

#### Context Management

```typescript
// Provide context files
const conversation = await claude.createConversation({
    contextFiles: [
        'README.md',
        'package.json',
        'src/config.ts'
    ],
    systemPrompt: "You are helping with a TypeScript project"
});
```

## Comparison and Use Cases

### When to Use MCP Server Pattern

✅ **Best for:**
- Adding custom tools/capabilities to Claude Code
- Integrating with proprietary systems
- When you want users to interact through Claude Code's UI
- Providing domain-specific tools

**Examples:**
- Database query tools
- API integrations
- Custom build/deployment tools
- Domain-specific analyzers

### When to Use SDK Pattern

✅ **Best for:**
- Building your own CLI with Claude Code as a backend
- Programmatic automation
- Batch processing
- When you need fine-grained control over Claude Code

**Examples:**
- Code migration tools
- Automated refactoring
- CI/CD integrations
- Batch code analysis

### Feature Comparison

| Feature | MCP Server | SDK |
|---------|------------|-----|
| User Interface | Claude Code UI | Your CLI |
| Control Flow | Claude Code → Your Server | Your CLI → Claude Code |
| Tool Definition | You define tools | You use Claude Code's tools |
| Context Management | Claude Code manages | You manage |
| Streaming Support | Via MCP protocol | Native SDK support |
| Authentication | Config file | SDK credentials |

## Best Practices

### For MCP Servers

1. **Error Handling**: Always return proper error responses
2. **Logging**: Use stderr for logs, keep stdout for protocol
3. **Validation**: Validate all inputs before processing
4. **Documentation**: Provide clear tool descriptions

### For SDK Usage

1. **Rate Limiting**: Implement proper rate limiting
2. **Error Recovery**: Handle API errors gracefully
3. **Context Size**: Manage context to avoid token limits
4. **Security**: Never expose API keys in logs

## Example: Hybrid Approach

You can combine both approaches:

```python
class HybridCLI:
    def __init__(self):
        # SDK for complex agent tasks
        self.claude_sdk = ClaudeCode()
        
        # MCP server for custom tools
        self.mcp_server = MyMCPServer()
    
    def run(self):
        # Use SDK for analysis
        analysis = self.claude_sdk.analyze_codebase()
        
        # Process with custom tools via MCP
        results = self.process_with_custom_tools(analysis)
        
        # Use SDK for code generation
        code = self.claude_sdk.generate_code(results)
```

## Resources and Links

### Official Documentation
- **Claude Code Overview**: https://docs.anthropic.com/en/docs/claude-code
- **MCP Integration**: https://docs.anthropic.com/en/docs/claude-code/mcp
- **Claude Code SDK**: https://docs.anthropic.com/en/docs/claude-code/sdk
- **Configuration**: https://docs.anthropic.com/en/docs/claude-code/settings

### MCP Resources
- **MCP Protocol Specification**: https://modelcontextprotocol.io/docs
- **MCP Server Examples**: https://github.com/modelcontextprotocol/servers
- **MCP TypeScript SDK**: https://www.npmjs.com/package/@modelcontextprotocol/sdk
- **MCP Python SDK**: https://pypi.org/project/mcp/

### Additional Resources
- **Claude Code GitHub Issues**: https://github.com/anthropics/claude-code/issues
- **Community Examples**: https://github.com/topics/mcp-server
- **Video Tutorials**: Search "Claude Code MCP" on YouTube

## Getting Help

- **Help Command**: Run `/help` in Claude Code
- **Report Issues**: https://github.com/anthropics/claude-code/issues
- **Community Discord**: Join the Anthropic Discord for community support

## Next Steps

1. **For MCP Server**: Start with the MCP server template and implement your custom tools
2. **For SDK Integration**: Install the SDK and create a simple proof-of-concept
3. **Test Locally**: Test your integration thoroughly before deployment
4. **Share Your Work**: Consider open-sourcing your MCP server for the community!