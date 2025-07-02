#!/usr/bin/env bun

/**
 * Claude Code Tools Demo
 *
 * Demonstrates how Claude Code SDK integrates with specific development tools
 * like file operations, bash commands, and project analysis through the
 * @qi/agent unified framework.
 */

import { mkdir, writeFile } from "node:fs/promises";
import { ClaudeCode } from "@qi/agent";

console.log("🛠️  Claude Code Tools Integration Demo");
console.log("=====================================");

/**
 * Demo 1: File Analysis with Claude Code
 */
async function demoFileAnalysis() {
	console.log("\n📁 Demo 1: File Analysis");
	console.log("------------------------");

	try {
		// Create Claude Code agent optimized for code analysis
		const result = ClaudeCode.createAgent({
			model: "claude-3-5-sonnet-20241022",
			temperature: 0.2, // Lower temperature for more focused analysis
			maxTokens: 1500,
		});

		if (result._tag !== "Right") {
			console.log("❌ Failed to create agent:", result.left.message);
			return;
		}

		const agent = result.right;

		// Create a sample TypeScript file to analyze
		const sampleCode = `
export interface User {
    readonly id: number;
    readonly name: string;
    readonly email: string;
    readonly createdAt: Date;
}

export class UserService {
    private users: Map<number, User> = new Map();

    addUser(user: Omit<User, 'id'>): User {
        const id = this.users.size + 1;
        const newUser: User = { ...user, id };
        this.users.set(id, newUser);
        return newUser;
    }

    findUser(id: number): User | undefined {
        return this.users.get(id);
    }

    updateUser(id: number, updates: Partial<Omit<User, 'id'>>): User | null {
        const user = this.users.get(id);
        if (!user) return null;
        
        const updatedUser = { ...user, ...updates };
        this.users.set(id, updatedUser);
        return updatedUser;
    }
}`;

		console.log("📝 Analyzing TypeScript code with Claude Code...");

		const analysisResult = await agent.generate({
			prompt: `Please analyze this TypeScript code for:

1. **Code Quality**: Overall structure, naming, and patterns
2. **Type Safety**: How well it uses TypeScript features
3. **Potential Issues**: Bugs, edge cases, or improvements
4. **Best Practices**: What's done well and what could be improved

Here's the code:

\`\`\`typescript
${sampleCode}
\`\`\`

Provide a structured analysis with specific recommendations.`,
			systemPrompt:
				"You are an expert TypeScript developer and code reviewer. Provide practical, actionable feedback.",
		});

		if (analysisResult._tag === "Right") {
			const response = analysisResult.right;
			console.log("✅ File analysis completed!");
			console.log("📊 Analysis results:");
			console.log("---");
			console.log(response.content);
			console.log("---");
			console.log(`📈 Tokens used: ${response.usage?.totalTokens}`);
		} else {
			console.log("❌ Analysis failed:", analysisResult.left.message);
		}
	} catch (error) {
		console.log("❌ File analysis demo failed:", error);
	}
}

/**
 * Demo 2: Project Structure Analysis
 */
async function demoProjectStructureAnalysis() {
	console.log("\n🏗️  Demo 2: Project Structure Analysis");
	console.log("-------------------------------------");

	try {
		const result = ClaudeCode.createAgent({
			temperature: 0.3,
			maxTokens: 2000,
		});

		if (result._tag !== "Right") {
			console.log("❌ Failed to create agent:", result.left.message);
			return;
		}

		const agent = result.right;

		// Simulate project structure (in real scenario, you'd read from filesystem)
		const projectStructure = `
qicore-project/
├── src/
│   ├── qiagent/
│   │   ├── index.ts
│   │   └── claude-code.ts
│   ├── qimcp/
│   │   └── client.ts
│   ├── qiprompt/
│   │   └── index.ts
│   └── qicore/
│       ├── base/
│       │   ├── error.ts
│       │   └── result.ts
│       └── index.ts
├── tests/
│   ├── qiagent/
│   ├── qimcp/
│   └── qiprompt/
├── examples/
│   ├── claude-code-demo.ts
│   └── quickstart.ts
├── package.json
├── tsconfig.json
└── README.md`;

		console.log("🔍 Analyzing project structure with Claude Code...");

		const structureResult = await agent.generate({
			prompt: `Analyze this TypeScript project structure and provide insights on:

1. **Architecture**: How well the project is organized
2. **Modularity**: Separation of concerns and module design
3. **Scalability**: How well it would scale as the project grows
4. **Best Practices**: What follows Node.js/TypeScript conventions
5. **Recommendations**: Specific improvements or additions

Project structure:
\`\`\`
${projectStructure}
\`\`\`

Focus on practical architecture advice for a TypeScript AI/ML library.`,
			systemPrompt:
				"You are a senior software architect specializing in TypeScript and Node.js projects. Provide strategic architectural guidance.",
		});

		if (structureResult._tag === "Right") {
			const response = structureResult.right;
			console.log("✅ Project structure analysis completed!");
			console.log("📊 Architecture insights:");
			console.log("---");
			console.log(response.content);
			console.log("---");
		} else {
			console.log("❌ Structure analysis failed:", structureResult.left.message);
		}
	} catch (error) {
		console.log("❌ Project structure analysis failed:", error);
	}
}

/**
 * Demo 3: Test Generation with Claude Code
 */
async function demoTestGeneration() {
	console.log("\n🧪 Demo 3: Test Generation");
	console.log("--------------------------");

	try {
		const result = ClaudeCode.createAgent({
			temperature: 0.4, // Slightly higher for creative test scenarios
			maxTokens: 2000,
		});

		if (result._tag !== "Right") {
			console.log("❌ Failed to create agent:", result.left.message);
			return;
		}

		const agent = result.right;

		const functionToTest = `
export function calculateTax(income: number, taxRate: number): number {
    if (income < 0) {
        throw new Error("Income cannot be negative");
    }
    if (taxRate < 0 || taxRate > 1) {
        throw new Error("Tax rate must be between 0 and 1");
    }
    return income * taxRate;
}`;

		console.log("🧪 Generating tests with Claude Code...");

		const testResult = await agent.generate({
			prompt: `Generate comprehensive unit tests for this TypeScript function using Vitest:

\`\`\`typescript
${functionToTest}
\`\`\`

Please create tests that cover:
1. **Happy path**: Normal valid inputs
2. **Edge cases**: Boundary values, zero values
3. **Error cases**: Invalid inputs and error handling
4. **Type safety**: Ensure TypeScript types are properly tested

Use Vitest syntax with describe/it blocks and proper assertions.`,
			systemPrompt:
				"You are a test-driven development expert. Create thorough, maintainable tests that catch real bugs.",
		});

		if (testResult._tag === "Right") {
			const response = testResult.right;
			console.log("✅ Test generation completed!");
			console.log("📊 Generated tests:");
			console.log("---");
			console.log(response.content);
			console.log("---");

			// Optionally save the tests to a file
			try {
				await mkdir("./temp-output", { recursive: true });
				await writeFile("./temp-output/generated-tests.test.ts", response.content);
				console.log("💾 Tests saved to ./temp-output/generated-tests.test.ts");
			} catch (saveError) {
				console.log("⚠️  Could not save tests to file:", saveError);
			}
		} else {
			console.log("❌ Test generation failed:", testResult.left.message);
		}
	} catch (error) {
		console.log("❌ Test generation demo failed:", error);
	}
}

/**
 * Demo 4: Documentation Generation
 */
async function demoDocumentationGeneration() {
	console.log("\n📚 Demo 4: Documentation Generation");
	console.log("-----------------------------------");

	try {
		const result = ClaudeCode.createAgent({
			temperature: 0.3,
			maxTokens: 2500,
		});

		if (result._tag !== "Right") {
			console.log("❌ Failed to create agent:", result.left.message);
			return;
		}

		const agent = result.right;

		const apiCode = `
export class DatabaseManager {
    private connection: any;

    constructor(connectionString: string) {
        this.connection = this.connect(connectionString);
    }

    async query<T>(sql: string, params?: any[]): Promise<T[]> {
        // Execute SQL query with parameters
        return this.connection.query(sql, params);
    }

    async transaction<T>(fn: (tx: any) => Promise<T>): Promise<T> {
        const tx = await this.connection.beginTransaction();
        try {
            const result = await fn(tx);
            await tx.commit();
            return result;
        } catch (error) {
            await tx.rollback();
            throw error;
        }
    }

    async close(): Promise<void> {
        await this.connection.close();
    }
}`;

		console.log("📖 Generating API documentation with Claude Code...");

		const docResult = await agent.generate({
			prompt: `Generate comprehensive API documentation for this TypeScript class:

\`\`\`typescript
${apiCode}
\`\`\`

Create documentation that includes:
1. **Overview**: What the class does and when to use it
2. **Constructor**: Parameters and setup requirements
3. **Methods**: Each method with parameters, return types, examples
4. **Error Handling**: What errors can occur and how to handle them
5. **Usage Examples**: Practical code examples
6. **TypeScript Types**: Clear type information

Format as Markdown suitable for a README or API docs.`,
			systemPrompt:
				"You are a technical documentation expert. Create clear, comprehensive docs that help developers succeed.",
		});

		if (docResult._tag === "Right") {
			const response = docResult.right;
			console.log("✅ Documentation generation completed!");
			console.log("📊 Generated documentation:");
			console.log("---");
			console.log(response.content);
			console.log("---");

			// Save documentation
			try {
				await mkdir("./temp-output", { recursive: true });
				await writeFile("./temp-output/api-documentation.md", response.content);
				console.log("💾 Documentation saved to ./temp-output/api-documentation.md");
			} catch (saveError) {
				console.log("⚠️  Could not save documentation:", saveError);
			}
		} else {
			console.log("❌ Documentation generation failed:", docResult.left.message);
		}
	} catch (error) {
		console.log("❌ Documentation generation demo failed:", error);
	}
}

/**
 * Demo 5: Code Refactoring Suggestions
 */
async function demoCodeRefactoring() {
	console.log("\n🔧 Demo 5: Code Refactoring Suggestions");
	console.log("---------------------------------------");

	try {
		const result = ClaudeCode.createAgent({
			temperature: 0.2,
			maxTokens: 2000,
		});

		if (result._tag !== "Right") {
			console.log("❌ Failed to create agent:", result.left.message);
			return;
		}

		const agent = result.right;

		const legacyCode = `
function processUserData(userData) {
    var result = {};
    
    if (userData.name) {
        result.name = userData.name.toLowerCase();
    }
    
    if (userData.email) {
        if (userData.email.indexOf('@') > -1) {
            result.email = userData.email.toLowerCase();
        } else {
            throw new Error('Invalid email');
        }
    }
    
    if (userData.age) {
        if (userData.age > 0 && userData.age < 150) {
            result.age = parseInt(userData.age);
        } else {
            throw new Error('Invalid age');
        }
    }
    
    result.processedAt = new Date();
    
    return result;
}`;

		console.log("♻️  Analyzing code for refactoring with Claude Code...");

		const refactorResult = await agent.generate({
			prompt: `Please refactor this JavaScript code to modern TypeScript with improvements for:

1. **Type Safety**: Add proper TypeScript types
2. **Modern Syntax**: Use modern ES features and best practices
3. **Error Handling**: Improve error handling with Result patterns
4. **Validation**: Better input validation
5. **Maintainability**: More readable and maintainable structure

Original code:
\`\`\`javascript
${legacyCode}
\`\`\`

Show the refactored version with explanations of the improvements made.`,
			systemPrompt:
				"You are a senior developer focused on modern TypeScript best practices and clean code principles.",
		});

		if (refactorResult._tag === "Right") {
			const response = refactorResult.right;
			console.log("✅ Refactoring suggestions completed!");
			console.log("📊 Refactored code:");
			console.log("---");
			console.log(response.content);
			console.log("---");
		} else {
			console.log("❌ Refactoring failed:", refactorResult.left.message);
		}
	} catch (error) {
		console.log("❌ Code refactoring demo failed:", error);
	}
}

/**
 * Main demo execution
 */
async function runClaudeCodeToolsDemo() {
	console.log("🚀 Starting Claude Code Tools Integration Demo...\n");

	await demoFileAnalysis();
	await demoProjectStructureAnalysis();
	await demoTestGeneration();
	await demoDocumentationGeneration();
	await demoCodeRefactoring();

	console.log("\n🎉 Claude Code Tools Demo completed!");
	console.log("\n✨ Summary of Tools Integration:");
	console.log("   • File Analysis: Code quality and improvement suggestions");
	console.log("   • Project Structure: Architecture analysis and recommendations");
	console.log("   • Test Generation: Comprehensive unit test creation");
	console.log("   • Documentation: API documentation generation");
	console.log("   • Code Refactoring: Legacy code modernization");

	console.log("\n💡 Generated files (if successful):");
	console.log("   • ./temp-output/generated-tests.test.ts");
	console.log("   • ./temp-output/api-documentation.md");

	console.log("\n🔧 Next Steps:");
	console.log("   • Run generated tests with: bun test temp-output/generated-tests.test.ts");
	console.log("   • Review generated documentation");
	console.log("   • Integrate these patterns into your development workflow");
}

// Run if this file is executed directly
if (import.meta.main) {
	runClaudeCodeToolsDemo().catch(console.error);
}

export { runClaudeCodeToolsDemo };
