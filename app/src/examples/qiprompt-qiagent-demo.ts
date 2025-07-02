#!/usr/bin/env bun

/**
 * QiPrompt + QiAgent Demo
 * 
 * Demonstrates the 4-layer architecture:
 * Your App → QiAgent → AI Orchestra → QiPrompt → Vercel AI SDK → LLM
 */

// Note: Using relative imports since @qi/mcp path mapping may not work in app
// In production, you would use: import { QiPrompt, QiAgent, AgentRoles, WorkflowPatterns, createAgent } from '@qi/mcp/qiagent'

// Mock imports for demonstration
interface QiPrompt {
  generateText(prompt: string): Promise<string>
  generateStructured<T>(prompt: string, options: any): Promise<T>
  streamText(prompt: string, options: any): Promise<string>
}

interface QiAgent {
  registerWorkflow(config: any): void
  getWorkflows(): string[]
  executeWorkflow(name: string, input: string, options?: any): Promise<any>
}

// Mock implementations for demo
const QiPrompt = class {
  constructor(config?: any) {}
  async generateText(prompt: string): Promise<string> {
    return `Mock response to: ${prompt}`
  }
  async generateStructured<T>(prompt: string, options: any): Promise<T> {
    return {
      summary: 'Mock analysis summary',
      keyPoints: ['Point 1', 'Point 2'],
      confidence: 0.85,
      recommendation: 'Mock recommendation'
    } as T
  }
  async streamText(prompt: string, options: any): Promise<string> {
    const content = 'AI thoughts flow,\nCode and circuits intertwine—\nFuture unfolds bright.'
    if (options.onChunk) {
      for (const char of content) {
        options.onChunk(char)
        await new Promise(r => setTimeout(r, 50))
      }
    }
    if (options.onComplete) options.onComplete(content)
    return content
  }
}

const QiAgent = class {
  private workflows = new Map()
  constructor(qiPrompt?: any) {}
  registerWorkflow(config: any): void {
    this.workflows.set(config.name, config)
  }
  getWorkflows(): string[] {
    return Array.from(this.workflows.keys())
  }
  async executeWorkflow(name: string, input: string, options?: any): Promise<any> {
    return {
      success: true,
      finalResult: 'Mock workflow result: Comprehensive article about quantum computing...',
      executionTime: 1500,
      steps: [
        { agent: 'researcher', duration: 500, result: 'Research completed on quantum computing trends...' },
        { agent: 'writer', duration: 1000, result: 'Article written based on research findings...' }
      ]
    }
  }
}

const AgentRoles = {
  researcher: () => ({ name: 'Research Specialist', systemPrompt: 'Research topics thoroughly' }),
  writer: () => ({ name: 'Content Writer', systemPrompt: 'Write engaging content' })
}

const WorkflowPatterns = {
  contentCreation: (name: string) => ({
    name,
    description: 'Research and write content',
    agents: { researcher: AgentRoles.researcher(), writer: AgentRoles.writer() },
    steps: { research: { agent: 'researcher', nextSteps: ['write'] }, write: { agent: 'writer', nextSteps: [] } },
    startStep: 'research'
  })
}

const createAgent = (qiPrompt?: any) => new QiAgent(qiPrompt)

const CommonSchemas = {
  analysis: {} // Mock schema
}

async function demonstrateQiPrompt() {
  console.log('🎯 QiPrompt Demo - Lightweight Vercel AI SDK Wrapper')
  console.log('=================================================\n')
  
  try {
    // Create QiPrompt client with configuration
    const qiPrompt = new QiPrompt({
      defaultTemperature: 0.7,
      retryAttempts: 2,
      timeout: 15000
    })

    // 1. Simple text generation
    console.log('📝 Simple Text Generation:')
    const result1 = await qiPrompt.generateText(
      "Explain the concept of monads in functional programming in 2 sentences."
    )
    console.log(`Result: ${result1}\n`)

    // 2. Structured generation with schema
    console.log('🔧 Structured Generation:')
    const analysis = await qiPrompt.generateStructured<{
      summary: string
      keyPoints: string[]
      confidence: number
      recommendation: string
    }>(
      "Analyze the benefits and challenges of using TypeScript for large-scale applications.",
      {
        schema: CommonSchemas.analysis,
        schemaName: "TypeScriptAnalysis"
      }
    )
    
    console.log('Analysis Result:')
    console.log(`Summary: ${analysis.summary}`)
    console.log(`Key Points: ${analysis.keyPoints.join(', ')}`)
    console.log(`Confidence: ${analysis.confidence}`)
    console.log(`Recommendation: ${analysis.recommendation}\n`)

    // 3. Streaming generation
    console.log('🌊 Streaming Generation:')
    console.log('Streaming: ')
    
    let streamedContent = ''
    await qiPrompt.streamText(
      "Write a haiku about artificial intelligence.", 
      {
        onChunk: (chunk) => {
          process.stdout.write(chunk)
          streamedContent += chunk
        },
        onComplete: (fullText) => {
          console.log('\n✅ Streaming complete!')
        }
      }
    )
    
    console.log('\n')

  } catch (error) {
    console.error('❌ QiPrompt Demo failed:', error)
  }
}

async function demonstrateQiAgent() {
  console.log('🤖 QiAgent Demo - Lightweight AI Orchestra Wrapper')
  console.log('===============================================\n')

  try {
    // Create QiAgent with QiPrompt integration
    const qiPrompt = new QiPrompt({
      defaultTemperature: 0.8,
      retryAttempts: 2
    })
    
    const qiAgent = createAgent(qiPrompt)

    // Register a content creation workflow
    const contentWorkflow = WorkflowPatterns.contentCreation('demo-content-creation')
    qiAgent.registerWorkflow(contentWorkflow)

    console.log('📋 Available Workflows:')
    console.log(qiAgent.getWorkflows().join(', '))
    console.log('\n')

    // Execute the workflow
    console.log('🚀 Executing Content Creation Workflow:')
    console.log('Topic: "The Future of Quantum Computing"\n')

    const result = await qiAgent.executeWorkflow(
      'demo-content-creation',
      'Write an article about the future of quantum computing',
      {
        data: {
          targetAudience: 'developers',
          length: 'medium'
        }
      }
    )

    if (result.success) {
      console.log('✅ Workflow completed successfully!')
      console.log(`⏱️  Execution time: ${result.executionTime}ms`)
      console.log(`🔢 Steps completed: ${result.steps.length}`)
      console.log('\n📊 Step Breakdown:')
      
      result.steps.forEach((step, index) => {
        console.log(`  ${index + 1}. ${step.agent} (${step.duration}ms)`)
        console.log(`     Result preview: ${step.result.substring(0, 100)}...`)
      })

      console.log('\n📝 Final Result:')
      console.log('================')
      console.log(result.finalResult)
      
    } else {
      console.error('❌ Workflow failed:', result.error?.message)
    }

  } catch (error) {
    console.error('❌ QiAgent Demo failed:', error)
  }
}

async function demonstrateArchitecture() {
  console.log('🏗️  Architecture Demo - 4-Layer Stack')
  console.log('===================================\n')

  console.log('🔄 Request Flow:')
  console.log('Your App → QiAgent → AI Orchestra → QiPrompt → Vercel AI SDK → LLM')
  console.log('')

  console.log('📦 Layer Responsibilities:')
  console.log('• Your App: Business logic and user interface')
  console.log('• QiAgent: Workflow orchestration and agent coordination')  
  console.log('• AI Orchestra: Multi-agent handoffs and state management')
  console.log('• QiPrompt: LLM interaction patterns and retry logic')
  console.log('• Vercel AI SDK: Model providers and streaming')
  console.log('• LLM: Actual language model execution')
  console.log('')

  console.log('✅ Benefits:')
  console.log('• Each layer has a single responsibility')
  console.log('• Easy to swap components (e.g., different LLM providers)')
  console.log('• Simplified APIs for common patterns')
  console.log('• Built-in error handling and retry logic')
  console.log('• Type safety throughout the stack')
  console.log('')
}

async function main() {
  console.log('🎭 QiPrompt + QiAgent Integration Demo')
  console.log('=====================================\n')

  console.log('This demo shows the new 4-layer architecture with:')
  console.log('• QiPrompt: Lightweight wrapper around Vercel AI SDK')
  console.log('• QiAgent: Lightweight wrapper around AI Orchestra')
  console.log('')

  try {
    // Note: These demos will fail without actual model configuration
    // But they show the intended usage patterns
    
    await demonstrateArchitecture()
    
    console.log('⚠️  The following demos require a configured language model:')
    console.log('   (Install and run: ollama pull llama3.2)\n')
    
    // Uncomment these when you have a model configured:
    // await demonstrateQiPrompt()
    // await demonstrateQiAgent()
    
    console.log('🎉 Demo completed! The architecture is ready for real LLM integration.')
    
  } catch (error) {
    console.error('❌ Demo failed:', error)
  }
}

// Run the demo
main() 