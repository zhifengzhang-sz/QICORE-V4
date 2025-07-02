#!/usr/bin/env bun

/**
 * QiAgent Workflow Demo
 * 
 * Demonstrates the 4-layer architecture in action:
 * Your App → QiAgent → AI Orchestra → QiPrompt → Vercel AI SDK → LLM
 * 
 * Shows simplified workflow creation using the wrapper layers.
 */

// Mock implementations for demonstration
// In production: import { QiPrompt, QiAgent, AgentRoles, WorkflowPatterns, createAgent } from '@qi/mcp/qiagent'

interface QiPrompt {
  generateText(prompt: string, options?: any): Promise<string>
  generateStructured<T>(prompt: string, options: any): Promise<T>
}

interface QiAgent {
  registerWorkflow(config: any): void
  getWorkflows(): string[]
  executeWorkflow(name: string, input: string, options?: any): Promise<any>
}

// Mock implementations
const QiPrompt = class {
  constructor(config?: any) {}
  async generateText(prompt: string, options?: any): Promise<string> {
    return `Generated response to: ${prompt}`
  }
  async generateStructured<T>(prompt: string, options: any): Promise<T> {
    return {
      summary: `Analysis of: ${prompt}`,
      keyPoints: ['Key point 1', 'Key point 2', 'Key point 3'],
      confidence: 0.85,
      recommendation: 'This is a well-structured analysis with clear recommendations.'
    } as T
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
    const workflow = this.workflows.get(name)
    const steps = Object.keys(workflow?.steps || {})
    
    return {
      success: true,
      finalResult: `Comprehensive workflow result for: ${input}. This demonstrates the complete ${name} process with high-quality output from all agents in the workflow.`,
      executionTime: 1500 + Math.floor(Math.random() * 1000),
      steps: steps.map((step, index) => ({
        agent: workflow?.steps[step]?.agent || `agent-${index}`,
        duration: 300 + Math.floor(Math.random() * 500),
        result: `Step ${index + 1} completed: ${step} agent processed the input and generated relevant output for the next stage.`
      }))
    }
  }
}

const AgentRoles = {
  researcher: () => ({
    name: 'Research Specialist',
    description: 'Analyzes topics and gathers relevant information',
    systemPrompt: 'You are a research specialist. Analyze topics thoroughly and provide comprehensive research summaries.'
  }),
  writer: () => ({
    name: 'Content Writer',
    description: 'Creates engaging content based on research and requirements',
    systemPrompt: 'You are a skilled content writer. Create engaging, well-structured content based on the provided research.'
  })
}

const WorkflowPatterns = {
  contentCreation: (name: string) => ({
    name,
    description: 'Research, write, and review content',
    agents: {
      researcher: AgentRoles.researcher(),
      writer: AgentRoles.writer()
    },
    steps: {
      research: {
        agent: 'researcher',
        nextSteps: ['write']
      },
      write: {
        agent: 'writer',
        nextSteps: []
      }
    },
    startStep: 'research'
  })
}

const createAgent = (qiPrompt?: any) => new QiAgent(qiPrompt)

const CommonSchemas = {
  analysis: {} // Mock schema
}

/**
 * DEMO 1: Using Pre-built Workflow Patterns
 * 
 * Shows how QiAgent simplifies workflow creation with pre-built patterns
 */
async function demonstrateWorkflowPatterns() {
  console.log('🎯 Demo 1: Pre-built Workflow Patterns')
  console.log('=====================================\n')

  try {
    // Create QiPrompt with configuration
    const qiPrompt = new QiPrompt({
      defaultTemperature: 0.7,
      timeout: 15000
    })

    // Create QiAgent with QiPrompt integration
    const agent = createAgent(qiPrompt)

    // Use pre-built content creation pattern
    const contentWorkflow = WorkflowPatterns.contentCreation('demo-content-workflow')
    agent.registerWorkflow(contentWorkflow)

    console.log('📋 Registered Workflows:')
    console.log(agent.getWorkflows().join(', '))
    console.log('\n🚀 Executing content creation workflow...\n')

    // Execute the workflow
    const result = await agent.executeWorkflow(
      'demo-content-workflow',
      'Write a comprehensive guide about TypeScript decorators'
    )

    if (result.success) {
      console.log('✅ Workflow completed successfully!')
      console.log(`⏱️  Execution time: ${result.executionTime}ms`)
      console.log(`🔢 Steps completed: ${result.steps.length}`)
      
      console.log('\n📊 Step-by-step execution:')
      result.steps.forEach((step, index) => {
        console.log(`  ${index + 1}. ${step.agent} (${step.duration}ms)`)
        console.log(`     Result: ${step.result.substring(0, 80)}...`)
      })

      console.log('\n📝 Final Result:')
      console.log('================')
      console.log(result.finalResult)
    } else {
      console.error('❌ Workflow failed:', result.error?.message)
    }

  } catch (error) {
    console.error('❌ Demo failed:', error)
  }
}

/**
 * DEMO 2: Custom Workflow Creation
 * 
 * Shows how to create custom workflows using QiAgent's fluent API
 */
async function demonstrateCustomWorkflow() {
  console.log('\n🛠️  Demo 2: Custom Workflow Creation')
  console.log('===================================\n')

  try {
    // Create QiPrompt with specific model configuration
    const qiPrompt = new QiPrompt({
      defaultTemperature: 0.8,
      timeout: 20000
    })

    const agent = createAgent(qiPrompt)

    // Create a custom mathematical analysis workflow
    const mathWorkflow = {
      name: 'mathematical-analysis',
      description: 'Analyze mathematical concepts with formal verification',
      agents: {
        researcher: {
          ...AgentRoles.researcher(),
          systemPrompt: `
            You are a mathematical research specialist. Your role is to:
            1. Analyze the given mathematical concept thoroughly
            2. Research relevant theorems, proofs, and applications
            3. Identify key properties and relationships
            4. Prepare findings for formal verification
            
            Focus on mathematical rigor and clarity.
          `
        },
        verifier: {
          name: "Formal Verifier",
          description: "Verifies mathematical concepts using formal methods",
          systemPrompt: `
            You are a formal verification specialist. Your role is to:
            1. Take the research findings from the researcher
            2. Apply formal verification methods
            3. Check mathematical proofs and properties
            4. Validate correctness and completeness
            
            Use rigorous mathematical reasoning and formal logic.
          `
        },
        documenter: {
          name: "Mathematical Documenter", 
          description: "Creates comprehensive mathematical documentation",
          systemPrompt: `
            You are a mathematical documentation specialist. Your role is to:
            1. Take verified mathematical analysis
            2. Create clear, comprehensive documentation
            3. Include examples, proofs, and applications
            4. Make complex concepts accessible
            
            Focus on clarity and educational value.
          `
        }
      },
      steps: {
        research: {
          agent: 'researcher',
          nextSteps: ['verify']
        },
        verify: {
          agent: 'verifier',
          nextSteps: ['document']
        },
        document: {
          agent: 'documenter',
          nextSteps: []
        }
      },
      startStep: 'research'
    }

    // Register the custom workflow
    agent.registerWorkflow(mathWorkflow)

    console.log('📋 Custom workflow registered: mathematical-analysis')
    console.log('🚀 Executing mathematical analysis workflow...\n')

    // Execute with a mathematical concept
    const result = await agent.executeWorkflow(
      'mathematical-analysis',
      'Analyze the mathematical properties and applications of the Fibonacci sequence'
    )

    if (result.success) {
      console.log('✅ Mathematical analysis completed!')
      console.log(`⏱️  Total time: ${result.executionTime}ms`)
      
      console.log('\n📈 Execution Flow:')
      result.steps.forEach((step, index) => {
        console.log(`  ${index + 1}. ${step.agent} → ${step.result.substring(0, 60)}...`)
      })

      console.log('\n📚 Complete Analysis:')
      console.log('=====================')
      console.log(result.finalResult)
    } else {
      console.error('❌ Mathematical analysis failed:', result.error?.message)
    }

  } catch (error) {
    console.error('❌ Demo failed:', error)
  }
}

/**
 * DEMO 3: QiPrompt Direct Usage
 * 
 * Shows how to use QiPrompt directly for simple LLM interactions
 */
async function demonstrateQiPromptDirect() {
  console.log('\n🎯 Demo 3: QiPrompt Direct Usage')
  console.log('================================\n')

  try {
    const qiPrompt = new QiPrompt({
      defaultTemperature: 0.6,
      timeout: 10000
    })

    // Simple text generation
    console.log('📝 Simple text generation:')
    const simpleResult = await qiPrompt.generateText(
      "Explain the concept of functional programming in one paragraph."
    )
    console.log(`Result: ${simpleResult}\n`)

    // Structured generation
    console.log('🔧 Structured generation:')
    
    const analysis = await qiPrompt.generateStructured<{
      summary: string
      keyPoints: string[]
      confidence: number
      recommendation: string
    }>(
      "Analyze the pros and cons of using microservices architecture.",
      { schema: CommonSchemas.analysis }
    )
    
    console.log('Analysis Results:')
    console.log(`📋 Summary: ${analysis.summary}`)
    console.log(`🔑 Key Points: ${analysis.keyPoints.join(', ')}`)
    console.log(`📊 Confidence: ${analysis.confidence}`)
    console.log(`💡 Recommendation: ${analysis.recommendation}\n`)

  } catch (error) {
    console.error('❌ QiPrompt demo failed:', error)
  }
}

/**
 * DEMO 4: Architecture Benefits
 * 
 * Shows the benefits of the 4-layer architecture
 */
function demonstrateArchitectureBenefits() {
  console.log('🏗️  Demo 4: Architecture Benefits')
  console.log('================================\n')

  console.log('🎯 4-Layer Architecture Benefits:')
  console.log('')
  
  console.log('✅ **Simplified APIs**')
  console.log('   • QiAgent.executeWorkflow() vs complex AI Orchestra setup')
  console.log('   • QiPrompt.generateText() vs manual Vercel AI SDK configuration')
  console.log('')
  
  console.log('✅ **Built-in Reliability**') 
  console.log('   • Automatic retry logic in QiPrompt')
  console.log('   • Error handling across all layers')
  console.log('   • Workflow state management in QiAgent')
  console.log('')
  
  console.log('✅ **Easy Customization**')
  console.log('   • Pre-built workflow patterns')
  console.log('   • Custom agent role definitions') 
  console.log('   • Flexible model provider switching')
  console.log('')
  
  console.log('✅ **Type Safety**')
  console.log('   • End-to-end TypeScript support')
  console.log('   • Structured output validation')
  console.log('   • Clear interface definitions')
  console.log('')
  
  console.log('✅ **Production Ready**')
  console.log('   • Proper separation of concerns')
  console.log('   • Streaming support throughout')
  console.log('   • Zero vendor lock-in')
  console.log('')
}

/**
 * Main demo function
 */
async function main() {
  console.log('🎭 QiAgent + QiPrompt Workflow Demo')
  console.log('==================================\n')

  console.log('This demo showcases the 4-layer architecture:')
  console.log('Your App → QiAgent → AI Orchestra → QiPrompt → Vercel AI SDK → LLM')
  console.log('')

  try {
    // Architecture overview
    demonstrateArchitectureBenefits()

    console.log('⚠️  The following demos require a configured language model.')
    console.log('   For real execution, configure QiPrompt with your model:')
    console.log('   const qiPrompt = new QiPrompt({ defaultModel: ollama("llama3.2") })')
    console.log('')

    // Run demonstrations (with mock implementations)
    await demonstrateWorkflowPatterns()
    await demonstrateCustomWorkflow() 
    await demonstrateQiPromptDirect()

    console.log('\n🎉 All demos completed successfully!')
    console.log('')
    console.log('🔑 Key Takeaways:')
    console.log('• QiAgent simplifies multi-agent workflow creation')
    console.log('• QiPrompt provides reliable LLM interactions')  
    console.log('• 4-layer architecture offers maximum flexibility')
    console.log('• Type safety and error handling throughout')
    console.log('• Ready for production with real models')

  } catch (error) {
    console.error('❌ Demo suite failed:', error)
  }
}

// Run the demo
main() 