#!/usr/bin/env bun

/**
 * 4-Layer Wrapper Architecture Demo
 * 
 * Demonstrates: Your App → QiAgent → QiPrompt → Vercel AI SDK → LLM
 * 
 * This is the CORRECT way to use the wrappers for demos
 */

// Import from the simplified structure
import { createWorkflowOrchestra, createMathematicalWorkflow, executeWorkflow } from '../../../lib/src/qiagent/index.js'
import { QiPrompt } from '../../../lib/src/qiprompt/index.js'
import { z } from 'zod'

/**
 * DEMO 1: Simple QiPrompt Usage
 */
async function demonstrateQiPrompt() {
  console.log('1️⃣  QiPrompt Demo (Layer 3: Vercel AI SDK Wrapper)')
  console.log('================================================\n')

  const qiPrompt = new QiPrompt({
    defaultTemperature: 0.7,
    timeout: 15000
  })

  // Simple text generation
  console.log('📝 Simple text generation:')
  const result = await qiPrompt.generateText("Explain functional programming in one sentence.")
  console.log(`   Result: ${result}\n`)

  // Structured generation
  console.log('🔧 Structured generation:')
  console.log('   Note: Structured generation available with real Zod schemas')  
  console.log('   Example: qiPrompt.generateStructured(prompt, { schema: z.object({...}) })\n')
}

/**
 * DEMO 2: QiAgent Workflow
 */
async function demonstrateQiAgent() {
  console.log('2️⃣  QiAgent Demo (Layer 4: Workflow Wrapper)')
  console.log('==========================================\n')

  // Create QiPrompt instance
  const qiPrompt = new QiPrompt({
    defaultTemperature: 0.8
  })

  // Create QiAgent using QiPrompt
  const agent = createAgent(qiPrompt)

  // Register a workflow
  const contentWorkflow = WorkflowPatterns.contentCreation('demo-workflow')
  agent.registerWorkflow(contentWorkflow)

  console.log('📋 Registered workflows:', agent.getWorkflows().join(', '))
  
  console.log('\n🚀 Executing workflow...')
  const result = await agent.executeWorkflow(
    'demo-workflow',
    'Write a guide about clean code principles'
  )

  if (result.success) {
    console.log('\n✅ Workflow completed successfully!')
    console.log(`⏱️  Execution time: ${result.executionTime}ms`)
    console.log(`🔢 Steps: ${result.steps.length}`)
    
    console.log('\n📊 Step-by-step execution:')
    result.steps.forEach((step, index) => {
      console.log(`   ${index + 1}. ${step.agent} (${step.duration}ms)`)
      console.log(`      → ${step.result.substring(0, 80)}...`)
    })

    console.log('\n📝 Final Result:')
    console.log('================')
    console.log(result.finalResult)
  } else {
    console.error('❌ Workflow failed:', result.error?.message)
  }
}

/**
 * DEMO 3: Custom Agent Roles
 */
async function demonstrateCustomAgents() {
  console.log('\n3️⃣  Custom Agent Roles Demo')
  console.log('===========================\n')

  const qiPrompt = new QiPrompt()
  const agent = createAgent(qiPrompt)

  // Create custom workflow with specialized agents
  const codeReviewWorkflow = {
    name: 'code-review',
    description: 'Comprehensive code review process',
    agents: {
      analyzer: {
        name: 'Code Analyzer',
        description: 'Analyzes code structure and patterns',
        systemPrompt: 'You are a code analysis expert. Examine code for structure, patterns, and architecture.'
      },
      security: {
        name: 'Security Auditor', 
        description: 'Reviews code for security vulnerabilities',
        systemPrompt: 'You are a security expert. Identify potential security issues, vulnerabilities, and best practices.'
      },
      performance: {
        name: 'Performance Reviewer',
        description: 'Evaluates code for performance optimization',
        systemPrompt: 'You are a performance optimization expert. Review code for efficiency and speed improvements.'
      }
    },
    steps: {
      analyze: {
        agent: 'analyzer',
        nextSteps: ['security']
      },
      security: {
        agent: 'security', 
        nextSteps: ['performance']
      },
      performance: {
        agent: 'performance',
        nextSteps: []
      }
    },
    startStep: 'analyze'
  }

  agent.registerWorkflow(codeReviewWorkflow)

  console.log('🔍 Executing code review workflow...')
  const result = await agent.executeWorkflow(
    'code-review',
    `
    function processData(data) {
      let result = [];
      for (let i = 0; i < data.length; i++) {
        if (data[i].active) {
          result.push(data[i]);
        }
      }
      return result;
    }
    `
  )

  if (result.success) {
    console.log('\n✅ Code review completed!')
    console.log(`📊 Reviewed by ${result.steps.length} specialized agents`)
    
    result.steps.forEach((step, index) => {
      console.log(`\n${index + 1}. ${step.agent}:`)
      console.log(`   ${step.result}`)
    })
  }
}

/**
 * DEMO 4: Architecture Benefits
 */
function demonstrateArchitectureBenefits() {
  console.log('\n4️⃣  Architecture Benefits')
  console.log('========================\n')

  console.log('🏗️  4-Layer Wrapper Architecture:')
  console.log('')
  console.log('   Your App (Layer 1)')
  console.log('       ↓')
  console.log('   QiAgent (Layer 2) ← Simple workflow management')
  console.log('       ↓')
  console.log('   QiPrompt (Layer 3) ← LLM interaction wrapper')
  console.log('       ↓')
  console.log('   Vercel AI SDK (Layer 4) ← Provider abstraction')
  console.log('       ↓')
  console.log('   LLM Provider (Ollama, OpenAI, etc.)')
  console.log('')
  
  console.log('✅ Benefits:')
  console.log('• **Simplicity**: One-line workflow execution')
  console.log('• **Flexibility**: Easy to switch providers or models')
  console.log('• **Reliability**: Built-in retry and error handling')
  console.log('• **Type Safety**: Full TypeScript support')
  console.log('• **Production Ready**: Clean separation of concerns')
  console.log('')

  console.log('🔧 Development Benefits:')
  console.log('• Mock implementations for testing')
  console.log('• Clear API boundaries')
  console.log('• Easy to extend and customize')
  console.log('• Minimal boilerplate code')
}

/**
 * Main demo runner
 */
async function main() {
  console.log('🎭 4-Layer Wrapper Architecture Demo')
  console.log('===================================\n')

  console.log('This demo showcases the proper wrapper usage:')
  console.log('Your App → QiAgent → QiPrompt → Vercel AI SDK → LLM\n')

  try {
    // Show architecture benefits first
    demonstrateArchitectureBenefits()

    // Note about mock implementations
    console.log('\n⚠️  Note: These demos use mock implementations.')
    console.log('   For production, configure with real models:')
    console.log('')
    console.log('   ```typescript')
    console.log('   import { ollama } from "ai/ollama"')
    console.log('   const qiPrompt = new QiPrompt({')
    console.log('     defaultModel: ollama("llama3.2")')
    console.log('   })')
    console.log('   ```')
    console.log('')

    // Run demos
    await demonstrateQiPrompt()
    await demonstrateQiAgent()
    await demonstrateCustomAgents()

    console.log('\n🎉 All wrapper demos completed!')
    console.log('')
    console.log('🔑 Key Takeaways:')
    console.log('• QiPrompt simplifies LLM interactions')
    console.log('• QiAgent handles workflow orchestration')
    console.log('• 4-layer architecture provides flexibility')
    console.log('• Easy to test with mock implementations')
    console.log('• Production-ready with real model configuration')
    console.log('')
    console.log('🚀 Ready to build sophisticated AI workflows!')

  } catch (error) {
    console.error('❌ Demo failed:', error)
  }
}

// Run the demo
main() 