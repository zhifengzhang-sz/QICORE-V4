#!/usr/bin/env bun

import { existsSync, mkdirSync } from 'node:fs';
import { StudyRunner } from './src/generators/study-runner';
import { StudyDatabase } from './src/database/study-db';
import { getStudyConfig } from './src/config/study-config';
import { studyConfigSchema } from './src/types/study';

async function main() {
  console.log('🚀 Starting Haskell AI Consistency Study...\n');

  try {
    // Load study configuration from YAML
    console.log('📋 Loading study configuration...');
    const config = await getStudyConfig();
    
    // Validate configuration
    const configResult = studyConfigSchema.safeParse(config);
    if (!configResult.success) {
      console.error('❌ Invalid study configuration:');
      console.error(configResult.error.issues);
      process.exit(1);
    }

    const validatedConfig = configResult.data;
    console.log(`📋 Study: ${validatedConfig.name}`);
    console.log(`📝 Description: ${validatedConfig.description}`);
    console.log(`🤖 Models: ${validatedConfig.models.length}`);
    console.log(`📄 Instructions: ${validatedConfig.instructions.length}`);
    console.log(`🔄 Runs per combination: ${validatedConfig.runsPerCombination}`);
    console.log(`📁 Output directory: ${validatedConfig.outputDir}\n`);

    // Create output directory
    if (!existsSync(validatedConfig.outputDir)) {
      mkdirSync(validatedConfig.outputDir, { recursive: true });
      console.log(`✅ Created output directory: ${validatedConfig.outputDir}`);
    }

    // Report instruction loading status
    for (const instruction of validatedConfig.instructions) {
      if (instruction.content && instruction.content.length > 50) {
        console.log(`📖 Loaded instruction: ${instruction.name} (${instruction.content.length} chars)`);
      } else {
        console.warn(`⚠️  Instruction content missing or too short: ${instruction.name}`);
      }
    }

    // Initialize database
    const db = new StudyDatabase(':memory:'); // In-memory for now
    console.log('💾 Database initialized\n');

    // Initialize study runner (for future use)
    const _runner = new StudyRunner(validatedConfig, db);
    
    // Calculate total combinations
    const totalCombinations = validatedConfig.models.length * validatedConfig.instructions.length;
    const totalRuns = totalCombinations * validatedConfig.runsPerCombination;
    
    console.log(`🎯 Study Plan:`);
    console.log(`   Models × Instructions = ${validatedConfig.models.length} × ${validatedConfig.instructions.length} = ${totalCombinations} combinations`);
    console.log(`   Total runs = ${totalCombinations} × ${validatedConfig.runsPerCombination} = ${totalRuns} runs`);
    console.log(`   Estimated time: ${Math.ceil(totalRuns * 30 / 60)} minutes\n`);

    // For now, let's do a dry run to test our framework
    console.log('🧪 Running DRY RUN (no actual AI calls)...\n');
    
    let runNumber = 1;
    for (const model of validatedConfig.models) {
      for (const instruction of validatedConfig.instructions) {
        console.log(`\n📝 Testing: ${model.name} × ${instruction.name}`);
        
        for (let run = 1; run <= validatedConfig.runsPerCombination; run++) {
          console.log(`   Run ${run}/${validatedConfig.runsPerCombination} (${runNumber}/${totalRuns})`);
          
          // Simulate code generation (replace with actual AI call later)
          const mockCode = generateMockHaskellCode(instruction.category);
          
          // Simulate evaluation
          const mockScores = {
            syntactic: 85 + Math.random() * 10,
            semantic: 80 + Math.random() * 15, 
            modern: 75 + Math.random() * 20,
            completeness: 90 + Math.random() * 10,
            documentation: 70 + Math.random() * 25,
            performance: 85 + Math.random() * 10,
            overall: 82 + Math.random() * 12,
          };

          console.log(`   ✅ Generated ${mockCode.split('\n').length} lines, Overall Score: ${mockScores.overall.toFixed(1)}`);
          runNumber++;
        }
      }
    }

    console.log('\n🎉 Dry run completed successfully!');
    console.log('\n📊 Next steps:');
    console.log('   1. Add AI API credentials (ANTHROPIC_API_KEY, OPENAI_API_KEY)');
    console.log('   2. Replace mock generation with actual AI calls');
    console.log('   3. Implement real code evaluation (syntax checking, compilation)');
    console.log('   4. Run statistical analysis on results');
    console.log('   5. Generate consistency report\n');
    
  } catch (error) {
    console.error('❌ Study execution failed:', error);
    process.exit(1);
  }
}

function generateMockHaskellCode(category: string): string {
  if (category === 'simple') {
    return `module ListUtils (safeHead, safeTail, safeIndex, safeReverse) where

-- | Safely get the first element of a list
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Safely get the tail of a list  
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

-- | Safely get element at index
safeIndex :: [a] -> Int -> Maybe a
safeIndex [] _ = Nothing
safeIndex (x:_) 0 = Just x
safeIndex (_:xs) n
  | n < 0 = Nothing
  | otherwise = safeIndex xs (n - 1)

-- | Reverse a list safely
safeReverse :: [a] -> [a]
safeReverse = reverse`;
  } else {
    return `module Calculator (Expr(..), eval, prettyPrint, simplify) where

data Expr = 
    Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Eq, Show)

-- | Evaluate expression with division by zero handling
eval :: Expr -> Maybe Double
eval (Num x) = Just x
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2
eval (Sub e1 e2) = (-) <$> eval e1 <*> eval e2
eval (Mul e1 e2) = (*) <$> eval e1 <*> eval e2
eval (Div e1 e2) = do
  v1 <- eval e1
  v2 <- eval e2
  if v2 == 0 then Nothing else Just (v1 / v2)

-- | Pretty print with parentheses
prettyPrint :: Expr -> String
prettyPrint (Num x) = show x
prettyPrint (Add e1 e2) = "(" ++ prettyPrint e1 ++ " + " ++ prettyPrint e2 ++ ")"
prettyPrint (Sub e1 e2) = "(" ++ prettyPrint e1 ++ " - " ++ prettyPrint e2 ++ ")"
prettyPrint (Mul e1 e2) = "(" ++ prettyPrint e1 ++ " * " ++ prettyPrint e2 ++ ")"
prettyPrint (Div e1 e2) = "(" ++ prettyPrint e1 ++ " / " ++ prettyPrint e2 ++ ")"

-- | Basic algebraic simplification
simplify :: Expr -> Expr
simplify (Add (Num 0) e) = simplify e
simplify (Add e (Num 0)) = simplify e
simplify (Mul (Num 1) e) = simplify e
simplify (Mul e (Num 1)) = simplify e
simplify (Mul (Num 0) _) = Num 0
simplify (Mul _ (Num 0)) = Num 0
simplify e = e`;
  }
}

// Run the study
if (import.meta.main) {
  main().catch(console.error);
} 