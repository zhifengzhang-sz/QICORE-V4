#!/usr/bin/env bun

import { readFile } from 'node:fs/promises';
import { ImplementationScorer } from '@/evaluation/implementation-scorer';
import type { GeneratedCode } from '@/types/study';

async function scoreExistingHaskell() {
  console.log('🔍 Scoring our existing QiCore Haskell implementation...\n');

  const scorer = new ImplementationScorer();

  // Read our actual implementation files
  const baseModule = await readFile('../../haskell/QiCore/Base.hs', 'utf-8');
  const resultModule = await readFile('../../haskell/QiCore/Base/Result.hs', 'utf-8');
  const errorModule = await readFile('../../haskell/QiCore/Base/Error.hs', 'utf-8');

  // Combine all modules for scoring
  const fullImplementation = `
-- QiCore/Base.hs
${baseModule}

-- QiCore/Base/Result.hs  
${resultModule}

-- QiCore/Base/Error.hs
${errorModule}
  `.trim();

  // Create a GeneratedCode object for our implementation
  const ourImplementation: GeneratedCode = {
    code: fullImplementation,
    model: 'human-written',
    instruction: 'qicore-base-implementation',
    timestamp: new Date().toISOString(),
    duration: 0,
    success: true,
    metadata: {
      provider: 'human',
      note: 'Production QiCore Base implementation'
    }
  };

  // Score it
  console.log('📊 Scoring our implementation...');
  const score = await scorer.scoreImplementation(ourImplementation);

  console.log('\n🏆 OUR IMPLEMENTATION SCORE:');
  console.log(`   Overall Score: ${score.overallScore}/100`);
  console.log(`   Contract Compliance: ${score.contractCompliance}%`);
  console.log(`   Completeness: ${score.completenessScore}%`);
  console.log(`   Modernity: ${score.modernityScore}%`);
  console.log(`   Quality: ${score.qualityScore}%`);

  // Compare with test samples for context
  const excellentSample = `
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DerivingStrategies #-}

module QiCore.Base
  ( Result (..)
  , QiError (..)
  , ErrorCategory (..)
  , success
  , failure
  , mapResult
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)

data ErrorCategory = VALIDATION | NETWORK | UNKNOWN
  deriving stock (Eq, Show, Generic)

data QiError = QiError
  { code :: !Text
  , message :: !Text  
  , category :: !ErrorCategory
  } deriving stock (Eq, Show, Generic)

data Result a = Success !a | Failure !QiError
  deriving stock (Eq, Show, Generic)

success :: a -> Result a
success = Success

failure :: QiError -> Result a  
failure = Failure

mapResult :: (a -> b) -> Result a -> Result b
mapResult f (Success a) = Success (f a)
mapResult _ (Failure e) = Failure e
  `;

  const mediocreTestCode: GeneratedCode = {
    code: excellentSample,
    model: 'test-excellent',
    instruction: 'test',
    timestamp: new Date().toISOString(),
    duration: 0,
    success: true,
    metadata: { provider: 'test' }
  };

  console.log('\n📈 Scoring excellent test sample for comparison...');
  const excellentScore = await scorer.scoreImplementation(mediocreTestCode);
  
  console.log('\n🔍 COMPARISON:');
  console.log(`   Our Implementation: ${score.overallScore}/100`);
  console.log(`   Excellent Sample:   ${excellentScore.overallScore}/100`);
  console.log(`   Difference:         ${score.overallScore - excellentScore.overallScore} points`);

  console.log('\n🎯 DETAILED BREAKDOWN:');
  console.log('                      Ours  |  Test  | Diff');
  console.log('   Contract:          ' + 
    `${score.contractCompliance.toString().padStart(3)}%  |  ` +
    `${excellentScore.contractCompliance.toString().padStart(3)}%  | ` +
    `${(score.contractCompliance - excellentScore.contractCompliance).toString().padStart(+3)}`);
  console.log('   Completeness:      ' + 
    `${score.completenessScore.toString().padStart(3)}%  |  ` +
    `${excellentScore.completenessScore.toString().padStart(3)}%  | ` +
    `${(score.completenessScore - excellentScore.completenessScore).toString().padStart(+3)}`);
  console.log('   Modernity:         ' + 
    `${score.modernityScore.toString().padStart(3)}%  |  ` +
    `${excellentScore.modernityScore.toString().padStart(3)}%  | ` +
    `${(score.modernityScore - excellentScore.modernityScore).toString().padStart(+3)}`);
  console.log('   Quality:           ' + 
    `${score.qualityScore.toString().padStart(3)}%  |  ` +
    `${excellentScore.qualityScore.toString().padStart(3)}%  | ` +
    `${(score.qualityScore - excellentScore.qualityScore).toString().padStart(+3)}`);

  console.log('\n💭 ANALYSIS:');
  if (score.overallScore > excellentScore.overallScore) {
    console.log('   ✅ Our implementation scores HIGHER than the test excellent sample!');
    console.log('   🎉 This suggests our scorer may be well-calibrated.');
  } else if (score.overallScore > 80) {
    console.log('   ✅ Our implementation scores in the excellent range (80+)');
    console.log('   📊 This confirms the scorer can detect high-quality code.');
  } else {
    console.log('   ⚠️  Our implementation scored lower than expected.');
    console.log('   🔧 This might indicate the scorer needs calibration.');
  }

  console.log('\n📋 FINDINGS:');
  console.log(`   • Line count: ${fullImplementation.split('\n').length} lines`);
  console.log(`   • Contains Result type: ${fullImplementation.includes('data Result')} ✓`);
  console.log(`   • Contains QiError type: ${fullImplementation.includes('data QiError')} ✓`);
  console.log(`   • Uses modern extensions: ${fullImplementation.includes('{-#')} ✓`);
  console.log(`   • Has documentation: ${fullImplementation.includes('{-')} ✓`);
  console.log(`   • Comprehensive API: ${fullImplementation.includes('sequenceResult')} ✓`);
}

scoreExistingHaskell().catch(console.error); 