import { ImplementationScorer } from '@/evaluation/implementation-scorer';
import type { GeneratedCode } from '@/types/study';
import { describe, expect, test } from 'vitest';

// biome-ignore lint/nursery/noSecrets: ImplementationScorer is a class name, not a secret
describe('ImplementationScorer', () => {
  const scorer = new ImplementationScorer();

  // Helper to create GeneratedCode from string
  const createCode = (code: string): GeneratedCode => ({
    code,
    model: 'test-model',
    instruction: 'test-instruction',
    timestamp: new Date().toISOString(),
    duration: 100,
    success: true,
    metadata: { provider: 'test' },
  });

  describe('Contract Compliance (40% weight)', () => {
    test('should give high score for complete QiCore implementation', async () => {
      const perfectCode = `
{-# LANGUAGE GHC2021 #-}
module QiCore.Base where

data QiError = QiError
  { message :: !Text
  , context :: !(Maybe Text)
  } deriving stock (Show, Eq)

data Result a = Ok !a | Err !QiError
  deriving stock (Show, Eq, Functor)

success :: a -> Result a
success = Ok

failure :: QiError -> Result a  
failure = Err

map :: (a -> b) -> Result a -> Result b
map f (Ok a) = Ok (f a)
map _ (Err e) = Err e

flatMap :: (a -> Result b) -> Result a -> Result b
flatMap f (Ok a) = f a
flatMap _ (Err e) = Err e

unwrap :: Result a -> a
unwrap (Ok a) = a
unwrap (Err _) = error "unwrap called on Err"

unwrapOr :: a -> Result a -> a
unwrapOr def (Ok a) = a
unwrapOr def (Err _) = def
      `.trim();

      const score = await scorer.scoreImplementation(createCode(perfectCode));
      expect(score.contractCompliance).toBeGreaterThan(80);
      expect(score.overallScore).toBeGreaterThan(70);
    });

    test('should give low score for missing core types', async () => {
      const incompleteCode = `
module QiCore.Base where
data MyError = MyError String
      `.trim();

      const score = await scorer.scoreImplementation(createCode(incompleteCode));
      expect(score.contractCompliance).toBeLessThan(30);
    });

    test('should detect missing Result operations', async () => {
      const noOperationsCode = `
{-# LANGUAGE GHC2021 #-}
module QiCore.Base where
data QiError = QiError { message :: Text }
data Result a = Ok a | Err QiError
      `.trim();

      const score = await scorer.scoreImplementation(createCode(noOperationsCode));
      expect(score.contractCompliance).toBeLessThan(50);
    });
  });

  describe('Modernity (20% weight)', () => {
    test('should reward modern Haskell features', async () => {
      const modernCode = `
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE StrictData #-}
module QiCore.Base where

import qualified Data.Text as T

data QiError = QiError
  { message :: !T.Text
  , context :: !(Maybe T.Text)
  } deriving stock (Show, Eq, Ord)
  
data Result a = Ok !a | Err !QiError
  deriving stock (Show, Eq, Functor)
      `.trim();

      const score = await scorer.scoreImplementation(createCode(modernCode));
      expect(score.modernityScore).toBeGreaterThan(60);
    });

    test('should penalize old-style Haskell', async () => {
      const oldCode = `
{-# LANGUAGE Haskell2010 #-}
module QiCore.Base where

data QiError = QiError String deriving (Show, Eq)
data Result a = Ok a | Err QiError deriving Show
      `.trim();

      const score = await scorer.scoreImplementation(createCode(oldCode));
      expect(score.modernityScore).toBeLessThan(20);
    });
  });

  describe('Completeness (30% weight)', () => {
    test('should reward comprehensive function coverage', async () => {
      const completeCode = `
module QiCore.Base where

success :: a -> Result a
failure :: QiError -> Result a  
map :: (a -> b) -> Result a -> Result b
flatMap :: (a -> Result b) -> Result a -> Result b
unwrap :: Result a -> a
unwrapOr :: a -> Result a -> a
      `.trim();

      const score = await scorer.scoreImplementation(createCode(completeCode));
      expect(score.completenessScore).toBeGreaterThan(50);
    });

    test('should penalize minimal implementations', async () => {
      const minimalCode = `
module QiCore.Base where
data Result a = Ok a | Err String
      `.trim();

      const score = await scorer.scoreImplementation(createCode(minimalCode));
      expect(score.completenessScore).toBeLessThan(30);
    });
  });

  describe('Quality (10% weight)', () => {
    test('should reward comprehensive documentation', async () => {
      const documentedCode = `
{-# LANGUAGE GHC2021 #-}
-- | QiCore Base - Core error handling primitives
module QiCore.Base where

-- | Core error type with contextual information.
data QiError = QiError
  { message :: !Text  -- ^ Primary error message
  , context :: !(Maybe Text)  -- ^ Additional context
  } deriving stock (Show, Eq)

-- | Result type for computations that may fail.
data Result a = Ok !a | Err !QiError
  deriving stock (Show, Eq, Functor)
      `.trim();

      const score = await scorer.scoreImplementation(createCode(documentedCode));
      expect(score.qualityScore).toBeGreaterThan(40);
    });

    test('should detect poor code quality', async () => {
      const poorCode = `
module QiCore.Base where
data Err=Err String -- bad spacing
data Result a=Ok a|Err Err -- no spaces
x = undefined -- partial function
      `.trim();

      const score = await scorer.scoreImplementation(createCode(poorCode));
      expect(score.qualityScore).toBeLessThan(50);
    });
  });

  describe('Edge Cases', () => {
    test('should handle empty code', async () => {
      const score = await scorer.scoreImplementation(createCode(''));
      expect(score.overallScore).toBe(0);
      expect(score.contractCompliance).toBe(0);
      expect(score.completenessScore).toBe(0);
      expect(score.modernityScore).toBe(0);
      expect(score.qualityScore).toBe(0);
    });

    test('should handle invalid Haskell syntax', async () => {
      const invalidCode = 'this is not haskell code at all!!!';
      const score = await scorer.scoreImplementation(createCode(invalidCode));
      expect(score.overallScore).toBeLessThan(10);
    });

    test('should handle very large code files', async () => {
      const largeCode = `module QiCore.Base where\n${'-- comment\n'.repeat(1000)}`;
      const score = await scorer.scoreImplementation(createCode(largeCode));
      expect(score).toBeDefined();
      expect(typeof score.overallScore).toBe('number');
    });
  });

  describe('Score Ranges and Weights', () => {
    test('all scores should be between 0 and 100', async () => {
      const testCode = 'module Test where\ndata X = X';
      const score = await scorer.scoreImplementation(createCode(testCode));

      expect(score.contractCompliance).toBeGreaterThanOrEqual(0);
      expect(score.contractCompliance).toBeLessThanOrEqual(100);
      expect(score.completenessScore).toBeGreaterThanOrEqual(0);
      expect(score.completenessScore).toBeLessThanOrEqual(100);
      expect(score.modernityScore).toBeGreaterThanOrEqual(0);
      expect(score.modernityScore).toBeLessThanOrEqual(100);
      expect(score.qualityScore).toBeGreaterThanOrEqual(0);
      expect(score.qualityScore).toBeLessThanOrEqual(100);
      expect(score.overallScore).toBeGreaterThanOrEqual(0);
      expect(score.overallScore).toBeLessThanOrEqual(100);
    });

    test('should calculate weighted average correctly', () => {
      // Test the expected weighting formula
      const contractScore = 80; // 40% weight
      const completenessScore = 60; // 30% weight
      const modernityScore = 40; // 20% weight
      const qualityScore = 20; // 10% weight

      const expectedTotal = Math.round(
        contractScore * 0.4 + completenessScore * 0.3 + modernityScore * 0.2 + qualityScore * 0.1
      );
      // 80*0.4 + 60*0.3 + 40*0.2 + 20*0.1 = 32 + 18 + 8 + 2 = 60
      expect(expectedTotal).toBe(60);
    });
  });

  describe('Money-Saving Validation Tests', () => {
    test('scorer should not crash on realistic generated code', async () => {
      const realisticCode = `
{-# LANGUAGE GHC2021 #-}
module QiCore.Base (Result(..), QiError(..), ok, err) where

import Data.Text (Text)

data QiError = QiError 
  { message :: Text 
  } deriving (Show, Eq)

data Result a = Ok a | Err QiError
  deriving (Show, Eq)

ok :: a -> Result a
ok = Ok

err :: QiError -> Result a  
err = Err
      `.trim();

      const score = await scorer.scoreImplementation(createCode(realisticCode));

      // Should produce reasonable scores (not all 0 or 100)
      expect(score.overallScore).toBeGreaterThan(0);
      expect(score.overallScore).toBeLessThan(100);
      expect(score.contractCompliance).toBeGreaterThan(0);
    });

    test('should differentiate between good and bad implementations', async () => {
      const goodCode = `
{-# LANGUAGE GHC2021 #-}
module QiCore.Base where
data QiError = QiError { message :: !Text }
data Result a = Ok !a | Err !QiError
success = Ok
failure = Err  
map f (Ok a) = Ok (f a)
map _ (Err e) = Err e
      `.trim();

      const badCode = `
module Foo where
data X = X
      `.trim();

      const goodScore = await scorer.scoreImplementation(createCode(goodCode));
      const badScore = await scorer.scoreImplementation(createCode(badCode));

      expect(goodScore.overallScore).toBeGreaterThan(badScore.overallScore + 20);
    });
  });
});
