/**
 * Mock responses and fixtures for testing
 */

export const MOCK_HASKELL_CONTENT = `{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | QiCore Cache Module
--
-- This module provides a high-performance caching system with TTL support,
-- LRU eviction, and thread-safe operations using STM.
--
-- Example usage:
-- >>> cache <- createMemoryCache 100
-- >>> result <- set cache "key" "value" 60
-- >>> value <- get cache "key"
module QiCore.Core.Cache
  ( -- * Cache Types
    Cache (..)
  , CacheConfig (..)
  , TTL

    -- * Construction
  , createMemoryCache
  , createPersistentCache
  , defaultConfig

    -- * Operations
  , get
  , set
  , delete
  , clear
  , getOrSet

    -- * Information
  , size
  , capacity
  , stats
  ) where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar, modifyTVar')
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import GHC.Generics (Generic)
import QiCore.Base (Result, QiError, success, failure)
import QiCore.Base.Error qualified as Error

-- | Time-to-live in seconds
type TTL = Int

-- | Cache statistics
data CacheStats = CacheStats
  { hits :: !Int
  , misses :: !Int
  , evictions :: !Int
  , size :: !Int
  } deriving stock (Eq, Show, Generic)

-- | Cache entry with TTL and access time
data CacheEntry a = CacheEntry
  { value :: !a
  , expiresAt :: !UTCTime
  , lastAccessed :: !UTCTime
  } deriving stock (Eq, Show, Generic)

-- | Cache configuration
data CacheConfig = CacheConfig
  { maxCapacity :: !Int
  , defaultTTL :: !TTL
  , enableStats :: !Bool
  } deriving stock (Eq, Show, Generic)

-- | Default cache configuration
defaultConfig :: CacheConfig
defaultConfig = CacheConfig
  { maxCapacity = 1000
  , defaultTTL = 3600  -- 1 hour
  , enableStats = True
  }

-- | Thread-safe cache implementation
data Cache k v = Cache
  { cacheData :: !(TVar (Map.Map k (CacheEntry v)))
  , cacheStats :: !(TVar CacheStats)
  , cacheConfig :: !CacheConfig
  } deriving stock (Generic)

-- | Create an in-memory cache with specified capacity
createMemoryCache :: (Ord k, MonadIO m) => Int -> m (Cache k v)
createMemoryCache capacity = liftIO $ do
  dataVar <- newTVar Map.empty
  statsVar <- newTVar (CacheStats 0 0 0 0)
  let config = defaultConfig { maxCapacity = capacity }
  pure Cache
    { cacheData = dataVar
    , cacheStats = statsVar
    , cacheConfig = config
    }

-- | Create a persistent cache (placeholder for future implementation)
createPersistentCache :: (Ord k, MonadIO m) => FilePath -> Int -> m (Result (Cache k v))
createPersistentCache _path _capacity = do
  -- TODO: Implement persistent storage backend
  error <- Error.create
    "NOT_IMPLEMENTED"
    "Persistent cache not yet implemented"
    Error.UNKNOWN
    Nothing
    Nothing
  pure $ failure error

-- | Get value from cache
get :: (Ord k, MonadIO m) => Cache k v -> k -> m (Result (Maybe v))
get cache key = liftIO $ do
  now <- getCurrentTime
  atomically $ do
    dataMap <- readTVar (cacheData cache)
    case Map.lookup key dataMap of
      Nothing -> do
        when (enableStats $ cacheConfig cache) $
          modifyTVar' (cacheStats cache) $ \\stats ->
            stats { misses = misses stats + 1 }
        pure $ success Nothing
      
      Just entry
        | expiresAt entry < now -> do
            -- Entry expired, remove it
            writeTVar (cacheData cache) (Map.delete key dataMap)
            when (enableStats $ cacheConfig cache) $
              modifyTVar' (cacheStats cache) $ \\stats ->
                stats { misses = misses stats + 1, evictions = evictions stats + 1 }
            pure $ success Nothing
        
        | otherwise -> do
            -- Entry valid, update access time and stats
            let updatedEntry = entry { lastAccessed = now }
            writeTVar (cacheData cache) (Map.insert key updatedEntry dataMap)
            when (enableStats $ cacheConfig cache) $
              modifyTVar' (cacheStats cache) $ \\stats ->
                stats { hits = hits stats + 1 }
            pure $ success (Just $ value entry)

-- | Set value in cache with TTL
set :: (Ord k, MonadIO m) => Cache k v -> k -> v -> TTL -> m (Result ())
set cache key val ttlSeconds = liftIO $ do
  now <- getCurrentTime
  let expiryTime = addUTCTime (fromIntegral ttlSeconds) now
  let entry = CacheEntry val expiryTime now
  
  atomically $ do
    dataMap <- readTVar (cacheData cache)
    let newMap = Map.insert key entry dataMap
    
    -- Check if we need to evict entries
    finalMap <- if Map.size newMap > maxCapacity (cacheConfig cache)
                then evictLRU newMap (maxCapacity $ cacheConfig cache)
                else pure newMap
    
    writeTVar (cacheData cache) finalMap
    pure $ success ()
  where
    evictLRU :: Map.Map k (CacheEntry v) -> Int -> STM (Map.Map k (CacheEntry v))
    evictLRU mp maxSize = do
      if Map.size mp <= maxSize
        then pure mp
        else do
          -- Find LRU entry and remove it
          let oldestKey = fst $ Map.foldrWithKey
                (\\k entry (minKey, minTime) ->
                  if lastAccessed entry < minTime
                    then (k, lastAccessed entry)
                    else (minKey, minTime))
                (fst $ Map.findMin mp, lastAccessed $ snd $ Map.findMin mp)
                mp
          when (enableStats $ cacheConfig cache) $
            modifyTVar' (cacheStats cache) $ \\stats ->
              stats { evictions = evictions stats + 1 }
          pure $ Map.delete oldestKey mp

-- | Delete value from cache
delete :: (Ord k, MonadIO m) => Cache k v -> k -> m (Result Bool)
delete cache key = liftIO $ atomically $ do
  dataMap <- readTVar (cacheData cache)
  case Map.lookup key dataMap of
    Nothing -> pure $ success False
    Just _ -> do
      writeTVar (cacheData cache) (Map.delete key dataMap)
      pure $ success True

-- | Clear all entries from cache
clear :: MonadIO m => Cache k v -> m (Result ())
clear cache = liftIO $ atomically $ do
  writeTVar (cacheData cache) Map.empty
  writeTVar (cacheStats cache) (CacheStats 0 0 0 0)
  pure $ success ()

-- | Get value or set it if not present (atomic operation)
getOrSet :: (Ord k, MonadIO m) => Cache k v -> k -> m (Result v) -> TTL -> m (Result v)
getOrSet cache key computeValue ttlSeconds = do
  getResult <- get cache key
  case getResult of
    Left err -> pure $ failure err
    Right (Just val) -> pure $ success val
    Right Nothing -> do
      computeResult <- computeValue
      case computeResult of
        Left err -> pure $ failure err
        Right val -> do
          setResult <- set cache key val ttlSeconds
          case setResult of
            Left err -> pure $ failure err
            Right () -> pure $ success val

-- | Get current cache size
size :: MonadIO m => Cache k v -> m Int
size cache = liftIO $ atomically $ do
  dataMap <- readTVar (cacheData cache)
  pure $ Map.size dataMap

-- | Get cache capacity
capacity :: Cache k v -> Int
capacity = maxCapacity . cacheConfig

-- | Get cache statistics
stats :: MonadIO m => Cache k v -> m CacheStats
stats cache = liftIO $ atomically $ readTVar (cacheStats cache)

-- Type class instances

instance Functor (Cache k) where
  fmap f cache = cache
    { cacheData = undefined -- This would require STM operations
    }

-- Note: Proper Functor instance would require more complex STM handling
-- This is a simplified version for demonstration
`;

export const MOCK_TYPESCRIPT_CONTENT = `import { z } from 'zod';

/**
 * User interface with comprehensive validation
 */
export const UserSchema = z.object({
  id: z.number().positive(),
  name: z.string().min(1).max(100),
  email: z.string().email(),
  age: z.number().min(0).max(150).optional(),
  roles: z.array(z.string()).default([]),
});

export type User = z.infer<typeof UserSchema>;

/**
 * Result type for operations that can fail
 */
export type Result<T, E = Error> = 
  | { success: true; data: T }
  | { success: false; error: E };

/**
 * Create a successful result
 */
export function success<T>(data: T): Result<T> {
  return { success: true, data };
}

/**
 * Create a failed result
 */
export function failure<E>(error: E): Result<never, E> {
  return { success: false, error };
}

/**
 * User service with comprehensive error handling
 */
export class UserService {
  private users: Map<number, User> = new Map();
  private nextId = 1;

  /**
   * Create a new user with validation
   */
  async createUser(userData: unknown): Promise<Result<User, string>> {
    try {
      const validatedUser = UserSchema.parse({
        ...userData,
        id: this.nextId++,
      });

      this.users.set(validatedUser.id, validatedUser);
      return success(validatedUser);
    } catch (error) {
      if (error instanceof z.ZodError) {
        return failure(\`Validation failed: \${error.message}\`);
      }
      return failure('Unknown error occurred');
    }
  }

  /**
   * Get user by ID
   */
  async getUserById(id: number): Promise<Result<User, string>> {
    const user = this.users.get(id);
    if (!user) {
      return failure(\`User with ID \${id} not found\`);
    }
    return success(user);
  }

  /**
   * Update user with partial data
   */
  async updateUser(id: number, updates: Partial<Omit<User, 'id'>>): Promise<Result<User, string>> {
    const existingUser = this.users.get(id);
    if (!existingUser) {
      return failure(\`User with ID \${id} not found\`);
    }

    try {
      const updatedUser = UserSchema.parse({
        ...existingUser,
        ...updates,
      });

      this.users.set(id, updatedUser);
      return success(updatedUser);
    } catch (error) {
      if (error instanceof z.ZodError) {
        return failure(\`Validation failed: \${error.message}\`);
      }
      return failure('Unknown error occurred');
    }
  }

  /**
   * Delete user by ID
   */
  async deleteUser(id: number): Promise<Result<boolean, string>> {
    const deleted = this.users.delete(id);
    if (!deleted) {
      return failure(\`User with ID \${id} not found\`);
    }
    return success(true);
  }

  /**
   * Get all users with optional filtering
   */
  async getAllUsers(filter?: (user: User) => boolean): Promise<Result<User[], never>> {
    const allUsers = Array.from(this.users.values());
    const filteredUsers = filter ? allUsers.filter(filter) : allUsers;
    return success(filteredUsers);
  }
}

/**
 * Utility functions for Result type
 */
export const ResultUtils = {
  /**
   * Map over the success value
   */
  map: <T, U, E>(result: Result<T, E>, fn: (value: T) => U): Result<U, E> => {
    return result.success ? success(fn(result.data)) : result;
  },

  /**
   * Chain operations that return Results
   */
  flatMap: <T, U, E>(result: Result<T, E>, fn: (value: T) => Result<U, E>): Result<U, E> => {
    return result.success ? fn(result.data) : result;
  },

  /**
   * Handle both success and failure cases
   */
  match: <T, U, E>(
    result: Result<T, E>,
    onSuccess: (value: T) => U,
    onFailure: (error: E) => U
  ): U => {
    return result.success ? onSuccess(result.data) : onFailure(result.error);
  },

  /**
   * Provide a default value for failures
   */
  getOrElse: <T, E>(result: Result<T, E>, defaultValue: T): T => {
    return result.success ? result.data : defaultValue;
  },
};
`;

export const MOCK_CLI_RESPONSES = {
  simple: {
    success: true,
    content: 'This is a simple response from Claude Code CLI.',
    error: null,
  },
  
  haskell: {
    success: true,
    content: MOCK_HASKELL_CONTENT,
    error: null,
  },
  
  typescript: {
    success: true,
    content: MOCK_TYPESCRIPT_CONTENT,
    error: null,
  },
  
  error: {
    success: false,
    content: null,
    error: 'Mock execution error for testing',
  },
  
  timeout: {
    success: false,
    content: null,
    error: 'Operation timed out after 5000ms',
  },
  
  invalidJson: {
    success: true,
    content: 'This is not valid JSON but should be handled gracefully',
    error: null,
  },
};

export const MOCK_SDK_RESPONSES = {
  streaming: [
    'Starting code generation...',
    'Analyzing requirements...',
    'Generating TypeScript module...',
    'Adding type safety features...',
    'Finalizing implementation...',
    'Code generation complete!',
  ],
  
  simple: [
    'Simple SDK response message',
  ],
  
  error: [], // Empty array to simulate no messages before error
};

export const MOCK_INSTRUCTIONS = {
  haskell: {
    workingDir: '/test/project',
    knowledgeUpdate: [
      'Modern Haskell 2024 best practices',
      'GHC2024 language extensions',
      'Performance optimization techniques',
    ],
    actionInstruction: 'Generate a high-performance Cache module following QiCore patterns',
    qualityStandards: [
      'GHC2024 language edition with modern extensions',
      'Strict performance annotations',
      'Rich Haddock documentation',
      'Mathematical law compliance',
    ],
    validationCriteria: [
      'Code style matches QiCore base components',
      'Implements comprehensive API',
      'Includes property-based tests',
      'Provides usage examples',
    ],
    context: {
      targetFile: 'haskell/QiCore/Core/Cache.hs',
      componentType: 'Cache',
    },
  },
  
  typescript: {
    workingDir: '/test/project',
    knowledgeUpdate: [
      'Modern TypeScript 5.7+ features',
      'Advanced type-level programming',
      'Runtime validation with Zod',
    ],
    actionInstruction: 'Generate a user service with comprehensive error handling',
    qualityStandards: [
      'Strict TypeScript configuration',
      'Runtime validation with Zod',
      'Functional programming patterns',
    ],
    validationCriteria: [
      'Passes strict TypeScript compilation',
      'Runtime validation schemas match types',
      'Comprehensive test coverage',
    ],
    context: {
      targetFile: 'src/UserService.ts',
      language: 'TypeScript',
    },
  },
  
  simple: {
    workingDir: '/test/project',
    knowledgeUpdate: ['Basic programming concepts'],
    actionInstruction: 'Explain what a function is in programming',
    qualityStandards: ['Clear explanation', 'Practical examples'],
    validationCriteria: ['Covers function concept', 'Includes examples'],
  },
  
  invalid: {
    workingDir: '', // Invalid
    knowledgeUpdate: [], // Invalid
    actionInstruction: '', // Invalid
    qualityStandards: [],
    validationCriteria: [],
  },
};

export const MOCK_VALIDATION_RESULTS = {
  successful: [
    {
      passed: true,
      criteria: 'Basic execution success',
      details: 'Execution completed successfully',
      score: 1.0,
    },
    {
      passed: true,
      criteria: 'Content quality',
      details: 'Passed 4/4 quality checks',
      score: 0.9,
    },
    {
      passed: true,
      criteria: 'File existence: src/Test.ts',
      details: 'File was generated successfully',
      score: 1.0,
    },
  ],
  
  failed: [
    {
      passed: false,
      criteria: 'Basic execution success',
      details: 'Execution failed with error',
      score: 0.0,
    },
    {
      passed: false,
      criteria: 'Content quality',
      details: 'Content too short and missing code patterns',
      score: 0.2,
    },
  ],
  
  mixed: [
    {
      passed: true,
      criteria: 'Basic execution success',
      details: 'Execution completed successfully',
      score: 1.0,
    },
    {
      passed: false,
      criteria: 'File existence: missing-file.ts',
      details: 'Expected file was not generated',
      score: 0.0,
    },
    {
      passed: true,
      criteria: 'Content quality',
      details: 'Good content with code examples',
      score: 0.8,
    },
  ],
};

/**
 * Create a mock execution result for testing
 */
export function createMockExecutionResult(overrides: any = {}) {
  return {
    instruction: MOCK_INSTRUCTIONS.simple,
    response: MOCK_CLI_RESPONSES.simple,
    executionTime: 2000,
    timestamp: new Date().toISOString(),
    method: 'cli' as const,
    ...overrides,
  };
}

/**
 * Create a mock test result for testing
 */
export function createMockTestResult(overrides: any = {}) {
  return {
    testName: 'mock_test_result',
    instruction: MOCK_INSTRUCTIONS.simple,
    executionResult: createMockExecutionResult(),
    validationResults: MOCK_VALIDATION_RESULTS.successful,
    overallScore: 0.93,
    success: true,
    generatedFiles: ['src/test.ts'],
    notes: 'Mock test result for testing',
    ...overrides,
  };
}