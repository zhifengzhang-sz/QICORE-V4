# Integration Patterns for QiCore v4 Components

## Overview
This guide defines how the 5 QiCore v4 components integrate with each other to form a cohesive system.

## Core Integration Principles

### 1. Result<T> Throughout
All fallible operations across all components return `Result<T>` for consistent error handling.

### 2. QiError Consistency
All error cases use QiError for structured error reporting with context.

### 3. Dependency Flow
Components have clear dependency relationships without circular dependencies.

## Component Integration Architecture

```
Configuration ──> Logger ──> Cache
     │              │         │
     └──> Result<T> <┴────> QiError
```

## Integration Pattern 1: Configuration → Logger

### Purpose
Logger accepts Configuration for initialization and runtime configuration.

### Implementation Pattern
```typescript
// Configuration provides logger settings
const loggerConfig = await Configuration.get(config, 'logger');
const logger = loggerConfig.fold(
  error => Logger.createDefault(),
  config => Logger.create(config)
);
```

### Integration Requirements
- Logger must handle Configuration errors gracefully
- Configuration changes should be observable by Logger
- Default Logger behavior when Configuration is unavailable

### Error Handling
```typescript
const initializeLogger = (config: Configuration): Result<Logger> => {
  return Configuration.get(config, 'logger')
    .flatMap(loggerConfig => Logger.create(loggerConfig))
    .mapError(error => QiError.create(
      'LOGGER_INIT_FAILED',
      'Failed to initialize logger from configuration',
      ErrorCategory.SYSTEM
    ).withCause(error));
};
```

## Integration Pattern 2: Cache → Logger

### Purpose
Cache uses Logger for operational logging and error reporting.

### Implementation Pattern
```typescript
class Cache {
  constructor(private logger: Logger) {}

  set<T>(key: string, value: T, ttl?: number): Result<void> {
    return Result.fromTryCatch(() => {
      this.logger.debug('Cache set operation', { key, ttl });
      // ... cache implementation
    }).mapError(error => {
      this.logger.error('Cache set failed', { key, error });
      return QiError.create('CACHE_SET_FAILED', 'Failed to set cache value', ErrorCategory.SYSTEM)
        .withContext({ key, ttl })
        .withCause(error);
    });
  }
}
```

### Integration Requirements
- Cache operations should be logged at appropriate levels
- Cache errors should be logged with context
- Logger failures should not break Cache operations

## Integration Pattern 3: Result<T> Error Propagation

### Purpose
Consistent error handling and propagation across all components.

### Implementation Pattern
```typescript
// Error propagation through component chain
const initializeSystem = async (): Promise<Result<System>> => {
  return Configuration.fromFile('config.json')
    .flatMap(config => Logger.create(config))
    .flatMap(logger => Cache.create({ logger }))
    .map(cache => new System(cache));
};
```

### Integration Requirements
- All components must preserve error context
- Error chains should be traceable
- No error information should be lost in propagation

## Integration Pattern 4: Component Composition

### Purpose
Compose components into higher-level abstractions while maintaining clean boundaries.

### Implementation Pattern
```typescript
class QiCoreSystem {
  private constructor(
    private config: Configuration,
    private logger: Logger,
    private cache: Cache
  ) {}

  static async create(configPath: string): Promise<Result<QiCoreSystem>> {
    return Configuration.fromFile(configPath)
      .flatMap(config => 
        Logger.create(config).flatMap(logger =>
          Cache.create({ logger }).map(cache =>
            new QiCoreSystem(config, logger, cache)
          )
        )
      );
  }
}
```

## Integration Testing Patterns

### Pattern 1: Configuration-Logger Integration Test
```typescript
describe('Configuration-Logger Integration', () => {
  it('should initialize logger from configuration', async () => {
    const config = Configuration.fromObject({
      logger: { level: 'INFO', transports: ['console'] }
    });
    
    const loggerResult = config.flatMap(c => Logger.create(c));
    
    expect(loggerResult.isSuccess()).toBe(true);
    // ... additional assertions
  });

  it('should handle invalid logger configuration', async () => {
    const config = Configuration.fromObject({
      logger: { level: 'INVALID' }
    });
    
    const loggerResult = config.flatMap(c => Logger.create(c));
    
    expect(loggerResult.isFailure()).toBe(true);
    // ... error assertions
  });
});
```

### Pattern 2: Cache-Logger Integration Test
```typescript
describe('Cache-Logger Integration', () => {
  it('should log cache operations', async () => {
    const logger = Logger.createInMemory();
    const cache = Cache.create({ logger });
    
    await cache.set('key', 'value');
    
    const logs = logger.getLogs();
    expect(logs.some(log => log.message.includes('Cache set operation'))).toBe(true);
  });
});
```

### Pattern 3: Full System Integration Test
```typescript
describe('Full System Integration', () => {
  it('should initialize complete system from configuration', async () => {
    const systemResult = await QiCoreSystem.create('test-config.json');
    
    expect(systemResult.isSuccess()).toBe(true);
    
    const system = systemResult.unwrap();
    // Test system operations
  });
});
```

## Error Handling Integration Patterns

### Pattern 1: Graceful Degradation
```typescript
const initializeWithFallback = async (): Promise<System> => {
  const configResult = await Configuration.fromFile('config.json');
  
  const logger = configResult
    .flatMap(config => Logger.create(config))
    .getOrElse(() => Logger.createDefault());
    
  const cache = Cache.create({ logger })
    .getOrElse(() => Cache.createInMemory());
    
  return new System(logger, cache);
};
```

### Pattern 2: Error Context Enrichment
```typescript
const enrichError = (error: QiError, component: string, operation: string): QiError => {
  return error
    .withContext({ component, operation, timestamp: Date.now() })
    .withContext({ systemState: getSystemState() });
};
```

## Performance Integration Considerations

### Logging Performance Impact
- Logger level checks must be <1μs to avoid impacting other components
- Structured logging should not add significant overhead
- Async logging to avoid blocking operations

### Cache Integration Performance
- Cache operations should complete in <50μs
- Logger integration should not impact cache performance
- Error handling should be optimized for the happy path

### Configuration Loading Performance
- Configuration loading should complete in <1ms
- Configuration changes should not block system operations
- Lazy loading for non-critical configuration

## Resource Management Integration

### Cleanup Patterns
```typescript
class QiCoreSystem {
  async shutdown(): Promise<Result<void>> {
    return Result.fromTryCatch(async () => {
      await this.cache.close();
      await this.logger.close();
      // Configuration doesn't need cleanup
    });
  }
}
```

### Resource Sharing
- Logger instances can be shared across components
- Configuration should be immutable and shareable
- Cache instances should be isolated per use case

## Integration Verification Checklist

### Component Boundaries
- [ ] Clean interfaces between components
- [ ] No circular dependencies
- [ ] Proper error propagation

### Error Handling
- [ ] Consistent QiError usage
- [ ] Error context preservation
- [ ] Graceful degradation patterns

### Performance
- [ ] Integration doesn't impact individual component performance
- [ ] Resource sharing is efficient
- [ ] No performance bottlenecks at integration points

### Testing
- [ ] Integration tests for all component pairs
- [ ] Full system integration tests
- [ ] Error scenario integration tests
- [ ] Performance integration tests 