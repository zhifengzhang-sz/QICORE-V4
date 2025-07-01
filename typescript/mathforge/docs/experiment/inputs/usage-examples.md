# Usage Examples for QiCore v4 Components

## Result<T> Component Examples

### Basic Usage
```typescript
import { Result } from './result';

// Creating successful results
const success = Result.success(42);
const user = Result.success({ id: 1, name: 'Alice' });

// Creating failed results
const error = QiError.create('NOT_FOUND', 'User not found', ErrorCategory.BUSINESS);
const failure = Result.failure<User>(error);
```

### Transforming Results
```typescript
// Map transforms successful values
const doubled = Result.success(21).map(x => x * 2); // Result.success(42)

// FlatMap chains operations that can fail
const divide = (a: number, b: number): Result<number> => {
  if (b === 0) {
    return Result.failure(QiError.create('DIVISION_BY_ZERO', 'Cannot divide by zero', ErrorCategory.VALIDATION));
  }
  return Result.success(a / b);
};

const chained = Result.success(10)
  .flatMap(x => divide(x, 2))  // Result.success(5)
  .flatMap(x => divide(x, 0)); // Result.failure(error)
```

## QiError Component Examples

### Basic Error Creation
```typescript
import { QiError, ErrorCategory } from './qierror';

// Simple error
const error = QiError.create(
  'VALIDATION_FAILED',
  'Email address is invalid',
  ErrorCategory.VALIDATION
);

// Error with context
const errorWithContext = error
  .withContext({ email: 'invalid-email', field: 'email' })
  .withContext({ requestId: 'req-123' });
```

## Configuration Component Examples

### Loading and Merging
```typescript
import { Configuration } from './configuration';

// From object
const config = Configuration.fromObject({
  database: { host: 'localhost', port: 5432 },
  logging: { level: 'INFO' }
});

// Merge configurations
const merged = Configuration.merge([defaultConfig, userConfig]);
```

## Logger Component Examples

### Basic Logging
```typescript
import { Logger, LogLevel } from './logger';

const logger = Logger.create({
  level: LogLevel.INFO,
  transports: ['console']
}).unwrap();

logger.info('Application started');
logger.error('Database connection failed', { error: dbError });
```

## Cache Component Examples

### Basic Operations
```typescript
import { Cache } from './cache';

const cache = Cache.createMemory<string>({
  maxSize: 1000,
  defaultTtl: 3600
}).unwrap();

cache.set('user:123', 'Alice');
const user = cache.get('user:123');
``` 