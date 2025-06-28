{-# LANGUAGE OverloadedStrings #-}

-- | Example usage of QiCore Base components
module Example where

import Data.Aeson (Value (..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import QiCore.Base

-- | Example: Basic error creation and handling
basicErrorExample :: IO ()
basicErrorExample = do
  -- Create a validation error
  validationError <-
    create
      "INVALID_INPUT"
      "Username must be at least 3 characters long"
      VALIDATION
      (Just $ Map.fromList [("input", String "ab"), ("minLength", Number 3)])
      Nothing

  putStrLn $ "Error created: " <> T.unpack (toString validationError)

  -- Create an error with a cause
  networkError <-
    create
      "CONNECTION_FAILED"
      "Failed to connect to database"
      NETWORK
      (Just $ Map.fromList [("host", String "localhost"), ("port", Number 5432)])
      (Just validationError)

  putStrLn $ "Network error with cause: " <> T.unpack (toString networkError)

-- | Example: Result usage with functional composition
resultExample :: IO ()
resultExample = do
  -- Create some sample operations that return Results
  let parseNumber :: T.Text -> IO (Result Integer)
      parseNumber text = case reads (T.unpack text) of
        [(n, "")] -> return $ success n
        _ -> do
          err <- create "PARSE_ERROR" ("Invalid number: " <> text) VALIDATION Nothing Nothing
          return $ failure err

  let divideBy :: Integer -> Integer -> IO (Result Integer)
      divideBy x y
        | y == 0 = do
            err <- create "DIVISION_BY_ZERO" "Cannot divide by zero" VALIDATION Nothing Nothing
            return $ failure err
        | otherwise = return $ success (x `div` y)

  -- Example 1: Successful computation
  result1 <- do
    num1 <- parseNumber "42"
    case num1 of
      Success n1 -> do
        num2 <- parseNumber "2"
        case num2 of
          Success n2 -> divideBy n1 n2
          Failure err -> return $ Failure err
      Failure err -> return $ Failure err

  putStrLn $ "Result 1: " <> show result1

  -- Example 2: Using monadic composition (more idiomatic)
  result2 <- do
    num1Result <- parseNumber "100"
    num2Result <- parseNumber "5"
    case (num1Result, num2Result) of
      (Success n1, Success n2) -> divideBy n1 n2
      (Failure err, _) -> return $ Failure err
      (_, Failure err) -> return $ Failure err

  putStrLn $ "Result 2: " <> show result2

  -- Example 3: Handling failure
  result3 <- do
    num1Result <- parseNumber "abc"
    case num1Result of
      Success n1 -> do
        num2Result <- parseNumber "5"
        case num2Result of
          Success n2 -> divideBy n1 n2
          Failure err -> return $ Failure err
      Failure err -> return $ Failure err

  putStrLn $ "Result 3: " <> show result3

  -- Example 4: Using unwrapOr for safe defaults
  let safeValue = unwrapOr 0 result3
  putStrLn $ "Safe value from failed result: " <> show safeValue

-- | Example: Pattern matching with Result
patternMatchExample :: IO ()
patternMatchExample = do
  -- Create a successful result
  successResult <- return $ success "Hello, World!"

  -- Create a failed result
  failedResult <- do
    err <- create "TEST_ERROR" "This is a test error" UNKNOWN Nothing Nothing
    return $ failure err

  -- Pattern match on results
  let handleResult :: Result T.Text -> T.Text
      handleResult =
        match
          (\value -> "Success: " <> value)
          (\err -> "Error: " <> Error.toString err)

  putStrLn $ T.unpack $ handleResult successResult
  putStrLn $ T.unpack $ handleResult failedResult

-- | Example: Error context and cause chains
contextExample :: IO ()
contextExample = do
  -- Create a base error
  baseError <-
    create
      "FILE_NOT_FOUND"
      "Configuration file not found"
      FILESYSTEM
      (Just $ Map.fromList [("path", String "/etc/myapp/config.yaml")])
      Nothing

  -- Add more context
  enrichedError <-
    withContext baseError $
      Map.fromList
        [ ("attempted_paths", Array [String "/etc/myapp/config.yaml", String "~/.myapp/config.yaml"])
        , ("user", String "myuser")
        ]

  -- Create a higher-level error with the enriched error as cause
  appError <-
    create
      "CONFIG_LOAD_FAILED"
      "Application failed to load configuration"
      CONFIGURATION
      (Just $ Map.fromList [("component", String "ConfigManager")])
      (Just enrichedError)

  putStrLn $ "Final error: " <> T.unpack (toString appError)

-- | Main example runner
main :: IO ()
main = do
  putStrLn "=== QiCore Base Examples ==="

  putStrLn "\n--- Basic Error Example ---"
  basicErrorExample

  putStrLn "\n--- Result Example ---"
  resultExample

  putStrLn "\n--- Pattern Match Example ---"
  patternMatchExample

  putStrLn "\n--- Context Example ---"
  contextExample
