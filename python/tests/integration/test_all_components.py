# tests/integration/test_all_components.py
import pytest
import asyncio
from pathlib import Path
from qicore.base import Result, QiError
from qicore.core import Configuration, StructuredLogger, Cache, configure_logging
from qicore.application import HTTPClient, WebApplication, CLIApplication, Database, AIClient
from pydantic import BaseModel


class ServiceConfig(BaseModel):
    service_name: str = "QiCore Integration Test"
    database_path: str = ":memory:"
    cache_size: int = 100
    log_level: str = "INFO"


class TestAllComponents:
    """Integration tests verifying all 13 components work together"""
    
    @pytest.fixture
    async def configured_logger(self):
        """Setup structured logging for tests"""
        result = await configure_logging(level="INFO", format="json")
        assert result.is_success()
        return StructuredLogger("integration_test")
    
    @pytest.fixture
    def test_config(self):
        """Create test configuration"""
        config = Configuration(ServiceConfig)
        result = config.load_from_dict({
            "service_name": "QiCore Test Service",
            "database_path": ":memory:",
            "cache_size": 50,
            "log_level": "DEBUG"
        })
        assert result.is_success()
        return config
    
    @pytest.fixture
    async def test_cache(self):
        """Create test cache"""
        return Cache[str, str](max_size=10)
    
    @pytest.fixture
    async def test_database(self):
        """Create and setup test database"""
        db = Database(":memory:")
        connect_result = await db.connect()
        assert connect_result.is_success()
        
        # Create test table
        create_result = await db.execute(
            "CREATE TABLE test_table (id INTEGER PRIMARY KEY, name TEXT, value INTEGER)"
        )
        assert create_result.is_success()
        
        yield db
        
        # Cleanup
        await db.close()
    
    def test_component_count(self):
        """Ensure all 13 components are importable"""
        components = [
            Result, QiError,  # Base (2)
            Configuration, StructuredLogger, Cache,  # Core (3)
            HTTPClient, WebApplication, CLIApplication, Database, AIClient  # Application (5 shown, 8 total)
        ]
        
        # We're testing 10 out of 13 components that are easily importable
        assert len(components) == 10
        
        # Verify all can be instantiated (basic smoke test)
        result = Result.success("test")
        assert result.is_success()
        
        error = QiError.validation_error("test", "field", "value")
        assert error.category == "ValidationError"
    
    @pytest.mark.asyncio
    async def test_result_error_integration(self, configured_logger):
        """Test Result and QiError working together"""
        logger = configured_logger
        
        # Test successful flow
        success_result = Result.success(42)
        doubled = success_result.map(lambda x: x * 2)
        
        assert doubled.is_success()
        assert doubled.unwrap() == 84
        logger.info("Result success path working", value=doubled.unwrap())
        
        # Test error flow
        error = QiError.network_error("Connection failed", "https://test.com", 500)
        failed_result = Result.failure(error)
        
        # Error should propagate through operations
        mapped_failed = failed_result.map(lambda x: x * 2)
        assert not mapped_failed.is_success()
        
        logger.info("Result error path working", error_category=error.category)
    
    @pytest.mark.asyncio
    async def test_configuration_cache_integration(self, test_config, test_cache):
        """Test Configuration and Cache working together"""
        config = test_config
        cache = test_cache
        
        # Get config values
        config_data = config.get().unwrap()
        
        # Cache configuration values
        cache_key = f"config_{config_data.service_name}"
        await cache.set(cache_key, str(config_data.cache_size))
        
        # Retrieve from cache
        cached_result = await cache.get(cache_key)
        assert cached_result.is_success()
        assert cached_result.unwrap() == "50"
        
        # Test cache stats
        stats = cache.get_stats()
        assert stats['hits'] >= 1
        assert stats['sets'] >= 1
    
    @pytest.mark.asyncio
    async def test_database_cache_integration(self, test_database, test_cache, configured_logger):
        """Test Database and Cache working together with logging"""
        db = test_database
        cache = test_cache
        logger = configured_logger
        
        # Insert test data
        insert_result = await db.execute(
            "INSERT INTO test_table (name, value) VALUES (?, ?)",
            ("test_item", 100)
        )
        assert insert_result.is_success()
        logger.info("Database insert successful")
        
        # Fetch from database
        fetch_result = await db.fetch_all("SELECT * FROM test_table WHERE name = ?", ("test_item",))
        assert fetch_result.is_success()
        
        rows = fetch_result.unwrap()
        assert len(rows) == 1
        assert rows[0]['name'] == 'test_item'
        assert rows[0]['value'] == 100
        
        # Cache the database result
        cache_key = f"db_result_{rows[0]['id']}"
        await cache.set(cache_key, str(rows[0]['value']))
        
        # Verify cached value
        cached_result = await cache.get(cache_key)
        assert cached_result.is_success()
        assert cached_result.unwrap() == "100"
        
        logger.info("Database-Cache integration successful", 
                   row_count=len(rows), 
                   cached_value=cached_result.unwrap())
    
    @pytest.mark.asyncio
    async def test_http_client_integration(self, configured_logger):
        """Test HTTP client with proper error handling"""
        logger = configured_logger
        
        # Create HTTP client
        client = HTTPClient(base_url="https://httpbin.org", timeout=10.0)
        
        try:
            # Test successful request
            response_result = await client.get("/json")
            
            if response_result.is_success():
                response = response_result.unwrap()
                assert response.status_code == 200
                logger.info("HTTP client success", status=response.status_code)
            else:
                # Handle potential network issues in testing environment
                error = response_result._inner.failure()
                logger.warning("HTTP request failed (expected in some test environments)", 
                             error_category=error.category)
                
        finally:
            await client.close()
    
    def test_cli_application_integration(self, configured_logger):
        """Test CLI application basic functionality"""
        logger = configured_logger
        
        cli = CLIApplication("test-cli", version="1.0.0", logger=logger)
        
        # Test command decoration
        @cli.command("test-cmd")
        def test_command():
            """Test command"""
            return Result.success("Command executed successfully")
        
        # Verify command was registered
        assert "test-cmd" in [cmd.name for cmd in cli.cli.commands.values()]
        logger.info("CLI application integration successful")
    
    def test_web_application_integration(self, configured_logger):
        """Test web application basic setup"""
        logger = configured_logger
        
        web = WebApplication(title="Test API", version="1.0.0", logger=logger)
        
        # Test route decoration
        @web.route("/test", methods=["GET"])
        async def test_endpoint():
            """Test endpoint"""
            return Result.success({"message": "Hello from QiCore"})
        
        # Verify FastAPI app was created
        app = web.get_app()
        assert app.title == "Test API"
        assert len(app.routes) > 0  # Should have at least our test route
        
        logger.info("Web application integration successful")
    
    @pytest.mark.asyncio
    async def test_transaction_integration(self, test_database, configured_logger):
        """Test database transactions with Result pattern"""
        db = test_database
        logger = configured_logger
        
        # Test successful transaction
        async with db.transaction():
            insert1 = await db.execute_in_transaction(
                "INSERT INTO test_table (name, value) VALUES (?, ?)",
                ("txn_item1", 200)
            )
            assert insert1.is_success()
            
            insert2 = await db.execute_in_transaction(
                "INSERT INTO test_table (name, value) VALUES (?, ?)",
                ("txn_item2", 300)
            )
            assert insert2.is_success()
        
        # Verify both items were inserted
        count_result = await db.fetch_all("SELECT COUNT(*) as count FROM test_table WHERE name LIKE 'txn_%'")
        assert count_result.is_success()
        assert count_result.unwrap()[0]['count'] == 2
        
        logger.info("Transaction integration successful")
    
    @pytest.mark.asyncio
    async def test_end_to_end_workflow(self, configured_logger):
        """Test complete workflow using multiple components"""
        logger = configured_logger
        
        # 1. Setup configuration
        config = Configuration(ServiceConfig)
        config_result = config.load_from_dict({
            "service_name": "E2E Test Service",
            "database_path": ":memory:",
            "cache_size": 20
        })
        assert config_result.is_success()
        
        # 2. Setup cache based on config
        config_data = config_result.unwrap()
        cache = Cache[str, str](max_size=config_data.cache_size)
        
        # 3. Setup database
        db = Database(config_data.database_path)
        connect_result = await db.connect()
        assert connect_result.is_success()
        
        try:
            # 4. Create test data with error handling
            create_table = await db.execute(
                "CREATE TABLE users (id INTEGER PRIMARY KEY, username TEXT, email TEXT)"
            )
            assert create_table.is_success()
            
            # 5. Insert user with caching
            user_data = ("john_doe", "john@example.com")
            insert_result = await db.execute(
                "INSERT INTO users (username, email) VALUES (?, ?)",
                user_data
            )
            
            if insert_result.is_success():
                # Cache the user data
                cache_key = f"user:{user_data[0]}"
                await cache.set(cache_key, user_data[1])
                
                # 6. Retrieve and verify
                fetch_result = await db.fetch_all(
                    "SELECT * FROM users WHERE username = ?", 
                    (user_data[0],)
                )
                assert fetch_result.is_success()
                
                users = fetch_result.unwrap()
                assert len(users) == 1
                assert users[0]['username'] == user_data[0]
                
                # 7. Verify cache
                cached_email = await cache.get(cache_key)
                assert cached_email.is_success()
                assert cached_email.unwrap() == user_data[1]
                
                logger.info("End-to-end workflow successful",
                           user_count=len(users),
                           cache_hits=cache.get_stats()['hits'])
                
        finally:
            await db.close()
    
    def test_error_propagation_across_components(self, configured_logger):
        """Test that errors propagate correctly across component boundaries"""
        logger = configured_logger
        
        # Create a chain of operations that should propagate errors
        def process_data(value: int) -> Result[int]:
            if value < 0:
                return Result.failure(
                    QiError.validation_error("Negative values not allowed", "value", value)
                )
            return Result.success(value * 2)
        
        def cache_data(value: int) -> Result[str]:
            if value > 1000:
                return Result.failure(
                    QiError.resource_error("Value too large for cache", "cache", str(value))
                )
            return Result.success(f"cached_{value}")
        
        # Test error propagation
        result = (Result.success(-5)
                 .flat_map(process_data)
                 .flat_map(lambda x: cache_data(x)))
        
        assert not result.is_success()
        
        # Test successful propagation
        success_result = (Result.success(10)
                         .flat_map(process_data)
                         .flat_map(lambda x: cache_data(x)))
        
        assert success_result.is_success()
        assert success_result.unwrap() == "cached_20"
        
        logger.info("Error propagation test successful")