# src/qicore/application/ai/client.py
from collections.abc import AsyncIterator

import ollama
from anthropic import AsyncAnthropic
from openai import AsyncOpenAI

from ...base.error import QiError
from ...base.result import Result
from ...core.logging import StructuredLogger


class AIClient:
    """Unified AI/LLM client with streaming support"""
    
    def __init__(
        self,
        provider: str = "openai",
        api_key: str | None = None,
        model: str = "gpt-4",
        temperature: float = 0.7,
        logger: StructuredLogger | None = None
    ):
        self.provider = provider
        self.model = model
        self.temperature = temperature
        self.logger = logger or StructuredLogger("ai_client")
        
        # Initialize provider client
        if provider == "openai":
            self.client = AsyncOpenAI(api_key=api_key)
        elif provider == "anthropic":
            self.client = AsyncAnthropic(api_key=api_key)
        elif provider == "ollama":
            self.client = ollama.AsyncClient()
        else:
            raise ValueError(f"Unsupported provider: {provider}")
    
    # Operation 1: Complete
    async def complete(
        self,
        prompt: str,
        max_tokens: int = 1000,
        system_prompt: str | None = None
    ) -> Result[str]:
        """Single completion request"""
        try:
            messages = []
            if system_prompt:
                messages.append({"role": "system", "content": system_prompt})
            messages.append({"role": "user", "content": prompt})
            
            self.logger.info(f"Completion request to {self.provider}")
            
            if self.provider == "openai":
                response = await self.client.chat.completions.create(
                    model=self.model,
                    messages=messages,
                    temperature=self.temperature,
                    max_tokens=max_tokens
                )
                return Result.success(response.choices[0].message.content)
                
            if self.provider == "anthropic":
                response = await self.client.completions.create(
                    model=self.model,
                    prompt=f"\n\nHuman: {prompt}\n\nAssistant:",
                    max_tokens_to_sample=max_tokens,
                    temperature=self.temperature
                )
                return Result.success(response.completion)
                
            if self.provider == "ollama":
                response = await self.client.generate(
                    model=self.model,
                    prompt=prompt,
                    options={"temperature": self.temperature}
                )
                return Result.success(response['response'])
                
        except Exception as e:
            self.logger.error(f"Completion failed: {e}")
            return Result.failure(
                QiError.integration_error(
                    f"AI completion failed: {e}",
                    self.provider,
                    "complete"
                )
            )
    
    # Operation 2: Stream
    async def stream(
        self,
        prompt: str,
        max_tokens: int = 1000,
        system_prompt: str | None = None
    ) -> AsyncIterator[Result[str]]:
        """Streaming completion with backpressure handling"""
        try:
            messages = []
            if system_prompt:
                messages.append({"role": "system", "content": system_prompt})
            messages.append({"role": "user", "content": prompt})
            
            self.logger.info(f"Streaming request to {self.provider}")
            
            if self.provider == "openai":
                stream = await self.client.chat.completions.create(
                    model=self.model,
                    messages=messages,
                    temperature=self.temperature,
                    max_tokens=max_tokens,
                    stream=True
                )
                
                async for chunk in stream:
                    if chunk.choices[0].delta.content:
                        yield Result.success(chunk.choices[0].delta.content)
                        
            elif self.provider == "ollama":
                async for chunk in await self.client.generate_stream(
                    model=self.model,
                    prompt=prompt,
                    options={"temperature": self.temperature}
                ):
                    yield Result.success(chunk['response'])
                    
        except Exception as e:
            self.logger.error(f"Streaming failed: {e}")
            yield Result.failure(
                QiError.integration_error(
                    f"AI streaming failed: {e}",
                    self.provider,
                    "stream"
                )
            )
    
    # Operation 3: Embed text
    async def embed(self, text: str) -> Result[list[float]]:
        """Generate text embeddings"""
        try:
            if self.provider == "openai":
                response = await self.client.embeddings.create(
                    model="text-embedding-ada-002",
                    input=text
                )
                return Result.success(response.data[0].embedding)
            return Result.failure(
                QiError.integration_error(
                    f"Embeddings not supported for {self.provider}",
                    self.provider,
                    "embed"
                )
            )
        except Exception as e:
            return Result.failure(
                QiError.integration_error(
                    f"Embedding failed: {e}",
                    self.provider,
                    "embed"
                )
            )
    
    # Operation 4: List models
    async def list_models(self) -> Result[list[str]]:
        """List available models"""
        try:
            if self.provider == "openai":
                response = await self.client.models.list()
                models = [model.id for model in response.data]
                return Result.success(models)
            if self.provider == "ollama":
                response = await self.client.list()
                models = [model['name'] for model in response['models']]
                return Result.success(models)
            return Result.failure(
                QiError.integration_error(
                    f"Model listing not supported for {self.provider}",
                    self.provider,
                    "list_models"
                )
            )
        except Exception as e:
            return Result.failure(
                QiError.integration_error(
                    f"List models failed: {e}",
                    self.provider,
                    "list_models"
                )
            )
    
    # Operation 5: Change model
    def set_model(self, model: str) -> Result[None]:
        """Change the active model"""
        self.model = model
        self.logger.info(f"Model changed to {model}")
        return Result.success(None)