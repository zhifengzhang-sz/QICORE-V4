# todo list

## test coverage improvements

- [ ] improve qi/core/base coverage beyond 72.76% (target: 85%+)
- [ ] add implementation testing for qiprompt templates (not just api mocking)

## rag infrastructure (high priority)

- [ ] add vector database tools (pinecone, weaviate, chroma, qdrant) to qimcp
- [ ] add embedding generation tools (openai, cohere, local models) to qimcp
- [ ] add semantic search and similarity tools to qimcp
- [ ] add document chunking and preprocessing tools to qimcp

## platform integrations

- [ ] add dify api client for workflows to qimcp
- [ ] add dify app publishing and management tools to qimcp
- [ ] add llamaindex document ingestion and indexing to qimcp
- [ ] add llamaindex query engines and retrievers to qimcp
- [ ] add llamaindex agent tools and multi-step reasoning to qimcp

## search & knowledge

- [ ] add web search tools (tavily, serpapi, bing) to qimcp
- [ ] add knowledge base connectors (notion, confluence, github) to qimcp

## database integrations

- [ ] add postgresql pgvector integration to qimcp
- [ ] add mongodb atlas vector search to qimcp

## api & framework tools

- [ ] add rest api client tools to qimcp
- [ ] add graphql client tools to qimcp
- [ ] add langchain integration tools to qimcp

## monitoring & observability

- [ ] add llm observability tools (langsmith, w&b) to qimcp

## architecture design (high priority)

- [ ] design qimcp architecture: rag/, dify/, llamaindex/, search/, databases/, apis/, monitoring/

## crypto data platform (real-world project)

- [ ] add cryptocompare api client to qimcp/apis/
- [ ] add websocket streaming support to qimcp/apis/
- [ ] add redpanda/kafka producer tools to qimcp/streaming/
- [ ] add clickhouse integration to qimcp/databases/
- [ ] add timescaledb integration to qimcp/databases/
- [ ] create ohlcv data pipeline demo in app/examples/

---

*tight, actionable development tasks - each item is a concrete deliverable*