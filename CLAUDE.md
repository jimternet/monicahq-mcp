# Claude Code Context - MonicaHQ MCP Server

## Project Overview
Spring Boot-based MCP server providing Claude Desktop access to MonicaHQ CRM via 52 operations across 12 entity types.

## Current Status
- **Tests**: 136/136 passing (100%) ✅
- **Architecture**: STDIO MCP protocol with Spring Boot 3.x
- **Deployment**: Ready for production with Docker + Claude Desktop

## Tech Stack
- **Java 17+**, Spring Boot 3.x, Gradle
- **WebFlux** (reactive), **Resilience4j** (circuit breaker)
- **MapStruct** (mapping), **Lombok** (code generation)
- **JUnit 5** (testing), **Docker** (deployment)

## Project Structure
```
src/main/java/com/monicahq/mcp/
├── config/          # Spring configuration
├── controller/      # MCP handlers + tool registry
├── service/         # 52 business operations
├── client/          # MonicaHQ API client
├── dto/            # 12 entity models
├── mapper/         # Type-safe conversions
└── exception/      # Error handling

src/test/java/
├── contract/       # 107 operation tests
├── integration/    # 26 workflow tests
└── config/         # Test mocks + configuration
```

## Key Commands
```bash
# Test
./gradlew test

# Build
./gradlew build

# Docker
docker build -t monicahq-mcp .
docker run -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp
```

## Environment Variables
- `MONICA_API_URL`: MonicaHQ API endpoint
- `MONICA_API_TOKEN`: OAuth2 Bearer token

## MCP Operations (52 total)
**Entity Types**: Contact, Activity, Call, Note, Task, Tag, Reminder, JournalEntry, Conversation, ConversationMessage, ContactField, ContactTag

**Operations per entity**: create, get, update, delete, list (plus add/remove for relationships)

## Architecture Patterns
- **MCP Protocol**: JSON-RPC 2.0 over STDIO
- **Reactive**: Non-blocking I/O with WebFlux
- **Resilience**: Circuit breaker for API failures
- **Testing**: Contract tests + integration workflows
- **Security**: OAuth2 Bearer authentication

## Recent Changes
1. **Tests Fixed**: All 136 tests now passing (converted from HTTP to STDIO architecture)
2. **Integration Ready**: Docker + Claude Desktop deployment configured
3. **Production Ready**: Full error handling, logging, health checks

---
*Ready for Claude Desktop integration - see README.md for setup instructions*