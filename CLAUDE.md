# Claude Code Context - MonicaHQ MCP Server

## Project Overview
Building a Model Context Protocol (MCP) server that interfaces with MonicaHQ CRM API, providing 52 operations across 12 entity types through a standardized MCP interface.

## Current Branch
`001-build-an-local` - MonicaHQ MCP Server Integration

## Tech Stack
- **Language**: Java 17+
- **Framework**: Spring Boot 3.x (initialized via start.spring.io)
- **Build Tool**: Gradle (Groovy DSL)
- **Key Dependencies**:
  - spring-boot-starter-webflux (reactive programming)
  - spring-boot-starter-websocket (MCP protocol)
  - resilience4j (circuit breaker pattern)
  - mapstruct (DTO mapping)
  - lombok (boilerplate reduction)
- **Testing**: JUnit 5, Spring Boot Test, MockWebServer

## Project Structure
```
src/main/java/com/monicahq/mcp/
├── config/          # Spring configuration
├── controller/      # MCP WebSocket handlers
├── service/         # Business logic (52 operations)
├── client/          # MonicaHQ API client
├── dto/            # Data transfer objects
├── mapper/         # MapStruct mappers
└── exception/      # Custom exceptions

src/test/java/
├── contract/       # Contract tests (RED phase)
├── integration/    # Integration tests
└── unit/          # Unit tests
```

## Key Patterns
1. **MCP Protocol**: JSON-RPC 2.0 over WebSocket
2. **Reactive**: WebFlux for non-blocking I/O
3. **Resilience**: Circuit breaker with Resilience4j
4. **DTO Mapping**: MapStruct for type-safe conversions
5. **Testing**: TDD with RED-GREEN-Refactor cycle

## Current Implementation Status
- [x] Specification defined
- [x] Research completed
- [x] Data model designed
- [x] MCP contracts defined
- [ ] Contract tests (write first - must fail)
- [ ] Spring Boot project initialization
- [ ] MCP protocol handler
- [ ] MonicaHQ client implementation
- [ ] Entity services (52 operations)
- [ ] Integration tests
- [ ] Docker packaging

## MonicaHQ Entities
12 entity types with CRUD operations:
- Contact, Activity, Call, Note, Task
- Tag, Reminder, JournalEntry, Conversation
- ConversationMessage, ContactField, ContactTag

## Testing Commands
```bash
# Run all tests
./gradlew test

# Run specific test category
./gradlew test --tests "*contract*"
./gradlew test --tests "*integration*"

# Performance validation
./gradlew test --tests PerformanceTest
```

## Environment Setup
```bash
export MONICA_API_URL=https://your-instance.monicahq.com/api
export MONICA_API_TOKEN=your-api-token-here
```

## Key Files
- `/specs/001-build-an-local/spec.md` - Feature specification
- `/specs/001-build-an-local/plan.md` - Implementation plan
- `/specs/001-build-an-local/data-model.md` - Entity definitions
- `/specs/001-build-an-local/contracts/mcp-protocol.json` - MCP contract
- `/specs/001-build-an-local/quickstart.md` - Usage guide

## Constitutional Requirements
- **TDD Mandatory**: Write tests first, ensure they fail
- **Library-First**: Each feature as standalone module
- **CLI Support**: Each service exposes validation commands
- **Structured Logging**: SLF4J with correlation IDs
- **Version**: 0.1.0, increment BUILD on changes

## Recent Changes
1. Defined MCP protocol contract with 52 operations
2. Created data model for 12 MonicaHQ entities
3. Established Spring Boot architecture with WebFlux

---
*Auto-generated context for Claude Code - Last updated: 2025-09-13*