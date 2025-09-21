# Claude Code Context - MonicaHQ MCP Server

## Project Overview
Spring Boot-based MCP server providing Claude Desktop access to MonicaHQ CRM via 52 operations across 12 entity types.

## Current Status
- **Tests**: 147/147 passing (100%) ✅
- **Constitution**: v1.3.0 with enhanced Principle VI (Complete Monica API Data Access) ✅
- **Architecture**: STDIO MCP protocol with Spring Boot 3.x with complete field visibility
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

validation/             # Organized validation framework
├── crud/              # CRUD validation scripts (8 entity types)
├── constitutional/    # Constitutional governance validation
├── integration/       # MCP protocol integration tests
├── setup/            # Environment setup and utility scripts
└── docs/             # Validation reports and certificates
```

## Key Commands
```bash
# Validation Suite (Organized Structure)
cd validation/
./run-all-crud-tests.sh                    # Run all CRUD validations (8 entities)
./constitutional/validate-constitution.sh   # Validate all 5 core principles
./integration/test-mcp-complete.sh         # 7-phase comprehensive testing
./integration/test-claude-desktop.sh       # Claude Desktop integration

# Development
./gradlew test                # Run all tests (147 tests)
./gradlew build              # Build JAR file

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

## Constitutional Governance Framework
**Core Principles** (Constitution v1.3.0):
1. **MCP Protocol First**: JSON-RPC 2.0 over STDIO, Claude Desktop integration priority
2. **Test-Driven Development**: 100% test coverage mandatory (147/147 tests)
3. **Spring Boot Architecture Excellence**: WebFlux for external APIs, proper reactive patterns
4. **Production-Ready Deployment**: Docker + Claude Desktop from day one
5. **Type Safety & Code Generation**: MapStruct + Lombok for reliability
6. **Complete Monica API Data Access**: ALL fields from Monica API responses must appear in MCP content

**Validation Tools**:
- `./validate-constitution.sh` - Constitutional compliance verification
- `tests/constitutional/` - Principle-specific test suites
- `tests/integration/` - STDOUT contamination detection

## Recent Changes
1. **Enhanced Constitutional Principle VI**: Complete Monica API data visibility (v1.3.0)
2. **Generic Content Formatter**: ALL Monica API fields now visible in MCP content responses
3. **Universal Field Coverage**: Raw API data formatting ensures no data loss for Claude Desktop
4. **Test Suite Enhanced**: All 147 tests passing with comprehensive field validation

---
*Ready for Claude Desktop integration - see README.md for setup instructions*