# Claude Code Context - MonicaHQ MCP Server

## Project Overview
Spring Boot-based MCP server providing Claude Desktop access to MonicaHQ CRM via 171 operations across 27 entity types.

## Current Status
- **Tests**: 1,792/1,792 passing (100% pass rate) ✅
  - Active: 1,792 test methods for current features
  - Disabled: 8 tests for future debug mode functionality (TDD placeholders)
- **Phase 2 Complete**: ContactFieldType CRUD + Contact-scoped lists (17 new operations) ✅
- **API Coverage**: 100% (171 of 171 Phase 2 target operations) ✅
- **Security**: All P0 vulnerabilities fixed, production profile configured ✅
- **Constitution**: v1.3.0 with enhanced Principle VI (Complete Monica API Data Access) ✅
- **Architecture**: STDIO MCP protocol with Spring Boot 3.x with complete field visibility
- **Deployment**: Production-ready with Docker + Claude Desktop

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
├── service/         # 122 business operations
├── client/          # MonicaHQ API client
├── dto/            # 23 entity models
├── mapper/         # Type-safe conversions
└── exception/      # Error handling

src/test/java/
├── contract/       # 107 operation tests (require live Monica API)
├── integration/    # 26 workflow tests + 8 disabled debug mode tests (future)
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

## MCP Operations (171 total)
**Entity Types**: Contact, Activity, Call, Note, Task, Tag, Reminder, JournalEntry, Conversation, ConversationMessage, ContactField, ContactFieldType, ContactTag, Relationship, Company, RelationshipType, RelationshipTypeGroup, Debt, Document, Photo, Gift, AuditLog, Country, Currency, Gender, Place, LifeEvent

**Operations per entity**: create, get, update, delete, list (plus add/remove for relationships, search for reference data, list_by_contact for contact-scoped queries)

**Phase 1 Additions** (13 operations):
- **Gender Management** (4 ops): Full CRUD for inclusive gender definitions
- **Place/Location Management** (5 ops): Geographic location tracking with CRUD + list
- **LifeEvent Management** (4 ops): Life milestone tracking with full CRUD

**Phase 2 Additions** (17 operations):
- **ContactFieldType CRUD** (4 ops): Custom field type management (name, type, protocol, fontawesome_icon)
- **Contact-Scoped Lists** (13 ops): Efficient queries for all contact-related data (activities, addresses, calls, contactfields, debts, documents, gifts, notes, occupations, pets, photos, reminders, tasks)

## Architecture Patterns
- **MCP Protocol**: JSON-RPC 2.0 over STDIO
- **Reactive**: Non-blocking I/O with WebFlux
- **Resilience**: Circuit breaker for API failures
- **Testing**: Contract tests + integration workflows
- **Security**: OAuth2 Bearer authentication

## Constitutional Governance Framework
**Core Principles** (Constitution v1.3.0):
1. **MCP Protocol First**: JSON-RPC 2.0 over STDIO, Claude Desktop integration priority
2. **Test-Driven Development**: 100% test coverage mandatory (173/173 tests)
3. **Spring Boot Architecture Excellence**: WebFlux for external APIs, proper reactive patterns
4. **Production-Ready Deployment**: Docker + Claude Desktop from day one
5. **Type Safety & Code Generation**: MapStruct + Lombok for reliability
6. **Complete Monica API Data Access**: ALL fields from Monica API responses must appear in MCP content

**Validation Tools**:
- `./validate-constitution.sh` - Constitutional compliance verification
- `tests/constitutional/` - Principle-specific test suites
- `tests/integration/` - STDOUT contamination detection

## Recent Changes
1. **Phase 2 Complete** (2026-02-07): ContactFieldType CRUD + Contact-scoped lists (17 operations, 100% Phase 2 API coverage)
2. **Phase 1 Complete** (2026-02-06): Gender, Place, LifeEvent management (13 operations, 92.4% API coverage)
3. **Security Hardening**: All P0 vulnerabilities fixed (token logging eliminated, production profile configured)
4. **Test Coverage Expanded**: 1,792 tests passing (47 new contract tests, 6 CRUD validation scripts, 2 integration workflows)
5. **Integration Testing Infrastructure**: Docker-based testing with real Monica instance support
6. **Enhanced Constitutional Principle VI**: Complete Monica API data visibility (v1.3.0)
7. **Generic Content Formatter**: ALL Monica API fields now visible in MCP content responses
8. **Universal Field Coverage**: Raw API data formatting ensures no data loss for Claude Desktop

## Future Enhancements
**Debug Mode** (TDD placeholders - 8 tests disabled):
- Environment variable and MCP capability-based activation
- Enhanced logging to stderr (no stdout contamination)
- Tool call tracing with timing and parameter logging
- Sensitive data redaction in debug output
- Configurable verbosity levels (DEBUG/TRACE)
- Performance validation with debug overhead monitoring

Tests are written and disabled until implementation is scheduled. See `DebugModeActivationTest.java`.

---
*Ready for Claude Desktop integration - see README.md for setup instructions*