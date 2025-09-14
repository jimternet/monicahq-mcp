# Tasks: MonicaHQ MCP Server Integration

**Input**: Design documents from `/specs/001-build-an-local/`
**Prerequisites**: plan.md (required), research.md, data-model.md, contracts/

## Execution Flow (main)
```
1. Load plan.md from feature directory
   → Tech stack: Java 17+, Spring Boot 3.x, Gradle
   → Libraries: WebFlux, WebSocket, Resilience4j, MapStruct, Lombok
   → Structure: Single project with src/ and tests/ at root
2. Load optional design documents:
   → data-model.md: 12 core entities + 17 extended entities (29 total from MonicaHQ API)
   → contracts/mcp-protocol.json: 52 Phase 1 MCP operations + framework for 145+ total
   → research.md: All 29 MonicaHQ endpoints confirmed, OAuth2 Bearer auth, modular architecture
3. Generate tasks by category:
   → Setup: Spring Boot init, Gradle dependencies, configuration
   → Tests: 52 contract tests for Phase 1 + integration scenarios
   → Core: 12 entity models, services, MCP handler with modular framework
   → Integration: MonicaHQ client, WebSocket, circuit breaker
   → Polish: unit tests, performance validation, documentation, Phase 2 preparation
4. Apply task rules:
   → Different files = mark [P] for parallel
   → Same file = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001-T085)
6. Generate dependency graph
7. Create parallel execution examples
8. Validate task completeness:
   → All 52 Phase 1 MCP operations have tests
   → All 12 core entities have models
   → Modular framework supports 29 total entities
   → Extension path for Phase 2 (145+ operations)
9. Return: SUCCESS (tasks ready for execution)
```

## Format: `[ID] [P?] Description`
- **[P]**: Can run in parallel (different files, no dependencies)
- Include exact file paths in descriptions

## Path Conventions
- **Single project**: `src/`, `tests/` at repository root
- Java package: `com.monicahq.mcp`
- Test package: `com.monicahq.mcp` (same structure in `src/test`)

## Phase 3.1: Setup
- [x] T001 Initialize Spring Boot project via start.spring.io with WebFlux, WebSocket, Validation dependencies (Gradle)
- [x] T002 Configure build.gradle with Resilience4j, MapStruct, Lombok, MockWebServer, Spring Boot Test
- [x] T003 [P] Create application.yml with MonicaHQ OAuth2 API and MCP WebSocket configuration
- [x] T004 [P] Set up SLF4J structured logging configuration in src/main/resources/logback-spring.xml
- [x] T005 [P] Create .env.example with MONICA_API_URL and MONICA_API_TOKEN placeholders

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**

### Contract Tests - Contact Entity (5 operations)
- [x] T006 [P] Contract test for contact_create in src/test/java/com/monicahq/mcp/contract/ContactCreateTest.java
- [x] T007 [P] Contract test for contact_get in src/test/java/com/monicahq/mcp/contract/ContactGetTest.java
- [x] T008 [P] Contract test for contact_update in src/test/java/com/monicahq/mcp/contract/ContactUpdateTest.java
- [x] T009 [P] Contract test for contact_delete in src/test/java/com/monicahq/mcp/contract/ContactDeleteTest.java
- [x] T010 [P] Contract test for contact_list in src/test/java/com/monicahq/mcp/contract/ContactListTest.java

### Contract Tests - Activity Entity (5 operations)
- [x] T011 [P] Contract test for activity_create in src/test/java/com/monicahq/mcp/contract/ActivityCreateTest.java
- [x] T012 [P] Contract test for activity_get in src/test/java/com/monicahq/mcp/contract/ActivityGetTest.java
- [x] T013 [P] Contract test for activity_update in src/test/java/com/monicahq/mcp/contract/ActivityUpdateTest.java
- [x] T014 [P] Contract test for activity_delete in src/test/java/com/monicahq/mcp/contract/ActivityDeleteTest.java
- [x] T015 [P] Contract test for activity_list in src/test/java/com/monicahq/mcp/contract/ActivityListTest.java

### Contract Tests - Call Entity (5 operations)
- [x] T016 [P] Contract test for call_create in src/test/java/com/monicahq/mcp/contract/CallCreateTest.java
- [x] T017 [P] Contract test for call_get in src/test/java/com/monicahq/mcp/contract/CallGetTest.java
- [x] T018 [P] Contract test for call_update in src/test/java/com/monicahq/mcp/contract/CallUpdateTest.java
- [x] T019 [P] Contract test for call_delete in src/test/java/com/monicahq/mcp/contract/CallDeleteTest.java
- [x] T020 [P] Contract test for call_list in src/test/java/com/monicahq/mcp/contract/CallListTest.java

### Contract Tests - Note Entity (5 operations)
- [x] T021 [P] Contract test for note_create in src/test/java/com/monicahq/mcp/contract/NoteCreateTest.java
- [x] T022 [P] Contract test for note_get in src/test/java/com/monicahq/mcp/contract/NoteGetTest.java
- [x] T023 [P] Contract test for note_update in src/test/java/com/monicahq/mcp/contract/NoteUpdateTest.java
- [x] T024 [P] Contract test for note_delete in src/test/java/com/monicahq/mcp/contract/NoteDeleteTest.java
- [x] T025 [P] Contract test for note_list in src/test/java/com/monicahq/mcp/contract/NoteListTest.java

### Contract Tests - Task Entity (5 operations)
- [x] T026 [P] Contract test for task_create in src/test/java/com/monicahq/mcp/contract/TaskCreateTest.java
- [x] T027 [P] Contract test for task_get in src/test/java/com/monicahq/mcp/contract/TaskGetTest.java
- [x] T028 [P] Contract test for task_update in src/test/java/com/monicahq/mcp/contract/TaskUpdateTest.java
- [x] T029 [P] Contract test for task_delete in src/test/java/com/monicahq/mcp/contract/TaskDeleteTest.java
- [x] T030 [P] Contract test for task_list in src/test/java/com/monicahq/mcp/contract/TaskListTest.java

### Contract Tests - Tag Entity (5 operations)
- [x] T031 [P] Contract test for tag_create in src/test/java/com/monicahq/mcp/contract/TagCreateTest.java
- [x] T032 [P] Contract test for tag_get in src/test/java/com/monicahq/mcp/contract/TagGetTest.java
- [x] T033 [P] Contract test for tag_update in src/test/java/com/monicahq/mcp/contract/TagUpdateTest.java
- [x] T034 [P] Contract test for tag_delete in src/test/java/com/monicahq/mcp/contract/TagDeleteTest.java
- [x] T035 [P] Contract test for tag_list in src/test/java/com/monicahq/mcp/contract/TagListTest.java

### Contract Tests - Reminder Entity (5 operations)
- [x] T036 [P] Contract test for reminder_create in src/test/java/com/monicahq/mcp/contract/ReminderCreateTest.java
- [x] T037 [P] Contract test for reminder_get in src/test/java/com/monicahq/mcp/contract/ReminderGetTest.java
- [x] T038 [P] Contract test for reminder_update in src/test/java/com/monicahq/mcp/contract/ReminderUpdateTest.java
- [x] T039 [P] Contract test for reminder_delete in src/test/java/com/monicahq/mcp/contract/ReminderDeleteTest.java
- [x] T040 [P] Contract test for reminder_list in src/test/java/com/monicahq/mcp/contract/ReminderListTest.java

### Contract Tests - Other Core Entities (17 operations)
- [x] T041 [P] Contract tests for journal entry operations (5 tests) in src/test/java/com/monicahq/mcp/contract/JournalEntryTests.java
- [x] T042 [P] Contract tests for conversation operations (4 tests) in src/test/java/com/monicahq/mcp/contract/ConversationTests.java
- [x] T043 [P] Contract tests for conversation message operations (2 tests) in src/test/java/com/monicahq/mcp/contract/MessageTests.java
- [x] T044 [P] Contract tests for contact field operations (4 tests) in src/test/java/com/monicahq/mcp/contract/ContactFieldTests.java
- [x] T045 [P] Contract test for contacttag_add in src/test/java/com/monicahq/mcp/contract/ContactTagAddTest.java
- [x] T046 [P] Contract test for contacttag_remove in src/test/java/com/monicahq/mcp/contract/ContactTagRemoveTest.java

### Integration Tests from Quickstart Scenarios
- [x] T047 [P] Integration test for creating contact with OAuth2 in src/test/java/com/monicahq/mcp/integration/ContactCreationTest.java
- [x] T048 [P] Integration test for contact and note workflow in src/test/java/com/monicahq/mcp/integration/ContactNoteFlowTest.java
- [x] T049 [P] Integration test for task management workflow in src/test/java/com/monicahq/mcp/integration/TaskManagementTest.java
- [x] T050 [P] Integration test for tag management workflow in src/test/java/com/monicahq/mcp/integration/TaggingFlowTest.java
- [x] T051 [P] Integration test for MCP WebSocket connection in src/test/java/com/monicahq/mcp/integration/McpConnectionTest.java
- [x] T052 [P] Integration test for OAuth2 Bearer authentication in src/test/java/com/monicahq/mcp/integration/AuthenticationTest.java

## Phase 3.3: Core Implementation (ONLY after tests are failing)

### Entity Models [P] - Can run in parallel (different files)
- [x] T053 [P] Contact entity model with OAuth2 requirements in src/main/java/com/monicahq/mcp/dto/Contact.java
- [x] T054 [P] Activity entity model in src/main/java/com/monicahq/mcp/dto/Activity.java
- [x] T055 [P] Call entity model in src/main/java/com/monicahq/mcp/dto/Call.java
- [x] T056 [P] Note entity model in src/main/java/com/monicahq/mcp/dto/Note.java
- [x] T057 [P] Task entity model in src/main/java/com/monicahq/mcp/dto/Task.java
- [x] T058 [P] Tag entity model in src/main/java/com/monicahq/mcp/dto/Tag.java
- [x] T059 [P] Reminder entity model in src/main/java/com/monicahq/mcp/dto/Reminder.java
- [x] T060 [P] JournalEntry entity model in src/main/java/com/monicahq/mcp/dto/JournalEntry.java
- [x] T061 [P] Conversation entity model in src/main/java/com/monicahq/mcp/dto/Conversation.java
- [x] T062 [P] ConversationMessage entity model in src/main/java/com/monicahq/mcp/dto/ConversationMessage.java
- [x] T063 [P] ContactField entity model in src/main/java/com/monicahq/mcp/dto/ContactField.java
- [x] T064 [P] ContactTag relationship model in src/main/java/com/monicahq/mcp/dto/ContactTag.java

### MapStruct Mappers [P] - Can run in parallel (different files)
- [x] T065 [P] Contact mapper in src/main/java/com/monicahq/mcp/mapper/ContactMapper.java
- [x] T066 [P] Activity mapper in src/main/java/com/monicahq/mcp/mapper/ActivityMapper.java
- [x] T067 [P] Call mapper in src/main/java/com/monicahq/mcp/mapper/CallMapper.java
- [x] T068 [P] Note mapper in src/main/java/com/monicahq/mcp/mapper/NoteMapper.java
- [x] T069 [P] Task mapper in src/main/java/com/monicahq/mcp/mapper/TaskMapper.java
- [x] T070 [P] Other entity mappers (7 mappers) in src/main/java/com/monicahq/mcp/mapper/

### MonicaHQ Client Implementation
- [x] T071 OAuth2 MonicaHQ REST client with WebClient in src/main/java/com/monicahq/mcp/client/MonicaHqClient.java
- [x] T072 Circuit breaker configuration with Resilience4j in src/main/java/com/monicahq/mcp/config/ResilienceConfig.java
- [x] T073 OAuth2 Bearer authentication interceptor in src/main/java/com/monicahq/mcp/client/AuthInterceptor.java

### MCP Protocol Handler
- [x] T074 MCP WebSocket controller in src/main/java/com/monicahq/mcp/controller/McpWebSocketController.java
- [x] T075 MCP message handler with tool routing in src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java
- [x] T076 MCP tool registry for 52 operations in src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java

### Service Layer Implementation
- [x] T077 ContactService with 5 operations in src/main/java/com/monicahq/mcp/service/ContactService.java
- [x] T078 ActivityService with 5 operations in src/main/java/com/monicahq/mcp/service/ActivityService.java
- [x] T079 CallService with 5 operations in src/main/java/com/monicahq/mcp/service/CallService.java
- [x] T080 NoteService with 5 operations in src/main/java/com/monicahq/mcp/service/NoteService.java
- [x] T081 TaskService with 5 operations in src/main/java/com/monicahq/mcp/service/TaskService.java
- [x] T082 [P] Other entity services (7 services) in src/main/java/com/monicahq/mcp/service/

## Phase 3.4: Extension Framework for Phase 2
- [x] T083 Create modular entity framework in src/main/java/com/monicahq/mcp/framework/EntityFramework.java
- [x] T084 [P] Plugin interface for 17 remaining entities in src/main/java/com/monicahq/mcp/framework/EntityPlugin.java
- [x] T085 [P] Configuration for Phase 2 entities in src/main/resources/entities-phase2.yml

## Phase 3.5: Integration
- [x] T086 Wire 52 services to MCP tool registry in McpToolRegistry.java
- [x] T087 Configure WebSocket endpoint for MCP in src/main/java/com/monicahq/mcp/config/WebSocketConfig.java
- [x] T088 Add correlation ID to logging in src/main/java/com/monicahq/mcp/config/LoggingConfig.java
- [x] T089 Health check endpoint with MonicaHQ connectivity in src/main/java/com/monicahq/mcp/controller/HealthController.java
- [x] T090 Error handling and MCP error responses in src/main/java/com/monicahq/mcp/exception/GlobalExceptionHandler.java

## Phase 3.6: Polish
- [ ] T091 [P] Unit tests for OAuth2 validation in src/test/java/com/monicahq/mcp/unit/ValidationTest.java
- [ ] T092 [P] Unit tests for mappers in src/test/java/com/monicahq/mcp/unit/MapperTest.java
- [ ] T093 Performance test (<500ms response) in src/test/java/com/monicahq/mcp/performance/PerformanceTest.java
- [ ] T094 [P] Create Dockerfile with multi-stage build for MonicaHQ MCP server
- [ ] T095 [P] Update README.md with Phase 1/Phase 2 roadmap and OAuth2 setup
- [ ] T096 Execute test script from quickstart.md to validate 52 operations
- [ ] T097 [P] Document Phase 2 extension process for remaining 17 entities
- [ ] T098 [P] Create plugin examples for Company and Gift entities (Phase 2 prep)

## Dependencies
- Setup (T001-T005) must complete first
- All tests (T006-T052) before any implementation (T053-T082)
- Models (T053-T064) before mappers (T065-T070)
- Client (T071-T073) before services (T077-T082)
- Services before extension framework (T083-T085)
- Extension framework before integration (T086-T090)
- Everything before polish (T091-T098)

## Parallel Execution Examples

### Launch all Contact contract tests together:
```bash
Task: "Contract test for contact_create in src/test/java/com/monicahq/mcp/contract/ContactCreateTest.java"
Task: "Contract test for contact_get in src/test/java/com/monicahq/mcp/contract/ContactGetTest.java"
Task: "Contract test for contact_update in src/test/java/com/monicahq/mcp/contract/ContactUpdateTest.java"
Task: "Contract test for contact_delete in src/test/java/com/monicahq/mcp/contract/ContactDeleteTest.java"
Task: "Contract test for contact_list in src/test/java/com/monicahq/mcp/contract/ContactListTest.java"
```

### Launch all Phase 1 entity models together:
```bash
Task: "Contact entity model with OAuth2 requirements in src/main/java/com/monicahq/mcp/dto/Contact.java"
Task: "Activity entity model in src/main/java/com/monicahq/mcp/dto/Activity.java"
Task: "Call entity model in src/main/java/com/monicahq/mcp/dto/Call.java"
Task: "Note entity model in src/main/java/com/monicahq/mcp/dto/Note.java"
Task: "Task entity model in src/main/java/com/monicahq/mcp/dto/Task.java"
Task: "Tag entity model in src/main/java/com/monicahq/mcp/dto/Tag.java"
# ... and remaining models
```

### Launch all integration tests together:
```bash
Task: "Integration test for creating contact with OAuth2 in src/test/java/com/monicahq/mcp/integration/ContactCreationTest.java"
Task: "Integration test for contact and note workflow in src/test/java/com/monicahq/mcp/integration/ContactNoteFlowTest.java"
Task: "Integration test for task management workflow in src/test/java/com/monicahq/mcp/integration/TaskManagementTest.java"
Task: "Integration test for tag management workflow in src/test/java/com/monicahq/mcp/integration/TaggingFlowTest.java"
Task: "Integration test for MCP WebSocket connection in src/test/java/com/monicahq/mcp/integration/McpConnectionTest.java"
Task: "Integration test for OAuth2 Bearer authentication in src/test/java/com/monicahq/mcp/integration/AuthenticationTest.java"
```

## Notes
- [P] tasks = different files, no dependencies
- Verify tests fail before implementing (RED phase of TDD)
- Commit after each task with descriptive message
- OAuth2 Bearer token authentication required for MonicaHQ API
- MockWebServer for MonicaHQ API mocking in tests
- Use Resilience4j for circuit breaker pattern
- Follow Spring Boot best practices for configuration
- Gradle build system with Groovy DSL
- Modular architecture supports expansion to all 29 MonicaHQ entities

## Validation Checklist
- ✅ All 52 Phase 1 MCP operations have corresponding contract tests
- ✅ All 12 core entities have model tasks
- ✅ All tests come before implementation (T006-T052 before T053-T082)
- ✅ Parallel tasks truly independent (different files)
- ✅ Each task specifies exact file path
- ✅ No task modifies same file as another [P] task in same phase
- ✅ Extension framework for Phase 2 (17 additional entities)
- ✅ OAuth2 Bearer authentication properly implemented

---
*Generated from complete MonicaHQ API specifications in `/specs/001-build-an-local/`*
*Total tasks: 98 (Setup: 5, Tests: 47, Core: 30, Extension: 3, Integration: 5, Polish: 8)*
*Phase 1: 12 core entities (52 MCP operations) | Phase 2: +17 entities (145+ total operations)*
*MonicaHQ API: 29 entities fully specified with OAuth2 authentication*