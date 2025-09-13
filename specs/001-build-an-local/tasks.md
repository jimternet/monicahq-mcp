# Tasks: MonicaHQ MCP Server Integration

**Input**: Design documents from `/specs/001-build-an-local/`
**Prerequisites**: plan.md (required), research.md, data-model.md, contracts/

## Execution Flow (main)
```
1. Load plan.md from feature directory
   → Tech stack: Java 17+, Spring Boot 3.x, Maven
   → Libraries: WebFlux, WebSocket, Resilience4j, MapStruct, Lombok
   → Structure: Single project with src/ and tests/ at root
2. Load optional design documents:
   → data-model.md: 12 entities (Contact, Activity, Call, Note, Task, Tag, Reminder, JournalEntry, Conversation, ConversationMessage, ContactField, ContactTag)
   → contracts/mcp-protocol.json: 52 MCP operations
   → research.md: Technical decisions (WebFlux, Resilience4j, MockWebServer)
3. Generate tasks by category:
   → Setup: Spring Boot init, Maven dependencies, configuration
   → Tests: 52 contract tests for MCP operations
   → Core: 12 entity models, 12 services, MCP handler
   → Integration: MonicaHQ client, WebSocket, circuit breaker
   → Polish: unit tests, performance validation, documentation
4. Apply task rules:
   → Different files = mark [P] for parallel
   → Same file = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001-T080)
6. Generate dependency graph
7. Create parallel execution examples
8. Validate task completeness:
   → All 52 MCP operations have tests
   → All 12 entities have models
   → All endpoints implemented
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
- [ ] T001 Initialize Spring Boot project via start.spring.io with WebFlux, WebSocket, Validation
- [ ] T002 Configure Maven pom.xml with Resilience4j, MapStruct, Lombok, MockWebServer dependencies
- [ ] T003 [P] Create application.yml with MonicaHQ API and MCP WebSocket configuration
- [ ] T004 [P] Set up SLF4J structured logging configuration in src/main/resources/logback-spring.xml
- [ ] T005 [P] Create .env.example with MONICA_API_URL and MONICA_API_TOKEN placeholders

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**

### Contract Tests - Contact Entity (5 operations)
- [ ] T006 [P] Contract test for contact_create in src/test/java/com/monicahq/mcp/contract/ContactCreateTest.java
- [ ] T007 [P] Contract test for contact_get in src/test/java/com/monicahq/mcp/contract/ContactGetTest.java
- [ ] T008 [P] Contract test for contact_update in src/test/java/com/monicahq/mcp/contract/ContactUpdateTest.java
- [ ] T009 [P] Contract test for contact_delete in src/test/java/com/monicahq/mcp/contract/ContactDeleteTest.java
- [ ] T010 [P] Contract test for contact_list in src/test/java/com/monicahq/mcp/contract/ContactListTest.java

### Contract Tests - Activity Entity (5 operations)
- [ ] T011 [P] Contract test for activity_create in src/test/java/com/monicahq/mcp/contract/ActivityCreateTest.java
- [ ] T012 [P] Contract test for activity_get in src/test/java/com/monicahq/mcp/contract/ActivityGetTest.java
- [ ] T013 [P] Contract test for activity_update in src/test/java/com/monicahq/mcp/contract/ActivityUpdateTest.java
- [ ] T014 [P] Contract test for activity_delete in src/test/java/com/monicahq/mcp/contract/ActivityDeleteTest.java
- [ ] T015 [P] Contract test for activity_list in src/test/java/com/monicahq/mcp/contract/ActivityListTest.java

### Contract Tests - Call Entity (5 operations)
- [ ] T016 [P] Contract test for call_create in src/test/java/com/monicahq/mcp/contract/CallCreateTest.java
- [ ] T017 [P] Contract test for call_get in src/test/java/com/monicahq/mcp/contract/CallGetTest.java
- [ ] T018 [P] Contract test for call_update in src/test/java/com/monicahq/mcp/contract/CallUpdateTest.java
- [ ] T019 [P] Contract test for call_delete in src/test/java/com/monicahq/mcp/contract/CallDeleteTest.java
- [ ] T020 [P] Contract test for call_list in src/test/java/com/monicahq/mcp/contract/CallListTest.java

### Contract Tests - Note Entity (5 operations)
- [ ] T021 [P] Contract test for note_create in src/test/java/com/monicahq/mcp/contract/NoteCreateTest.java
- [ ] T022 [P] Contract test for note_get in src/test/java/com/monicahq/mcp/contract/NoteGetTest.java
- [ ] T023 [P] Contract test for note_update in src/test/java/com/monicahq/mcp/contract/NoteUpdateTest.java
- [ ] T024 [P] Contract test for note_delete in src/test/java/com/monicahq/mcp/contract/NoteDeleteTest.java
- [ ] T025 [P] Contract test for note_list in src/test/java/com/monicahq/mcp/contract/NoteListTest.java

### Contract Tests - Task Entity (5 operations)
- [ ] T026 [P] Contract test for task_create in src/test/java/com/monicahq/mcp/contract/TaskCreateTest.java
- [ ] T027 [P] Contract test for task_get in src/test/java/com/monicahq/mcp/contract/TaskGetTest.java
- [ ] T028 [P] Contract test for task_update in src/test/java/com/monicahq/mcp/contract/TaskUpdateTest.java
- [ ] T029 [P] Contract test for task_delete in src/test/java/com/monicahq/mcp/contract/TaskDeleteTest.java
- [ ] T030 [P] Contract test for task_list in src/test/java/com/monicahq/mcp/contract/TaskListTest.java

### Contract Tests - Other Entities (27 operations)
- [ ] T031 [P] Contract tests for Tag operations (5 tests) in src/test/java/com/monicahq/mcp/contract/TagTests.java
- [ ] T032 [P] Contract tests for Reminder operations (5 tests) in src/test/java/com/monicahq/mcp/contract/ReminderTests.java
- [ ] T033 [P] Contract tests for JournalEntry operations (5 tests) in src/test/java/com/monicahq/mcp/contract/JournalTests.java
- [ ] T034 [P] Contract tests for Conversation operations (5 tests) in src/test/java/com/monicahq/mcp/contract/ConversationTests.java
- [ ] T035 [P] Contract tests for ConversationMessage operations (2 tests) in src/test/java/com/monicahq/mcp/contract/MessageTests.java
- [ ] T036 [P] Contract tests for ContactField operations (4 tests) in src/test/java/com/monicahq/mcp/contract/ContactFieldTests.java
- [ ] T037 [P] Contract test for contacttag_add in src/test/java/com/monicahq/mcp/contract/ContactTagAddTest.java
- [ ] T038 [P] Contract test for contacttag_remove in src/test/java/com/monicahq/mcp/contract/ContactTagRemoveTest.java

### Integration Tests from Quickstart Scenarios
- [ ] T039 [P] Integration test for creating contact and adding note in src/test/java/com/monicahq/mcp/integration/ContactNoteFlowTest.java
- [ ] T040 [P] Integration test for task creation and completion in src/test/java/com/monicahq/mcp/integration/TaskManagementTest.java
- [ ] T041 [P] Integration test for tag management flow in src/test/java/com/monicahq/mcp/integration/TaggingFlowTest.java
- [ ] T042 [P] Integration test for MCP WebSocket connection in src/test/java/com/monicahq/mcp/integration/McpConnectionTest.java

## Phase 3.3: Core Implementation (ONLY after tests are failing)

### Entity Models [P] - Can run in parallel (different files)
- [ ] T043 [P] Contact entity model in src/main/java/com/monicahq/mcp/dto/Contact.java
- [ ] T044 [P] Activity entity model in src/main/java/com/monicahq/mcp/dto/Activity.java
- [ ] T045 [P] Call entity model in src/main/java/com/monicahq/mcp/dto/Call.java
- [ ] T046 [P] Note entity model in src/main/java/com/monicahq/mcp/dto/Note.java
- [ ] T047 [P] Task entity model in src/main/java/com/monicahq/mcp/dto/Task.java
- [ ] T048 [P] Tag entity model in src/main/java/com/monicahq/mcp/dto/Tag.java
- [ ] T049 [P] Reminder entity model in src/main/java/com/monicahq/mcp/dto/Reminder.java
- [ ] T050 [P] JournalEntry entity model in src/main/java/com/monicahq/mcp/dto/JournalEntry.java
- [ ] T051 [P] Conversation entity model in src/main/java/com/monicahq/mcp/dto/Conversation.java
- [ ] T052 [P] ConversationMessage entity model in src/main/java/com/monicahq/mcp/dto/ConversationMessage.java
- [ ] T053 [P] ContactField entity model in src/main/java/com/monicahq/mcp/dto/ContactField.java
- [ ] T054 [P] ContactTag relationship model in src/main/java/com/monicahq/mcp/dto/ContactTag.java

### MapStruct Mappers [P] - Can run in parallel (different files)
- [ ] T055 [P] Contact mapper in src/main/java/com/monicahq/mcp/mapper/ContactMapper.java
- [ ] T056 [P] Activity mapper in src/main/java/com/monicahq/mcp/mapper/ActivityMapper.java
- [ ] T057 [P] Other entity mappers (10 mappers) in src/main/java/com/monicahq/mcp/mapper/

### MCP Protocol Handler
- [ ] T058 MCP WebSocket controller in src/main/java/com/monicahq/mcp/controller/McpWebSocketController.java
- [ ] T059 MCP message handler in src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java
- [ ] T060 MCP tool registry in src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java

### MonicaHQ Client
- [ ] T061 MonicaHQ REST client with WebClient in src/main/java/com/monicahq/mcp/client/MonicaHqClient.java
- [ ] T062 Circuit breaker configuration in src/main/java/com/monicahq/mcp/config/ResilienceConfig.java
- [ ] T063 Authentication interceptor in src/main/java/com/monicahq/mcp/client/AuthInterceptor.java

### Service Layer Implementation
- [ ] T064 ContactService with 5 operations in src/main/java/com/monicahq/mcp/service/ContactService.java
- [ ] T065 ActivityService with 5 operations in src/main/java/com/monicahq/mcp/service/ActivityService.java
- [ ] T066 CallService with 5 operations in src/main/java/com/monicahq/mcp/service/CallService.java
- [ ] T067 NoteService with 5 operations in src/main/java/com/monicahq/mcp/service/NoteService.java
- [ ] T068 TaskService with 5 operations in src/main/java/com/monicahq/mcp/service/TaskService.java
- [ ] T069 [P] Other entity services (7 services) in src/main/java/com/monicahq/mcp/service/

## Phase 3.4: Integration
- [ ] T070 Wire services to MCP tool registry in McpToolRegistry.java
- [ ] T071 Configure WebSocket endpoint in src/main/java/com/monicahq/mcp/config/WebSocketConfig.java
- [ ] T072 Add correlation ID to logging in src/main/java/com/monicahq/mcp/config/LoggingConfig.java
- [ ] T073 Health check endpoint in src/main/java/com/monicahq/mcp/controller/HealthController.java
- [ ] T074 Error handling and MCP error responses in src/main/java/com/monicahq/mcp/exception/GlobalExceptionHandler.java

## Phase 3.5: Polish
- [ ] T075 [P] Unit tests for input validation in src/test/java/com/monicahq/mcp/unit/ValidationTest.java
- [ ] T076 [P] Unit tests for mappers in src/test/java/com/monicahq/mcp/unit/MapperTest.java
- [ ] T077 Performance test (<500ms response) in src/test/java/com/monicahq/mcp/performance/PerformanceTest.java
- [ ] T078 [P] Create Dockerfile with multi-stage build
- [ ] T079 [P] Update README.md with setup and usage instructions
- [ ] T080 Execute test script from quickstart.md to validate all operations

## Dependencies
- Setup (T001-T005) must complete first
- All tests (T006-T042) before any implementation (T043-T069)
- Models (T043-T054) before mappers (T055-T057)
- Client (T061-T063) before services (T064-T069)
- Services before integration (T070-T074)
- Everything before polish (T075-T080)

## Parallel Execution Examples

### Launch all Contact contract tests together:
```bash
Task: "Contract test for contact_create in src/test/java/com/monicahq/mcp/contract/ContactCreateTest.java"
Task: "Contract test for contact_get in src/test/java/com/monicahq/mcp/contract/ContactGetTest.java"
Task: "Contract test for contact_update in src/test/java/com/monicahq/mcp/contract/ContactUpdateTest.java"
Task: "Contract test for contact_delete in src/test/java/com/monicahq/mcp/contract/ContactDeleteTest.java"
Task: "Contract test for contact_list in src/test/java/com/monicahq/mcp/contract/ContactListTest.java"
```

### Launch all entity models together:
```bash
Task: "Contact entity model in src/main/java/com/monicahq/mcp/dto/Contact.java"
Task: "Activity entity model in src/main/java/com/monicahq/mcp/dto/Activity.java"
Task: "Call entity model in src/main/java/com/monicahq/mcp/dto/Call.java"
Task: "Note entity model in src/main/java/com/monicahq/mcp/dto/Note.java"
Task: "Task entity model in src/main/java/com/monicahq/mcp/dto/Task.java"
# ... and remaining models
```

## Notes
- [P] tasks = different files, no dependencies
- Verify tests fail before implementing (RED phase of TDD)
- Commit after each task with descriptive message
- MockWebServer for MonicaHQ API mocking in tests
- Use Resilience4j for circuit breaker pattern
- Follow Spring Boot best practices for configuration

## Validation Checklist
- ✅ All 52 MCP operations have corresponding contract tests
- ✅ All 12 entities have model tasks
- ✅ All tests come before implementation (T006-T042 before T043-T074)
- ✅ Parallel tasks truly independent (different files)
- ✅ Each task specifies exact file path
- ✅ No task modifies same file as another [P] task in same phase

---
*Generated from specifications in `/specs/001-build-an-local/`*
*Total tasks: 80 (Setup: 5, Tests: 37, Core: 27, Integration: 5, Polish: 6)*