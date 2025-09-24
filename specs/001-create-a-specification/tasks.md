# Tasks: API Gap Fixes

**Input**: Design documents from `/specs/001-create-a-specification/`
**Prerequisites**: spec.md (available), analysis from docs/monica-api-diff-analysis.md

## Execution Flow (main)
```
1. Load spec.md from feature directory
   → Found: API Gap Fixes specification
   → Extract: Missing entities (Users, Compliance), missing Contact operations
2. Load optional design documents:
   → No additional design docs yet (planning phase pending)
   → Work from spec and existing codebase structure
3. Generate tasks by category:
   → Setup: API endpoint investigation, error handling updates
   → Tests: Contract tests for new operations
   → Core: Services for missing operations, DTO models
   → Integration: Contact service extensions, new entity services
   → Polish: Documentation updates, validation tests
4. Apply task rules:
   → Different services = mark [P] for parallel
   → Same ContactService = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001, T002...)
6. Generate dependency graph
7. Create parallel execution examples
8. Return: SUCCESS (tasks ready for execution)
```

## Format: `[ID] [P?] Description`
- **[P]**: Can run in parallel (different files, no dependencies)
- Include exact file paths in descriptions

## Phase 3.1: Setup & Investigation
- [x] T001 Investigate Users API endpoints and admin requirements via Monica Docker instance
- [x] T002 Investigate Compliance API endpoints and determine available operations
- [x] T003 [P] Update ContactService error handling for graceful 404 responses in `/src/main/java/com/monicahq/mcp/service/ContactService.java`

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**
- [x] T004 [P] Contract test for contact search in `/src/test/java/com/monicahq/mcp/contract/ContactSearchTest.java`
- [x] T005 [P] Contract test for contact career update in `/src/test/java/com/monicahq/mcp/contract/ContactCareerUpdateTest.java`
- [x] T006 [P] Contract test for contact audit logs in `/src/test/java/com/monicahq/mcp/contract/ContactAuditLogTest.java`
- [x] T007 [P] Contract test for contacts by tag in `/src/test/java/com/monicahq/mcp/contract/ContactsByTagTest.java`
- [x] T008 [P] Contract test for Users operations in `/src/test/java/com/monicahq/mcp/contract/UserCreateTest.java`
- [x] T009 [P] Integration test for contact search workflow in `/src/test/java/com/monicahq/mcp/integration/ContactSearchFlowTest.java`

## Phase 3.3: DTO Models (ONLY after tests are failing)
- [x] T010 [P] Users DTO model in `/src/main/java/com/monicahq/mcp/dto/User.java`
- [x] T011 [P] Compliance DTO model in `/src/main/java/com/monicahq/mcp/dto/Compliance.java`
- [x] T012 [P] ContactCareer DTO model in `/src/main/java/com/monicahq/mcp/dto/ContactCareer.java`
- [x] T013 [P] ContactAuditLog DTO model in `/src/main/java/com/monicahq/mcp/dto/ContactAuditLog.java`

## Phase 3.4: Service Implementation
- [x] T014 Add contact search method to ContactService in `/src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T015 Add contact career update method to ContactService in `/src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T016 Add contact audit logs method to ContactService in `/src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T017 Add contacts by tag method to TagService in `/src/main/java/com/monicahq/mcp/service/TagService.java`
- [x] T018 [P] UserService CRUD operations in `/src/main/java/com/monicahq/mcp/service/UserService.java`
- [x] T019 [P] ComplianceService operations in `/src/main/java/com/monicahq/mcp/service/ComplianceService.java`

## Phase 3.5: MCP Tool Registry Integration
- [x] T020 Register contact search operation in McpToolRegistry
- [x] T021 Register contact career update operation in McpToolRegistry
- [x] T022 Register contact audit logs operation in McpToolRegistry
- [x] T023 Register contacts by tag operation in McpToolRegistry
- [x] T024 [P] Register Users operations in McpToolRegistry (if available)
- [x] T025 [P] Register Compliance operations in McpToolRegistry (if available)

## Phase 3.6: Error Handling & Documentation
- [x] T026 [P] Update GlobalExceptionHandler for new entity errors in `/src/main/java/com/monicahq/mcp/exception/GlobalExceptionHandler.java`
- [x] T027 Update API gap documentation in `/docs/monica-api-diff-analysis.md`
- [x] T028 [P] Add unit tests for new Contact operations in `/src/test/java/com/monicahq/mcp/service/ContactServiceTest.java`

## Phase 3.7: Polish
- [ ] T029 [P] Update Swagger documentation for new operations
- [ ] T030 [P] Performance tests for search operations (<500ms)
- [ ] T031 Validation tests for all new operations
- [ ] T032 Manual testing against Docker Monica instance

## Dependencies
- Investigation (T001-T002) before any implementation
- Tests (T004-T009) before implementation (T010-T025)
- DTOs (T010-T013) before services (T014-T019)
- Services before registry integration (T020-T025)
- Core implementation before polish (T026-T032)
- T014-T017 are sequential (same ContactService file)
- T020-T023 are sequential (same McpToolRegistry file)

## Parallel Example
```
# Launch T004-T008 together (different test files):
Task: "Contract test for contact search in /src/test/java/com/monicahq/mcp/contract/ContactSearchTest.java"
Task: "Contract test for contact career update in /src/test/java/com/monicahq/mcp/contract/ContactCareerUpdateTest.java"
Task: "Contract test for contact audit logs in /src/test/java/com/monicahq/mcp/contract/ContactAuditLogTest.java"
Task: "Contract test for contacts by tag in /src/test/java/com/monicahq/mcp/contract/ContactsByTagTest.java"
Task: "Contract test for Users operations in /src/test/java/com/monicahq/mcp/contract/UserCreateTest.java"

# Launch T010-T013 together (different DTO files):
Task: "Users DTO model in /src/main/java/com/monicahq/mcp/dto/User.java"
Task: "Compliance DTO model in /src/main/java/com/monicahq/mcp/dto/Compliance.java"
Task: "ContactCareer DTO model in /src/main/java/com/monicahq/mcp/dto/ContactCareer.java"
Task: "ContactAuditLog DTO model in /src/main/java/com/monicahq/mcp/dto/ContactAuditLog.java"
```

## Notes
- [P] tasks = different files, no dependencies
- T014-T017 modify same ContactService file, so must be sequential
- T020-T023 modify same McpToolRegistry file, so must be sequential
- Users and Compliance entities may not be implementable (404 responses)
- Focus on Contact operations which have clear endpoints
- Verify tests fail before implementing
- Test against Docker Monica instance for validation

## Task Generation Rules Applied
1. **From Spec Requirements**: Each FR requirement → implementation task
2. **From Missing Operations**: 4 Contact operations + 2 entities → 6 core tasks
3. **From Existing Structure**: Follow Spring Boot service layer pattern
4. **TDD Ordering**: Contract tests → DTOs → Services → Integration
5. **File Dependencies**: Same file tasks are sequential, different files are parallel

## Validation Checklist
- [x] All missing operations have corresponding tests
- [x] All new entities have DTO model tasks
- [x] All tests come before implementation
- [x] Parallel tasks are truly independent (different files)
- [x] Each task specifies exact file path
- [x] No task modifies same file as another [P] task in same phase