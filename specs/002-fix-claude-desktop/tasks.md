# Tasks: Fix Claude Desktop MCP Integration

**Input**: Design documents from `/specs/002-fix-claude-desktop/`
**Prerequisites**: plan.md (required), spec.md

## Execution Flow (main)
```
1. Load plan.md from feature directory
   → Extract: Java 17+, Spring Boot 3.x, Gradle, JUnit 5, MCP/STDIO
2. Load spec.md requirements:
   → FR-001 to FR-011: Tool discovery, parameter validation, debug mode
3. Generate tasks by category:
   → Setup: debug configuration, test utilities
   → Tests: contract tests for fixes, STDIO integration tests  
   → Core: parameter fixes, debug logging, protocol handling
   → Integration: Claude Desktop verification, validation scripts
   → Polish: documentation, performance tests
4. Apply task rules:
   → Different files = mark [P] for parallel
   → Same file = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001, T002...)
6. Generate dependency graph
7. Create parallel execution examples
8. Return: SUCCESS (tasks ready for execution)
```

## Format: `[ID] [P?] Description`
- **[P]**: Can run in parallel (different files, no dependencies)
- Include exact file paths in descriptions

## Path Conventions
- **Java project**: `src/main/java/com/monicahq/mcp/`, `src/test/java/com/monicahq/mcp/`
- **Resources**: `src/main/resources/`, `src/test/resources/`
- **Scripts**: `validation/` at repository root

## Phase 3.1: Setup
- [x] T001 Create debug logging configuration in src/main/resources/logback-debug.xml
- [x] T002 [P] Set up test utilities for STDIO testing in src/test/java/com/monicahq/mcp/util/StdioTestHelper.java
- [x] T003 [P] Create test fixtures for activity parameters in src/test/resources/fixtures/activity-test-data.json

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**
- [x] T004 [P] Contract test for attendees array format in src/test/java/com/monicahq/mcp/contract/ActivityAttendeesFormatTest.java
- [x] T005 [P] Integration test for STDIO activity creation in src/test/java/com/monicahq/mcp/integration/StdioActivityCreateTest.java
- [x] T006 [P] Integration test for tools/list endpoint in src/test/java/com/monicahq/mcp/integration/ToolsListValidationTest.java
- [x] T007 [P] Contract test for parameter validation errors in src/test/java/com/monicahq/mcp/contract/ParameterValidationErrorTest.java
- [x] T008 [P] Integration test for protocol version negotiation in src/test/java/com/monicahq/mcp/integration/ProtocolVersionNegotiationTest.java
- [x] T009 [P] Integration test for debug mode activation in src/test/java/com/monicahq/mcp/integration/DebugModeActivationTest.java
- [x] T010 [P] Shell script test for STDIO communication in validation/integration/test-stdio-communication.sh
- [x] T011 [P] Shell script test for all 122 tools discovery in validation/integration/test-all-tools-discovery.sh

## Phase 3.3: Core Implementation (ONLY after tests are failing)
- [x] T012 Fix attendees parameter handling in src/main/java/com/monicahq/mcp/service/ActivityService.java (lines 127-141 and 167-183)
- [x] T013 Update activity schema definition in src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java (createActivitySchema method)
- [x] T014 Add debug logging to STDIO server in src/main/java/com/monicahq/mcp/McpStdioServer.java
- [x] T015 Add debug logging to message handler in src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java
- [x] T016 [P] Create parameter formatter utility in src/main/java/com/monicahq/mcp/util/ParameterFormatter.java
- [x] T017 [P] Create detailed error builder in src/main/java/com/monicahq/mcp/exception/DetailedErrorBuilder.java
- [x] T018 Implement protocol version negotiation in src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java (handleInitialize method)
- [x] T019 [P] Create tool validator service in src/main/java/com/monicahq/mcp/service/ToolValidatorService.java
- [x] T020 [P] Create debug info provider in src/main/java/com/monicahq/mcp/util/DebugInfoProvider.java

## Phase 3.4: Integration
- [x] T021 Create Claude Desktop setup verification in validation/setup/verify-claude-desktop.sh
- [x] T022 Create all tools validation script in validation/integration/validate-all-122-tools.sh
- [x] T023 Update STDIO logging configuration in src/main/resources/logback-stdio.xml
- [x] T024 Add MCP debug configuration in src/main/java/com/monicahq/mcp/config/McpDebugConfiguration.java
- [x] T025 Create integration test runner in validation/integration/run-integration-tests.sh

## Phase 3.5: Polish
- [x] T026 [P] Document tool parameters in docs/TOOL_PARAMETERS.md
- [x] T027 [P] Create Claude Desktop setup guide in docs/CLAUDE_DESKTOP_SETUP.md
- [x] T028 [P] Create troubleshooting guide in docs/TROUBLESHOOTING.md
- [x] T029 [P] Create manual testing checklist in validation/checklists/manual-testing-checklist.md
- [x] T030 [P] Add activity request examples in docs/examples/activity-examples.json
- [x] T031 [P] Add contact request examples in docs/examples/contact-examples.json
- [x] T032 Update README with debug mode instructions in README.md
- [x] T033 Performance test for debug mode overhead in src/test/java/com/monicahq/mcp/performance/DebugModePerformanceTest.java
- [x] T034 Run full test suite validation with ./gradlew test
- [x] T035 Run complete validation suite in validation/run-all-crud-tests.sh

## Dependencies
- Tests (T004-T011) must complete before implementation (T012-T020)
- T012 (ActivityService fix) blocks T013 (schema update)
- T014 and T015 (debug logging) should be done together
- T016-T020 can run in parallel (different files)
- T021-T025 (integration scripts) depend on T012-T020
- T026-T031 (documentation) can all run in parallel
- T032 depends on T014-T015 (debug mode implementation)
- T033 depends on T014-T015 (needs debug mode to test)
- T034-T035 must run last (final validation)

## Parallel Example
```
# Phase 3.2 - Launch all test tasks together (T004-T011):
Task: "Contract test for attendees array format in src/test/java/com/monicahq/mcp/contract/ActivityAttendeesFormatTest.java"
Task: "Integration test for STDIO activity creation in src/test/java/com/monicahq/mcp/integration/StdioActivityCreateTest.java"
Task: "Integration test for tools/list endpoint in src/test/java/com/monicahq/mcp/integration/ToolsListValidationTest.java"
Task: "Contract test for parameter validation errors in src/test/java/com/monicahq/mcp/contract/ParameterValidationErrorTest.java"
Task: "Integration test for protocol version negotiation in src/test/java/com/monicahq/mcp/integration/ProtocolVersionNegotiationTest.java"
Task: "Integration test for debug mode activation in src/test/java/com/monicahq/mcp/integration/DebugModeActivationTest.java"
Task: "Shell script test for STDIO communication in validation/integration/test-stdio-communication.sh"
Task: "Shell script test for all 122 tools discovery in validation/integration/test-all-tools-discovery.sh"

# Phase 3.3 - Launch parallel utility tasks (T016, T017, T019, T020):
Task: "Create parameter formatter utility in src/main/java/com/monicahq/mcp/util/ParameterFormatter.java"
Task: "Create detailed error builder in src/main/java/com/monicahq/mcp/exception/DetailedErrorBuilder.java"
Task: "Create tool validator service in src/main/java/com/monicahq/mcp/service/ToolValidatorService.java"
Task: "Create debug info provider in src/main/java/com/monicahq/mcp/util/DebugInfoProvider.java"

# Phase 3.5 - Launch all documentation tasks together (T026-T031):
Task: "Document tool parameters in docs/TOOL_PARAMETERS.md"
Task: "Create Claude Desktop setup guide in docs/CLAUDE_DESKTOP_SETUP.md"
Task: "Create troubleshooting guide in docs/TROUBLESHOOTING.md"
Task: "Create manual testing checklist in validation/checklists/manual-testing-checklist.md"
Task: "Add activity request examples in docs/examples/activity-examples.json"
Task: "Add contact request examples in docs/examples/contact-examples.json"
```

## Critical Path
The following tasks are on the critical path and block other work:
1. **T004-T011** (All tests) - Must fail before any implementation
2. **T012** (Fix ActivityService attendees) - Core bug fix that blocks T013
3. **T014-T015** (Debug logging) - Must be done together for consistency
4. **T034** (Test suite validation) - Final gate before completion

## Success Criteria
- [ ] All tests in Phase 3.2 are written and initially FAIL
- [ ] ActivityAttendeesFormatTest passes with both array and object formats
- [ ] StdioActivityCreateTest successfully creates activity via STDIO
- [ ] All 122 tools appear in tools/list response
- [ ] Debug mode logs to stderr only (no stdout contamination)
- [ ] Claude Desktop can invoke activity_create without "tool not found" errors
- [ ] All 188 existing tests still pass
- [ ] Performance overhead of debug mode < 5%

## Notes
- **[P]** tasks = different files, no dependencies
- Verify tests fail before implementing (TDD principle)
- Commit after each task completion
- T012 is the highest priority bug fix
- Keep STDIO output clean - debug logs to stderr only
- Use feature flags for new functionality where possible

## Task Generation Rules Applied
1. **From spec.md requirements**:
   - FR-001 → T006 (tools/list validation)
   - FR-002 → T007 (parameter validation)
   - FR-003 → T004, T012 (attendees format fix)
   - FR-004 → T017 (error builder)
   - FR-005 → T014, T015 (debug logging)
   - FR-006 → T005, T010 (STDIO testing)
   - FR-007 → T021 (verification script)
   - FR-008 → T034 (backward compatibility)

2. **From plan.md components**:
   - ActivityService.java → T012
   - McpToolRegistry.java → T013
   - McpStdioServer.java → T014
   - McpMessageHandler.java → T015, T018

3. **Ordering applied**:
   - Setup → Tests → Core → Integration → Polish
   - TDD enforced: Tests before implementation
   - Parallel execution maximized where possible

## Validation Checklist
*GATE: Checked by main() before returning*

- [x] All functional requirements have corresponding tests
- [x] All tests come before implementation (TDD)
- [x] Parallel tasks truly independent (different files)
- [x] Each task specifies exact file path
- [x] No [P] task modifies same file as another [P] task
- [x] Critical path identified
- [x] Success criteria measurable