# Tasks: Constitutional Governance Framework and Validation Suite

**Input**: Feature specification from `/specs/003-constitutional-governance-framework/spec.md`
**Prerequisites**: Constitution v1.2.0 with new Principle VI (MCP Response Content Visibility)

## Feature Overview
Implement constitutional governance framework with comprehensive validation suite for MCP server quality assurance, including 6 core principles (updated from 5), automated compliance checking, and 7-phase testing methodology.

## Current Implementation Status
- ✅ Validation scripts created: `validate-constitution.sh`, `test-mcp-complete.sh`, `test-claude-desktop.sh`, `setup-mcp-tools.sh`
- ✅ Documentation created: `MCP-STDIO-LOGGING.md`
- ✅ Constitution document updated to v1.2.0 with Principle VI
- ⚠️ New Principle VI requires implementation across all 52 operations
- ❌ Integration testing needed for content visibility
- ❌ Tests need updating for content field verification

## Phase 3.1: Constitutional Framework Updates
- [x] T001 Update plan template references to constitution v1.2.0 in `.specify/templates/plan-template.md`
- [x] T002 Create constitutional compliance test cases for 6 principles in `tests/constitutional/test_compliance.sh`
- [x] T003 Update validation script to check all 6 principles in `validate-constitution.sh`

## Phase 3.2: Content Visibility Testing (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation improvements**
- [x] T004 [P] Test MCP Protocol First principle compliance in `tests/constitutional/test_mcp_protocol_first.sh`
- [x] T005 [P] Test TDD compliance verification in `tests/constitutional/test_tdd_compliance.sh`
- [x] T006 [P] Test Spring Boot Architecture patterns in `tests/constitutional/test_spring_architecture.sh`
- [x] T007 [P] Test Production Readiness requirements in `tests/constitutional/test_production_ready.sh`
- [x] T008 [P] Test Type Safety enforcement in `tests/constitutional/test_type_safety.sh`
- [x] T009 [P] Test MCP Response Content Visibility (NEW) in `tests/constitutional/test_content_visibility.sh`
- [x] T010 [P] Test content field formatting for Contact operations in `tests/contract/test_contact_content.sh`
- [x] T011 [P] Test content field formatting for Activity operations in `tests/contract/test_activity_content.sh`
- [x] T012 [P] Test content field formatting for Task operations in `tests/contract/test_task_content.sh`
- [x] T013 [P] Test content field formatting for Note operations in `tests/contract/test_note_content.sh`
- [x] T014 [P] Integration test for complete validation suite in `tests/integration/test_validation_suite_integration.sh`
- [x] T015 [P] STDOUT contamination detection test in `tests/integration/test_stdout_cleanliness.sh`

## Phase 3.3: Content Visibility Implementation (ONLY after tests are failing)
**Implement formatters for all 52 MCP operations to comply with Principle VI**

### Contact Entity Operations
- [x] T016 Update ContactService.create to format content field in `src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T017 Update ContactService.get to format content field in `src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T018 Update ContactService.update to format content field in `src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T019 Update ContactService.delete to format content field in `src/main/java/com/monicahq/mcp/service/ContactService.java`
- [x] T020 Update ContactService.list to format content field in `src/main/java/com/monicahq/mcp/service/ContactService.java`

### Activity Entity Operations
- [x] T021 Update ActivityService.create to format content field in `src/main/java/com/monicahq/mcp/service/ActivityService.java`
- [x] T022 Update ActivityService.get to format content field in `src/main/java/com/monicahq/mcp/service/ActivityService.java`
- [x] T023 Update ActivityService.update to format content field in `src/main/java/com/monicahq/mcp/service/ActivityService.java`
- [x] T024 Update ActivityService.delete to format content field in `src/main/java/com/monicahq/mcp/service/ActivityService.java`
- [x] T025 Update ActivityService.list to format content field in `src/main/java/com/monicahq/mcp/service/ActivityService.java`

### Task Entity Operations
- [x] T026 Update TaskService.create to format content field in `src/main/java/com/monicahq/mcp/service/TaskService.java`
- [x] T027 Update TaskService.get to format content field in `src/main/java/com/monicahq/mcp/service/TaskService.java`
- [x] T028 Update TaskService.update to format content field in `src/main/java/com/monicahq/mcp/service/TaskService.java`
- [x] T029 Update TaskService.delete to format content field in `src/main/java/com/monicahq/mcp/service/TaskService.java`
- [x] T030 Update TaskService.list to format content field in `src/main/java/com/monicahq/mcp/service/TaskService.java`

### Note Entity Operations
- [x] T031 Update NoteService.create to format content field in `src/main/java/com/monicahq/mcp/service/NoteService.java`
- [x] T032 Update NoteService.get to format content field in `src/main/java/com/monicahq/mcp/service/NoteService.java`
- [x] T033 Update NoteService.update to format content field in `src/main/java/com/monicahq/mcp/service/NoteService.java`
- [x] T034 Update NoteService.delete to format content field in `src/main/java/com/monicahq/mcp/service/NoteService.java`
- [x] T035 Update NoteService.list to format content field in `src/main/java/com/monicahq/mcp/service/NoteService.java`

### Other Entity Operations (Parallel Group)
- [x] T036 [P] Update Call operations content formatting in `src/main/java/com/monicahq/mcp/service/CallService.java`
- [x] T037 [P] Update Tag operations content formatting in `src/main/java/com/monicahq/mcp/service/TagService.java`
- [x] T038 [P] Update Reminder operations content formatting in `src/main/java/com/monicahq/mcp/service/ReminderService.java`
- [x] T039 [P] Update JournalEntry operations content formatting in `src/main/java/com/monicahq/mcp/service/JournalEntryService.java`
- [x] T040 [P] Update Conversation operations content formatting in `src/main/java/com/monicahq/mcp/service/ConversationService.java`
- [x] T041 [P] Update ConversationMessage operations content formatting in `src/main/java/com/monicahq/mcp/service/ConversationMessageService.java`
- [x] T042 [P] Update ContactField operations content formatting in `src/main/java/com/monicahq/mcp/service/ContactFieldService.java`
- [x] T043 [P] Update ContactTag operations content formatting in `src/main/java/com/monicahq/mcp/service/ContactTagService.java`

## Phase 3.4: Response Formatter Utilities
- [x] T044 Create ContentFormatter utility class in `src/main/java/com/monicahq/mcp/util/ContentFormatter.java`
- [x] T045 Implement entity-specific formatting templates in ContentFormatter
- [x] T046 Add list response formatting with pagination info in ContentFormatter
- [x] T047 Create formatting tests for ContentFormatter in `src/test/java/com/monicahq/mcp/util/ContentFormatterTest.java`

## Phase 3.5: Validation Framework Enhancement
- [ ] T048 Update constitutional validation script with Principle VI checks in `validate-constitution.sh`
- [ ] T049 Add content visibility verification to comprehensive test suite in `test-mcp-complete.sh`
- [ ] T050 Integrate content field validation into Claude Desktop testing in `test-claude-desktop.sh`
- [ ] T051 Update validation reporting to highlight content visibility issues
- [ ] T052 Add remediation guidance for content formatting violations

## Phase 3.6: Test Updates for Content Visibility
- [ ] T053 Update all 136 existing tests to verify content field formatting
- [ ] T054 Add content field assertions to contract tests in `src/test/java/com/monicahq/mcp/contract/`
- [ ] T055 Update integration tests to validate Claude Desktop visibility in `src/test/java/com/monicahq/mcp/integration/`
- [ ] T056 Create regression tests for content field completeness

## Phase 3.7: Documentation and Integration
- [ ] T057 Update CLAUDE.md with Principle VI documentation in `CLAUDE.md`
- [ ] T058 Update README.md with content visibility requirements in `README.md`
- [ ] T059 Create content formatting guide in `CONTENT-FORMATTING-GUIDE.md`
- [ ] T060 Update TESTING-GUIDE.md with content visibility testing in `TESTING-GUIDE.md`
- [ ] T061 Document constitutional amendment process for v1.2.0
- [ ] T062 Update CI/CD pipeline for content field validation

## Phase 3.8: Quality Assurance and Polish
- [ ] T063 [P] Performance validation for content formatting overhead
- [ ] T064 [P] MCP Inspector validation for content field visibility
- [ ] T065 [P] Claude Desktop integration testing with formatted responses
- [ ] T066 Cross-entity consistency validation for content formatting
- [ ] T067 Final constitutional compliance verification (all 6 principles)
- [ ] T068 Comprehensive test suite execution (<5 minutes target)

## Dependencies
- Constitutional tests (T004-T015) MUST complete before implementation (T016-T043)
- ContentFormatter utility (T044-T047) should be created before service updates
- Service updates (T016-T043) can run in parallel per entity type
- Test updates (T053-T056) after implementation complete
- Documentation (T057-T062) can run parallel with implementation

## Parallel Execution Example
```bash
# Launch content visibility tests for different entities:
Task: "Test content field formatting for Contact operations in tests/contract/test_contact_content.sh"
Task: "Test content field formatting for Activity operations in tests/contract/test_activity_content.sh" 
Task: "Test content field formatting for Task operations in tests/contract/test_task_content.sh"
Task: "Test content field formatting for Note operations in tests/contract/test_note_content.sh"

# After tests fail, update services in parallel:
Task: "Update Call operations content formatting in src/main/java/com/monicahq/mcp/service/CallService.java"
Task: "Update Tag operations content formatting in src/main/java/com/monicahq/mcp/service/TagService.java"
Task: "Update Reminder operations content formatting in src/main/java/com/monicahq/mcp/service/ReminderService.java"
```

## Key Implementation Notes
- **Constitution Version**: v1.2.0 with 6 core principles (Principle VI added)
- **Content Field Priority**: All data MUST be readable in content field for Claude Desktop
- **Backward Compatibility**: Keep data field for protocol compliance
- **Test Coverage**: Must maintain 100% coverage (136+ tests)
- **Format Standards**: Human-readable, structured, scannable content

## Validation Success Criteria
- [ ] All 6 constitutional principles validated in compliance script
- [ ] Content field contains complete data for all 52 operations
- [ ] Claude Desktop can access all response data via content field
- [ ] All 136+ tests updated and passing with content verification
- [ ] STDOUT contamination detection functional
- [ ] Performance impact of formatting <100ms per operation
- [ ] Documentation comprehensive for content visibility requirements
- [ ] CI/CD pipeline enforces content field validation

## File Path Conventions
- **Constitutional tests**: `tests/constitutional/`
- **Contract tests**: `tests/contract/` and `src/test/java/com/monicahq/mcp/contract/`
- **Integration tests**: `tests/integration/` and `src/test/java/com/monicahq/mcp/integration/`
- **Service implementations**: `src/main/java/com/monicahq/mcp/service/`
- **Utilities**: `src/main/java/com/monicahq/mcp/util/`
- **Documentation**: Repository root (`CLAUDE.md`, `README.md`, etc.)
- **Validation scripts**: Repository root (`validate-constitution.sh`, etc.)
- **Constitution**: `.specify/memory/constitution.md`

## Notes
- [P] tasks = different files, can run in parallel
- TDD requirement: Tests MUST fail before implementation
- Each service method needs individual content formatting
- Commit after each completed task with validation
- Focus on Claude Desktop visibility throughout implementation