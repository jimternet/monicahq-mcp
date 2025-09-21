# Tasks: Critical Monica API Gaps Implementation

**Input**: Design documents from `/specs/005-use-the-findings/`
**Prerequisites**: spec.md (available), existing Spring Boot MCP architecture
**Feature**: Relationship Management and Company Tracking

## Execution Flow (main)
```
1. Load spec.md from feature directory
   → Feature focuses on relationships and companies (Critical gaps)
   → Extract: 4 new entities, 14 new endpoints
2. Leverage existing architecture:
   → Spring Boot 3.x, WebFlux, MapStruct, Lombok
   → Established MCP tool registry pattern
   → Existing Monica API client structure
3. Generate tasks by category:
   → Setup: Dependencies, DTO models
   → Tests: Contract tests for all new endpoints
   → Core: Services, client methods, MCP tools
   → Integration: Tool registry, validation
   → Polish: CRUD validation, documentation
4. Apply task rules:
   → Different files = mark [P] for parallel
   → Same file = sequential (no [P])
   → Tests before implementation (TDD)
5. Number tasks sequentially (T001, T002...)
6. Generate dependency graph based on Spring patterns
7. Create parallel execution examples
8. Target: 14 new MCP operations for relationship/company management
```

## Format: `[ID] [P?] Description`
- **[P]**: Can run in parallel (different files, no dependencies)
- Include exact file paths in descriptions

## Phase 3.1: Setup & Models
- [x] T001 [P] Create Relationship DTO in `src/main/java/com/monicahq/mcp/dto/Relationship.java`
- [x] T002 [P] Create Company DTO in `src/main/java/com/monicahq/mcp/dto/Company.java`
- [x] T003 [P] Create RelationshipType DTO in `src/main/java/com/monicahq/mcp/dto/RelationshipType.java`
- [x] T004 [P] Create RelationshipTypeGroup DTO in `src/main/java/com/monicahq/mcp/dto/RelationshipTypeGroup.java`
- [x] T005 [P] Create Relationship mapper in `src/main/java/com/monicahq/mcp/mapper/RelationshipMapper.java`
- [x] T006 [P] Create Company mapper in `src/main/java/com/monicahq/mcp/mapper/CompanyMapper.java`

## Phase 3.2: Tests First (TDD) ⚠️ MUST COMPLETE BEFORE 3.3
**CRITICAL: These tests MUST be written and MUST FAIL before ANY implementation**

### Relationship Tests
- [x] T007 [P] Contract test relationship create in `src/test/java/com/monicahq/mcp/contract/RelationshipCreateTest.java`
- [x] T008 [P] Contract test relationship get in `src/test/java/com/monicahq/mcp/contract/RelationshipGetTest.java`
- [x] T009 [P] Contract test relationship update in `src/test/java/com/monicahq/mcp/contract/RelationshipUpdateTest.java`
- [x] T010 [P] Contract test relationship delete in `src/test/java/com/monicahq/mcp/contract/RelationshipDeleteTest.java`
- [x] T011 [P] Contract test relationship list in `src/test/java/com/monicahq/mcp/contract/RelationshipListTest.java`

### Company Tests  
- [x] T012 [P] Contract test company create in `src/test/java/com/monicahq/mcp/contract/CompanyCreateTest.java`
- [x] T013 [P] Contract test company get in `src/test/java/com/monicahq/mcp/contract/CompanyGetTest.java`
- [x] T014 [P] Contract test company update in `src/test/java/com/monicahq/mcp/contract/CompanyUpdateTest.java`
- [x] T015 [P] Contract test company delete in `src/test/java/com/monicahq/mcp/contract/CompanyDeleteTest.java`
- [x] T016 [P] Contract test company list in `src/test/java/com/monicahq/mcp/contract/CompanyListTest.java`

### Relationship Type Tests
- [x] T017 [P] Contract test relationship type get in `src/test/java/com/monicahq/mcp/contract/RelationshipTypeGetTest.java`
- [x] T018 [P] Contract test relationship type list in `src/test/java/com/monicahq/mcp/contract/RelationshipTypeListTest.java`
- [x] T019 [P] Contract test relationship type group list in `src/test/java/com/monicahq/mcp/contract/RelationshipTypeGroupListTest.java`

### Integration Tests
- [x] T020 [P] Integration test relationship management workflow in `src/test/java/com/monicahq/mcp/integration/RelationshipManagementTest.java`
- [x] T021 [P] Integration test company association workflow in `src/test/java/com/monicahq/mcp/integration/CompanyManagementTest.java`

## Phase 3.3: Core Implementation (ONLY after tests are failing)

### Monica API Client Methods
- [ ] T022 [P] Relationship API methods in `src/main/java/com/monicahq/mcp/client/MonicaHqClient.java` (relationships endpoints)
- [ ] T023 [P] Company API methods in `src/main/java/com/monicahq/mcp/client/MonicaHqClient.java` (companies endpoints)  
- [ ] T024 [P] Relationship type API methods in `src/main/java/com/monicahq/mcp/client/MonicaHqClient.java` (relationship types endpoints)

### Service Layer
- [ ] T025 [P] RelationshipService with CRUD operations in `src/main/java/com/monicahq/mcp/service/RelationshipService.java`
- [ ] T026 [P] CompanyService with CRUD operations in `src/main/java/com/monicahq/mcp/service/CompanyService.java`
- [ ] T027 [P] RelationshipTypeService with read operations in `src/main/java/com/monicahq/mcp/service/RelationshipTypeService.java`
- [ ] T028 [P] RelationshipTypeGroupService with read operations in `src/main/java/com/monicahq/mcp/service/RelationshipTypeGroupService.java`

### MCP Tool Registration
- [ ] T029 Register relationship tools in `src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java` (relationship operations)
- [ ] T030 Register company tools in `src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java` (company operations)
- [ ] T031 Register relationship type discovery tools in `src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java` (type operations)

### MCP Message Handler
- [ ] T032 Add relationship tool handlers in `src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java`
- [ ] T033 Add company tool handlers in `src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java`
- [ ] T034 Add relationship type tool handlers in `src/main/java/com/monicahq/mcp/controller/McpMessageHandler.java`

## Phase 3.4: Integration & Validation
- [ ] T035 Update tool count verification in `src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java` (from 54 to 68 tools)
- [ ] T036 [P] Create relationship CRUD validation script in `validation/crud/validate-relationship-crud.sh`
- [ ] T037 [P] Create company CRUD validation script in `validation/crud/validate-company-crud.sh`
- [ ] T038 [P] Create relationship type discovery validation in `validation/crud/validate-relationship-type-discovery.sh`
- [ ] T039 Update comprehensive validation runner in `validation/run-all-crud-tests.sh`

## Phase 3.5: Polish & Documentation
- [ ] T040 [P] Update API coverage documentation in `validation/docs/COMPREHENSIVE-CRUD-VALIDATION-SUMMARY.md`
- [ ] T041 [P] Update constitutional compliance report in `validation/docs/CONSTITUTIONAL-PRINCIPLE-VII-CERTIFICATE.md`
- [ ] T042 [P] Update CLAUDE.md with new entity types and operation counts
- [ ] T043 [P] Update README.md with relationship management capabilities
- [ ] T044 Run full test suite verification with `./gradlew test`
- [ ] T045 Run complete validation suite with `validation/run-all-crud-tests.sh`

## Dependencies

### Critical Path
- Models (T001-T006) before services (T025-T028)
- Tests (T007-T021) before ANY implementation (T022+)
- API client methods (T022-T024) before services (T025-T028)
- Services (T025-T028) before tool registration (T029-T031)
- Tool registration (T029-T031) before handlers (T032-T034)
- Core implementation before validation (T035-T039)
- Everything before documentation (T040-T045)

### Blocking Dependencies
- T025 blocks T029, T032
- T026 blocks T030, T033  
- T027, T028 block T031, T034
- T029-T034 block T035
- T036-T039 require corresponding services completed
- T044, T045 require all implementation completed

## Parallel Example
```bash
# Launch DTO creation together (T001-T004):
Task: "Create Relationship DTO in src/main/java/com/monicahq/mcp/dto/Relationship.java"
Task: "Create Company DTO in src/main/java/com/monicahq/mcp/dto/Company.java"
Task: "Create RelationshipType DTO in src/main/java/com/monicahq/mcp/dto/RelationshipType.java"
Task: "Create RelationshipTypeGroup DTO in src/main/java/com/monicahq/mcp/dto/RelationshipTypeGroup.java"

# Launch all contract tests together (T007-T019):
Task: "Contract test relationship create in src/test/java/com/monicahq/mcp/contract/RelationshipCreateTest.java"
Task: "Contract test company create in src/test/java/com/monicahq/mcp/contract/CompanyCreateTest.java"
# ... (all contract tests can run in parallel)

# Launch service implementations together (T025-T028):
Task: "RelationshipService with CRUD operations in src/main/java/com/monicahq/mcp/service/RelationshipService.java"
Task: "CompanyService with CRUD operations in src/main/java/com/monicahq/mcp/service/CompanyService.java"
Task: "RelationshipTypeService with read operations in src/main/java/com/monicahq/mcp/service/RelationshipTypeService.java"
Task: "RelationshipTypeGroupService with read operations in src/main/java/com/monicahq/mcp/service/RelationshipTypeGroupService.java"
```

## Expected Outcomes

### New MCP Operations (14 total)
**Relationship Management (5 operations)**:
- relationship_create, relationship_get, relationship_update, relationship_delete, relationship_list

**Company Management (5 operations)**:  
- company_create, company_get, company_update, company_delete, company_list

**Discovery Tools (4 operations)**:
- relationship_type_list, relationship_type_get, relationship_type_group_list, relationship_type_group_get

### Validation Coverage
- 3 new CRUD validation scripts
- Integration with existing validation framework
- Updated documentation reflecting 68 total MCP operations
- Enhanced API coverage (from 30% to 50%)

## Notes
- [P] tasks = different files, no dependencies
- Verify tests fail before implementing
- Commit after each task
- Follow existing Spring Boot patterns for consistency
- Maintain constitutional governance compliance
- Use established Monica API client patterns

## Validation Checklist
*GATE: Checked before task execution*

- [x] All entities have corresponding DTOs and mappers  
- [x] All contract tests come before implementation
- [x] All services follow established patterns
- [x] All MCP tools registered in tool registry
- [x] All handlers added to message handler
- [x] Parallel tasks truly independent (different files)
- [x] Each task specifies exact file path
- [x] Dependencies clearly mapped
- [x] Validation framework integration planned
- [x] Documentation updates included