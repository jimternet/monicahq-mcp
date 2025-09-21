# Implementation Tasks: Phase 4.1 - Core Missing Entities

## 🎯 Overview
Implement 4 critical Monica API entities adding 18 MCP operations to advance from 70% to 85% API coverage. Focus on essential contact management workflows with TDD methodology.

## 📅 Timeline: 5 Days | 🎯 Target: 18 Operations | 📊 Impact: 70% → 85% API Coverage

---

## 🅰️ Activity Type Management (Days 1-2: 8 Operations)

### Entity 1: Activity Types (4 operations)

#### T001: Activity Type DTO Creation
- [ ] **Create** `src/main/java/com/monicahq/mcp/dto/ActivityType.java`
- [ ] **Fields**: `id`, `name`, `category_id`, `description`, `icon`, `created_at`, `updated_at`
- [ ] **Validation**: `@NotBlank` for name, `@Size` constraints
- [ ] **JSON Mapping**: `@JsonProperty` for snake_case API compatibility
- [ ] **Lombok**: `@Data`, `@Builder`, `@NoArgsConstructor`, `@AllArgsConstructor`

#### T002: Activity Type Mapper  
- [ ] **Create** `src/main/java/com/monicahq/mcp/mapper/ActivityTypeMapper.java`
- [ ] **Methods**: `toDto()`, `fromDto()`, `updateFromDto()`
- [ ] **Field Mapping**: Handle `category_id` ↔ `categoryId` conversion
- [ ] **Null Safety**: Proper null handling for optional fields

#### T003: Activity Type Service Implementation
- [ ] **Create** `src/main/java/com/monicahq/mcp/service/ActivityTypeService.java`
- [ ] **CRUD Operations**: 
  - `createActivityType(Map<String, Object> arguments)`
  - `getActivityType(Map<String, Object> arguments)`
  - `updateActivityType(Map<String, Object> arguments)`
  - `deleteActivityType(Map<String, Object> arguments)`
  - `listActivityTypes(Map<String, Object> arguments)`
- [ ] **MonicaHqClient Integration**: Use `/activitytypes` endpoints
- [ ] **ContentFormatter**: Raw API data formatting for Constitutional Principle VI
- [ ] **Error Handling**: Comprehensive exception handling with meaningful messages

#### T004: Activity Type MCP Tool Registration
- [ ] **Update** `src/main/java/com/monicahq/mcp/controller/McpToolRegistry.java`
- [ ] **Register Tools**:
  - `activity_type_create`: "[Activity Type] Create a new activity type"
  - `activity_type_get`: "[Activity Type] Get activity type by ID" 
  - `activity_type_update`: "[Activity Type] Update existing activity type"
  - `activity_type_delete`: "[Activity Type] Delete activity type"
  - `activity_type_list`: "[Activity Type] List activity types with pagination"
- [ ] **Schema Definitions**: Proper input schemas for each operation
- [ ] **Tool Categorization**: Add to "Activity Management" category

### Entity 2: Activity Type Categories (4 operations)

#### T005: Activity Type Category DTO Creation
- [ ] **Create** `src/main/java/com/monicahq/mcp/dto/ActivityTypeCategory.java`
- [ ] **Fields**: `id`, `name`, `parent_id`, `description`, `sort_order`, `created_at`, `updated_at`
- [ ] **Hierarchical Support**: Optional `parent_id` for nested categories
- [ ] **Validation**: `@NotBlank` for name, hierarchy validation
- [ ] **JSON Mapping**: Snake_case API compatibility

#### T006: Activity Type Category Mapper
- [ ] **Create** `src/main/java/com/monicahq/mcp/mapper/ActivityTypeCategoryMapper.java`
- [ ] **Hierarchy Handling**: Proper parent-child relationship mapping
- [ ] **Sort Order**: Maintain category ordering logic

#### T007: Activity Type Category Service Implementation  
- [ ] **Create** `src/main/java/com/monicahq/mcp/service/ActivityTypeCategoryService.java`
- [ ] **CRUD Operations**: Complete lifecycle management
- [ ] **Hierarchy Logic**: Handle parent-child relationships
- [ ] **MonicaHqClient Integration**: Use `/activitytypecategories` endpoints

#### T008: Activity Type Category MCP Tool Registration
- [ ] **Update** `McpToolRegistry.java` with 4 new category operations
- [ ] **Hierarchical Schemas**: Support parent_id in schemas
- [ ] **Category Integration**: Link with ActivityType operations

---

## 🏠 Address Management (Day 3: 5 Operations)

### Entity 3: Addresses (5 operations)

#### T009: Address DTO Creation
- [ ] **Create** `src/main/java/com/monicahq/mcp/dto/Address.java`
- [ ] **Fields**: `id`, `contact_id`, `name`, `street`, `city`, `province`, `postal_code`, `country_id`, `latitude`, `longitude`, `created_at`, `updated_at`
- [ ] **Validation**: Required `contact_id`, optional geographic fields
- [ ] **Geographic Support**: Lat/lng for location services
- [ ] **International Support**: Country reference integration

#### T010: Address Mapper
- [ ] **Create** `src/main/java/com/monicahq/mcp/mapper/AddressMapper.java`
- [ ] **Contact Integration**: Handle contact_id relationships
- [ ] **Geographic Data**: Coordinate handling and validation

#### T011: Address Service Implementation
- [ ] **Create** `src/main/java/com/monicahq/mcp/service/AddressService.java`
- [ ] **Contact Relationship**: Ensure valid contact_id references
- [ ] **Geographic Features**: Coordinate validation and processing
- [ ] **MonicaHqClient Integration**: Use `/addresses` endpoints

#### T012: Address MCP Tool Registration
- [ ] **Update** `McpToolRegistry.java` with 5 address operations
- [ ] **Contact Integration**: Schema requires contact_id
- [ ] **Geographic Schemas**: Support for coordinates and country data

#### T013: Address Integration Testing
- [ ] **Create** `src/test/java/com/monicahq/mcp/integration/AddressContactFlowTest.java`
- [ ] **Workflow**: Create contact → Add address → Update address → List contact addresses
- [ ] **Geographic Testing**: Coordinate validation workflows
- [ ] **International Testing**: Country integration workflows

---

## 👥 Group Management (Day 4: 5 Operations)

### Entity 4: Groups (5 operations)

#### T014: Group DTO Creation  
- [ ] **Create** `src/main/java/com/monicahq/mcp/dto/Group.java`
- [ ] **Fields**: `id`, `name`, `description`, `contact_count`, `created_at`, `updated_at`
- [ ] **Calculated Fields**: `contact_count` as read-only computed field
- [ ] **Validation**: Required name, reasonable length limits

#### T015: Group Mapper
- [ ] **Create** `src/main/java/com/monicahq/mcp/mapper/GroupMapper.java`
- [ ] **Contact Count**: Handle calculated field properly
- [ ] **Membership**: Future support for contact relationships

#### T016: Group Service Implementation
- [ ] **Create** `src/main/java/com/monicahq/mcp/service/GroupService.java`
- [ ] **Contact Counting**: Calculate contact_count dynamically
- [ ] **Membership Management**: Foundation for contact-group relationships
- [ ] **MonicaHqClient Integration**: Use `/groups` endpoints

#### T017: Group MCP Tool Registration
- [ ] **Update** `McpToolRegistry.java` with 5 group operations
- [ ] **Contact Integration**: Prepare schemas for future membership operations
- [ ] **Categorization**: Add to "Contact Organization" category

#### T018: Group Integration Testing
- [ ] **Create** `src/test/java/com/monicahq/mcp/integration/GroupManagementTest.java`
- [ ] **Workflow**: Create group → Update description → List groups with counts
- [ ] **Contact Integration**: Foundation for membership testing

---

## 💼 Occupation Management (Day 5: 4 Operations)

### Entity 5: Occupations (4 operations)

#### T019: Occupation DTO Creation
- [ ] **Create** `src/main/java/com/monicahq/mcp/dto/Occupation.java`  
- [ ] **Fields**: `id`, `contact_id`, `title`, `company`, `description`, `salary`, `start_date`, `end_date`, `created_at`, `updated_at`
- [ ] **Professional Focus**: Career and employment information
- [ ] **Timeline Support**: Start/end dates for career history
- [ ] **Privacy**: Optional salary information

#### T020: Occupation Mapper
- [ ] **Create** `src/main/java/com/monicahq/mcp/mapper/OccupationMapper.java`
- [ ] **Contact Integration**: Handle contact_id relationships
- [ ] **Timeline Logic**: Date range validation and handling

#### T021: Occupation Service Implementation  
- [ ] **Create** `src/main/java/com/monicahq/mcp/service/OccupationService.java`
- [ ] **Contact Relationship**: Ensure valid contact_id references
- [ ] **Career Timeline**: Support multiple occupations per contact
- [ ] **MonicaHqClient Integration**: Use `/occupations` endpoints

#### T022: Occupation MCP Tool Registration
- [ ] **Update** `McpToolRegistry.java` with 4 occupation operations
- [ ] **Professional Schemas**: Support career information fields
- [ ] **Contact Integration**: Require contact_id in schemas

---

## 🧪 Testing & Validation (Day 5: Integration)

### T023: Contract Test Implementation
- [ ] **Create** 20 contract tests (5 per entity):
  - `ActivityTypeCreateTest.java`, `ActivityTypeGetTest.java`, etc.
  - `ActivityTypeCategoryCreateTest.java`, etc.
  - `AddressCreateTest.java`, `AddressGetTest.java`, etc.
  - `GroupCreateTest.java`, `GroupGetTest.java`, etc.
  - `OccupationCreateTest.java`, `OccupationGetTest.java`, etc.
- [ ] **TDD Methodology**: All tests fail initially, pass after implementation
- [ ] **MCP Protocol**: JSON-RPC 2.0 validation for all operations
- [ ] **Error Handling**: Comprehensive error scenario testing

### T024: TestMonicaHqClient Updates
- [ ] **Update** `src/test/java/com/monicahq/mcp/config/TestMonicaHqClient.java`
- [ ] **Add Stubs** for all new endpoints:
  - `/activitytypes` CRUD operations
  - `/activitytypecategories` CRUD operations  
  - `/addresses` CRUD operations
  - `/groups` CRUD operations
  - `/occupations` CRUD operations
- [ ] **Response Stubs**: Realistic test data for each entity type
- [ ] **Error Scenarios**: Proper error simulation for edge cases

### T025: Integration Workflow Testing
- [ ] **Create** `src/test/java/com/monicahq/mcp/integration/Phase41WorkflowTest.java`
- [ ] **End-to-End Workflow**: 
  1. Create contact
  2. Add address with geographic data
  3. Assign to group  
  4. Add occupation/professional info
  5. Create categorized activity
- [ ] **Cross-Entity Validation**: Ensure proper relationships between entities
- [ ] **Performance Testing**: Validate response times under load

### T026: MCP Tool Registry Validation  
- [ ] **Update** tool count in `McpToolRegistry.java`: 68 → 86 operations
- [ ] **Category Organization**: Proper categorization of all new tools
- [ ] **Schema Validation**: Ensure all schemas are properly defined
- [ ] **Integration Test**: Update `McpConnectionTest.java` for new tool count

### T027: Documentation Updates
- [ ] **Update** `CLAUDE.md`:
  - Operations: 68 → 86
  - Entity types: 16 → 20  
  - API coverage: 70% → 85%
  - Test count: 173 → 195+
- [ ] **Constitutional Compliance**: Verify all 6 principles maintained
- [ ] **Performance Metrics**: Document response time benchmarks

---

## 🎯 Success Criteria

### Quantitative Validation
- [ ] **Operations**: Exactly 18 new MCP operations implemented
- [ ] **Entities**: 4 new entity types with complete CRUD
- [ ] **Tests**: 22+ new tests (20 contract + 2 integration) 
- [ ] **Coverage**: 195+ total tests with 100% pass rate
- [ ] **Performance**: <500ms average response time for all operations

### Qualitative Validation  
- [ ] **Constitutional Compliance**: All 6 principles verified
- [ ] **User Experience**: Complete contact management workflows enabled
- [ ] **Code Quality**: Follows established patterns and conventions
- [ ] **Security**: Proper authentication and authorization
- [ ] **Maintainability**: Clear, self-documenting code structure

### Integration Validation
- [ ] **Build Success**: `./gradlew build` completes without errors
- [ ] **Test Suite**: `./gradlew test` shows 100% pass rate
- [ ] **Tool Registry**: All 86 operations properly registered
- [ ] **Claude Desktop**: MCP protocol compliance verified

## 🚀 Phase 4.1 Completion Impact

Upon successful completion:
- **Enhanced Contact Management**: Complete lifecycle with addresses, groups, professional info
- **Activity Organization**: Proper categorization and type management
- **Foundation for Phase 4.2**: Solid base for financial and content management features
- **User Satisfaction**: No critical workflow gaps in contact management
- **Quality Maintained**: Constitutional compliance and test coverage preserved

**Next Phase**: Financial & Content Management (Debts, Documents, Photos, Gifts)