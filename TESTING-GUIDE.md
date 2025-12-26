# MonicaHQ MCP Server Testing Guide

**Constitutional Governance Framework v1.1.0**

This guide provides comprehensive testing instructions for the MonicaHQ MCP Server, following our constitutional principles and quality assurance standards.

## Quick Reference

```bash
# Constitutional compliance validation
./validate-constitution.sh

# Complete 7-phase testing suite
./test-mcp-complete.sh

# Claude Desktop integration testing
./test-claude-desktop.sh

# Individual principle testing
./tests/constitutional/test_mcp_protocol_first.sh
./tests/constitutional/test_tdd_compliance.sh
./tests/constitutional/test_spring_architecture.sh
./tests/constitutional/test_production_ready.sh
./tests/constitutional/test_type_safety.sh

# STDOUT contamination detection
./tests/integration/test_stdout_cleanliness.sh
```

## Testing Framework Overview

### Constitutional Governance Testing

Our testing framework validates adherence to **5 Core Constitutional Principles**:

1. **MCP Protocol First** - JSON-RPC 2.0 over STDIO compliance
2. **Test-Driven Development** - 100% test coverage (136/136 tests)
3. **Spring Boot Architecture Excellence** - WebFlux for external APIs
4. **Production-Ready Deployment** - Docker + Claude Desktop ready
5. **Type Safety & Code Generation** - MapStruct + Lombok patterns

### 7-Phase Testing Methodology

The comprehensive testing suite follows a systematic 7-phase approach:

1. **Constitutional Compliance** - Validate all 5 core principles
2. **Unit and Integration Tests** - Run full test suite (136 tests)
3. **MCP Protocol Validation** - JSON-RPC 2.0 over STDIO testing
4. **Architecture Validation** - Spring Boot and reactive patterns
5. **Security and Quality** - Type safety, OAuth2, no hardcoded secrets
6. **Deployment Readiness** - Docker, JAR execution, health checks
7. **Tool Accessibility** - Helper scripts, documentation, Claude Desktop config

## Detailed Testing Instructions

### 1. Constitutional Compliance Validation

The primary governance validation ensures adherence to all constitutional principles:

```bash
./validate-constitution.sh
```

**Expected Output:**
```
🏛️  MonicaHQ MCP Server Constitutional Compliance Validator
Constitution Version: 1.1.0
========================================================

Principle I: MCP Protocol First
===============================
✅ PASS: JSON-RPC 2.0 protocol implementation found
✅ PASS: Tool categorization implemented
✅ PASS: STDIO mode for Claude Desktop implemented
✅ PASS: STDIO logging properly configured (STDERR only)
✅ PASS: No STDOUT contamination detected

[... additional principles ...]

🎉 FULLY COMPLIANT - No violations or warnings
```

### 2. Individual Constitutional Principle Testing

Test each principle independently for detailed validation:

#### MCP Protocol First Principle
```bash
./tests/constitutional/test_mcp_protocol_first.sh
```
Validates:
- JSON-RPC 2.0 specification compliance
- MCP tool categorization and discoverability
- STDOUT cleanliness for Claude Desktop integration
- Protocol error handling
- Compliance validation scripts

#### Test-Driven Development Principle
```bash
./tests/constitutional/test_tdd_compliance.sh
```
Validates:
- 100% test coverage requirement (136+ tests)
- RED-GREEN-Refactor cycle evidence
- Test hierarchy (Contract→Integration→E2E→Unit)
- Quality gate enforcement
- JUnit test structure

#### Spring Boot Architecture Excellence
```bash
./tests/constitutional/test_spring_architecture.sh
```
Validates:
- Spring Boot 3.x version compliance
- WebFlux reactive patterns for I/O operations
- Dual-mode architecture (STDIO + Web Server)
- Dependency injection and component scanning
- Circuit breaker patterns with Resilience4j
- Proper component structure

#### Production-Ready Deployment
```bash
./tests/constitutional/test_production_ready.sh
```
Validates:
- Docker containerization support
- Environment variable configuration
- Health checks and monitoring
- Production logging configuration
- Claude Desktop integration readiness
- Security best practices

#### Type Safety and Code Generation
```bash
./tests/constitutional/test_type_safety.sh
```
Validates:
- MapStruct type-safe mapping implementation
- Lombok code generation
- DTO structure and type safety
- Strong typing in API interactions
- Bean validation annotations
- Generic type usage (no raw types)

### 3. STDOUT Contamination Detection

Critical for MCP protocol compliance:

```bash
./tests/integration/test_stdout_cleanliness.sh
```

This test validates:
- STDIO logging configuration prevents STDOUT contamination
- McpStdioServer STDOUT cleanliness
- Spring Boot banner suppression
- JSON-RPC response format validation
- Error response cleanliness

**Why This Matters:**
STDOUT contamination breaks Claude Desktop integration. Only JSON-RPC responses should appear on STDOUT in STDIO mode.

### 4. Complete Integration Testing

Run the full 7-phase testing suite:

```bash
./test-mcp-complete.sh
```

**Prerequisites:**
```bash
# Set environment variables for complete testing
export MONICA_API_URL=https://your-monica-instance.com/api
export MONICA_API_TOKEN=your-oauth2-bearer-token
```

**Expected Output:**
```
🧪 MonicaHQ MCP Server - Comprehensive Testing Suite
=====================================================

Phase 1: Constitutional Compliance
==================================
✅ PASSED: Constitutional compliance validation

Phase 2: Unit and Integration Tests
===================================
✅ PASSED: Gradle test suite (136 tests)
✅ PASSED: Test count validation (≥50 test files)

[... 7 phases ...]

🎉 ALL TESTS PASSED (XX/XX)
The MonicaHQ MCP Server is ready for production deployment
```

### 5. Claude Desktop Integration Testing

Validate Claude Desktop compatibility:

```bash
./test-claude-desktop.sh
```

This comprehensive test validates:
- Constitutional compliance
- Configuration file validation
- MCP server connectivity
- Claude Desktop compatibility
- Real-world integration simulation

**Cross-Platform Support:**
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Linux**: `~/.config/claude-desktop/claude_desktop_config.json`

## Test Failure Troubleshooting

### Constitutional Violations

If constitutional validation fails:

1. **Review specific violation messages**
2. **Address architectural issues** (e.g., missing WebFlux, hardcoded secrets)
3. **Update code following constitutional principles**
4. **Re-run validation**: `./validate-constitution.sh`

### Test Coverage Issues

If test coverage drops below 100%:

1. **Identify missing test coverage**: `./gradlew test jacocoTestReport`
2. **Write tests following TDD methodology** (RED-GREEN-Refactor)
3. **Ensure tests are properly categorized** (contract/integration/unit)
4. **Verify test count**: Should be 136+ tests

### STDOUT Contamination

If STDOUT contamination is detected:

1. **Check logging configuration**: `src/main/resources/logback-stdio.xml`
2. **Review McpStdioServer.java** for System.out usage
3. **Ensure Spring Boot banner is disabled**
4. **Verify all output goes to STDERR** except JSON-RPC responses

### Claude Desktop Integration Issues

If Claude Desktop integration fails:

1. **Validate configuration file** with `jq . claude_desktop_config.json`
2. **Check JAR file path** is absolute and exists
3. **Verify environment variables** are properly set
4. **Test STDIO mode manually**: `java -jar build/libs/monicahqmcp-0.1.0.jar --stdio`
5. **Restart Claude Desktop** after configuration changes

## Continuous Integration

### Pre-commit Validation

Before committing code:

```bash
# Quick validation (recommended)
./validate-constitution.sh && ./gradlew test

# Complete validation (comprehensive)
./test-mcp-complete.sh
```

### Pull Request Requirements

All pull requests must pass:

- ✅ Constitutional compliance validation
- ✅ All 136+ tests passing (100% coverage)
- ✅ STDOUT cleanliness verification
- ✅ Claude Desktop integration testing

### Quality Gates

The constitutional framework enforces these quality gates:

1. **Zero constitutional violations** before merge
2. **100% test coverage maintained** (136+ tests)
3. **MCP protocol compliance** verified
4. **Production readiness** validated

## Service Unit Testing Patterns

This section documents the Mockito-based unit testing patterns used for service layer testing. These tests run without requiring a live MonicaHQ instance, providing fast feedback during development.

### Overview

The service layer tests use **Mockito** to mock the `MonicaHqClient` and `ContentFormatter` dependencies. This allows testing service logic in isolation, including:

- Input validation
- Field name mapping (camelCase ↔ snake_case)
- ID type parsing (Long, Integer, String)
- Response formatting
- Pagination handling

### ServiceTestBase

All service tests extend `ServiceTestBase`, which provides reusable mocking patterns and test data builders.

**Location:** `src/test/java/com/monicahq/mcp/service/ServiceTestBase.java`

#### Key Features

1. **Mock Response Builders** - Create standardized API response structures
2. **MonicaHqClient Mock Helpers** - Set up GET/POST/PUT/DELETE mock behaviors
3. **ContentFormatter Mock Helpers** - Mock JSON formatting
4. **Entity Data Builders** - Fluent builders for test data
5. **Argument Matchers** - Custom matchers for request verification

### Basic Test Structure

```java
package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

@ExtendWith(MockitoExtension.class)
class MyServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private MyService myService;

    private Map<String, Object> mockEntityData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        // Build mock entity data using builders from ServiceTestBase
        mockEntityData = contactBuilder()
            .id(1L)
            .firstName("John")
            .lastName("Doe")
            .build();

        // Wrap in standard API response format
        mockApiResponse = createSingleEntityResponse(mockEntityData);
    }

    @Test
    void createEntity_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");
        arguments.put("lastName", "Doe");
        arguments.put("genderId", 1);

        when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = myService.createContact(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        // Verify API was called with correct field mapping
        verify(monicaClient).post(eq("/contacts"), argThat(data ->
            "John".equals(data.get("first_name")) &&  // camelCase → snake_case
            "Doe".equals(data.get("last_name"))
        ));
    }
}
```

### Mocking MonicaHqClient

#### GET Requests

```java
// Simple GET mock
mockGet(monicaClient, "/contacts/123", mockApiResponse);

// GET with query parameter matching
mockGetWithParams(monicaClient, "/contacts",
    params -> "1".equals(params.get("page")) && "10".equals(params.get("limit")),
    mockListResponse);

// GET with endpoint pattern matching
mockGetWithPattern(monicaClient, "/contacts/\\d+", mockApiResponse);
```

#### POST Requests

```java
// Simple POST mock
mockPost(monicaClient, "/contacts", mockApiResponse);

// POST with request body verification
mockPostWithBody(monicaClient, "/contacts",
    body -> "John".equals(body.get("first_name")),
    mockApiResponse);
```

#### PUT Requests

```java
// Simple PUT mock
mockPut(monicaClient, "/contacts/123", mockApiResponse);

// PUT with request body verification
mockPutWithBody(monicaClient, "/contacts/123",
    body -> !body.containsKey("id"),  // Verify ID removed from body
    mockApiResponse);
```

#### DELETE Requests

```java
mockDelete(monicaClient, "/contacts/123", createDeleteResponse(123L));
```

### Response Builders

#### Single Entity Response

```java
// Wraps entity in {"data": entityData} format
Map<String, Object> response = createSingleEntityResponse(entityData);
```

#### List Response with Pagination

```java
List<Map<String, Object>> entities = List.of(entity1, entity2, entity3);

// Default pagination (page 1, 10 per page)
Map<String, Object> response = createListResponse(entities);

// Custom pagination
Map<String, Object> response = createListResponse(entities, page, perPage, total);
```

#### Delete Response

```java
Map<String, Object> response = createDeleteResponse(123L);
// Returns: {"deleted": true, "id": 123}
```

### Entity Data Builders

ServiceTestBase provides fluent builders for all entity types. Each builder initializes with sensible defaults.

#### Contact Builder

```java
Map<String, Object> contact = contactBuilder()
    .id(1L)
    .firstName("John")
    .lastName("Doe")
    .genderId(1)
    .email("john@example.com")
    .phone("+1-555-1234")
    .nickname("Johnny")
    .company("Acme Inc")
    .jobTitle("Developer")
    .birthdate("1990-05-15")
    .build();
```

#### Activity Builder

```java
Map<String, Object> activity = activityBuilder()
    .id(1L)
    .summary("Team meeting")
    .description("Weekly standup")
    .activityTypeId(1)
    .happenedAt("2024-01-15")
    .attendees(List.of(1L, 2L))
    .build();
```

#### Other Available Builders

- `taskBuilder()` - Tasks with title, contactId, completed status
- `noteBuilder()` - Notes with body, contactId, favorited status
- `tagBuilder()` - Tags with name, nameSlug, contactCount
- `callBuilder()` - Calls with content, contactId, calledAt
- `reminderBuilder()` - Reminders with title, initialDate, frequencyType
- `relationshipBuilder()` - Relationships with contactIs, ofContact, typeId
- `companyBuilder()` - Companies with name, website, numberOfEmployees
- `giftBuilder()` - Gifts with name, status, value, recipient
- `debtBuilder()` - Debts with amount, currency, inDebt status
- `occupationBuilder()` - Occupations with title, companyId, dates
- `conversationBuilder()` - Conversations with happenedAt, messages
- `journalEntryBuilder()` - Journal entries with title, date, content
- `documentBuilder()` - Documents with filename, mimeType, size
- `photoBuilder()` - Photos with filename, mimeType, dimensions
- `addressBuilder()` - Addresses with street, city, coordinates
- `contactFieldBuilder()` - Custom fields with data, typeId
- `groupBuilder()` - Groups with name, description
- `userBuilder()` - Users with name, email, admin status
- `activityTypeBuilder()` - Activity types with name, categoryId
- `activityTypeCategoryBuilder()` - Activity type categories

### Argument Matchers

ServiceTestBase provides custom argument matchers for request verification:

```java
// Check map contains specific key-value pair
verify(monicaClient).post(eq("/contacts"), argThat(hasEntry("first_name", "John")));

// Check map contains specific key
verify(monicaClient).post(eq("/contacts"), argThat(hasKey("first_name")));

// Check map contains multiple keys
verify(monicaClient).post(eq("/contacts"), argThat(hasKeys("first_name", "last_name", "gender_id")));

// Check string map entry
mockGetWithParams(monicaClient, "/contacts", hasStringEntry("page", "1"), response);

// Custom predicate matching
verify(monicaClient).post(eq("/contacts"), argThat(matching(data ->
    data.containsKey("first_name") && !data.containsKey("id")
)));
```

### Common Test Patterns

#### Testing Validation Errors

```java
@Test
void createEntity_MissingRequiredField_ThrowsException() {
    Map<String, Object> arguments = new HashMap<>();
    // Missing required 'firstName' field

    IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
        myService.createContact(arguments).block();
    });

    assertEquals("firstName is required", exception.getMessage());
    verifyNoInteractions(monicaClient);  // API should not be called
}
```

#### Testing Field Name Mapping

```java
@Test
void createEntity_MapsFieldNames_ToSnakeCase() {
    Map<String, Object> arguments = Map.of(
        "firstName", "John",
        "lastName", "Doe",
        "genderId", 1
    );

    when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));
    when(contentFormatter.formatAsEscapedJson(any())).thenReturn("JSON");

    myService.createContact(arguments).block();

    verify(monicaClient).post(eq("/contacts"), argThat(data ->
        data.containsKey("first_name") &&   // camelCase → snake_case
        data.containsKey("last_name") &&
        data.containsKey("gender_id") &&
        !data.containsKey("firstName")      // Original key should not be present
    ));
}
```

#### Testing ID Type Parsing

```java
@Test
void getEntity_StringId_ParsesCorrectly() {
    Map<String, Object> arguments = Map.of("id", "123");  // String ID

    when(monicaClient.get(eq("/contacts/123"), any())).thenReturn(Mono.just(mockApiResponse));
    when(contentFormatter.formatAsEscapedJson(any())).thenReturn("JSON");

    Map<String, Object> result = myService.getContact(arguments).block();

    assertNotNull(result);
    verify(monicaClient).get(eq("/contacts/123"), any());
}

@Test
void getEntity_IntegerId_ParsesCorrectly() {
    Map<String, Object> arguments = Map.of("id", 123);  // Integer ID

    when(monicaClient.get(eq("/contacts/123"), any())).thenReturn(Mono.just(mockApiResponse));
    when(contentFormatter.formatAsEscapedJson(any())).thenReturn("JSON");

    myService.getContact(arguments).block();

    verify(monicaClient).get(eq("/contacts/123"), any());
}
```

#### Testing Pagination and Limit Clamping

```java
@Test
void listEntities_ExceedsMaxLimit_ClampedTo100() {
    Map<String, Object> arguments = Map.of("limit", 500);  // Exceeds max

    when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockListResponse));
    when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("JSON");

    myService.listContacts(arguments).block();

    verify(monicaClient).get(eq("/contacts"), argThat(params ->
        "100".equals(params.get("limit"))  // Clamped to 100
    ));
}

@Test
void listEntities_BelowMinLimit_ClampedTo1() {
    Map<String, Object> arguments = Map.of("limit", 0);

    when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockListResponse));
    when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("JSON");

    myService.listContacts(arguments).block();

    verify(monicaClient).get(eq("/contacts"), argThat(params ->
        "1".equals(params.get("limit"))
    ));
}
```

#### Testing Update Operations (ID Removal)

```java
@Test
void updateEntity_RemovesIdFromBody() {
    Map<String, Object> arguments = Map.of(
        "id", 123L,
        "firstName", "Jane"
    );

    when(monicaClient.put(eq("/contacts/123"), any())).thenReturn(Mono.just(mockApiResponse));
    when(contentFormatter.formatAsEscapedJson(any())).thenReturn("JSON");

    myService.updateContact(arguments).block();

    verify(monicaClient).put(eq("/contacts/123"), argThat(data ->
        !data.containsKey("id") &&     // ID removed from body
        "Jane".equals(data.get("first_name"))
    ));
}
```

### Running Unit Tests

```bash
# Run all tests
./gradlew test

# Run service tests only
./gradlew test --tests 'com.monicahq.mcp.service.*Test'

# Run specific test class
./gradlew test --tests 'com.monicahq.mcp.service.ContactServiceTest'

# Run specific test method
./gradlew test --tests 'com.monicahq.mcp.service.ContactServiceTest.createContact_ValidArgs_ReturnsFormattedResponse'
```

### Test Coverage

The unit test suite covers:

| Service | Test Class | Test Count |
|---------|------------|------------|
| ActivityService | ActivityServiceTest | 38 |
| TaskService | TaskServiceTest | 38 |
| NoteService | NoteServiceTest | 38 |
| TagService | TagServiceTest | 48 |
| CallService | CallServiceTest | 52 |
| ReminderService | ReminderServiceTest | 52 |
| RelationshipService | RelationshipServiceTest | 52 |
| CompanyService | CompanyServiceTest | 48 |
| GiftService | GiftServiceTest | 52 |
| DebtService | DebtServiceTest | 60 |
| OccupationService | OccupationServiceTest | 70 |
| ConversationService | ConversationServiceTest | 52 |
| JournalEntryService | JournalEntryServiceTest | 60 |
| DocumentService | DocumentServiceTest | 70 |
| PhotoService | PhotoServiceTest | 70 |
| AddressService | AddressServiceTest | 70 |
| ContactFieldService | ContactFieldServiceTest | 60 |
| ContactTagService | ContactTagServiceTest | 52 |
| ContactService | ContactServiceTest | 75 |
| UserService | UserServiceTest | 70 |
| ComplianceService | ComplianceServiceTest | 72 |
| AuditLogService | AuditLogServiceTest | 60 |
| ActivityTypeService | ActivityTypeServiceTest | 45 |
| ActivityTypeCategoryService | ActivityTypeCategoryServiceTest | 45 |
| CurrencyService | CurrencyServiceTest | 25 |
| CountryService | CountryServiceTest | 25 |
| GenderService | GenderServiceTest | 15 |
| GroupService | GroupServiceTest | 60 |
| RelationshipTypeService | RelationshipTypeServiceTest | 20 |
| RelationshipTypeGroupService | RelationshipTypeGroupServiceTest | 22 |

### Best Practices

1. **Use Builders for Test Data** - Always use ServiceTestBase builders instead of manually creating maps
2. **Verify Field Mapping** - Test that camelCase fields are mapped to snake_case for API calls
3. **Test All ID Types** - Verify Long, Integer, and String ID parsing works correctly
4. **Test Validation Early** - Validate required fields before making API calls
5. **Mock ContentFormatter** - Always mock the formatter to avoid test dependencies
6. **Use Specific Assertions** - Prefer specific assertions over generic `assertNotNull`
7. **Verify No Interactions** - Use `verifyNoInteractions(monicaClient)` when validation should prevent API calls

---

## Advanced Testing

### MCP Inspector Integration

For interactive testing:

```bash
# Install MCP Inspector
npm install -g @anthropic/mcp-inspector

# Test with JAR
mcp-inspector java -jar build/libs/monicahqmcp-0.1.0.jar --stdio

# Test with Docker
mcp-inspector docker run --rm -i -e MONICA_API_URL -e MONICA_API_TOKEN monicahq-mcp
```

### Performance Testing

Validate the 7-phase testing suite executes within 5 minutes:

```bash
time ./test-mcp-complete.sh
```

### Custom Test Scenarios

Create custom test scenarios in `tests/custom/`:

```bash
mkdir -p tests/custom
echo '#!/bin/bash
# Custom test scenario
echo "Custom test passed"' > tests/custom/my_test.sh
chmod +x tests/custom/my_test.sh
```

## Documentation

### Test Documentation Standards

All test files should include:

```bash
#!/bin/bash
# Test Purpose: Brief description
# Constitutional Principle: Which principle this validates
# Expected Behavior: What should pass/fail
```

### Constitutional Documentation

See `.specify/memory/constitution.md` for:
- Complete constitutional framework
- All 5 core principles
- Governance process
- Amendment procedures

---

## Support

For testing issues:

1. **Review this guide** for common solutions
2. **Check constitutional compliance** with `./validate-constitution.sh`
3. **Validate STDOUT cleanliness** - common Claude Desktop integration issue
4. **Ensure environment variables** are properly configured

**Remember**: The constitutional framework ensures quality, maintainability, and Claude Desktop compatibility. All tests serve the purpose of maintaining these standards.

---

*Constitutional Governance Framework v1.1.0 | Last Updated: 2025-09-20*