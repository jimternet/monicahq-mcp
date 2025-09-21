# MonicaHQ MCP Server Constitution

<!--
Sync Impact Report:
Version change: 1.3.0 → 2.0.0 (Major MCP Protocol Compliance Enhancement)
Modified principles: 
  - Principle I - Enhanced with airtight MCP response structure requirements and forbidden/mandatory patterns
  - Principle II - Added MCP-specific testing requirements including MCP Inspector validation
  - Principle III - Enhanced with Spring Boot MCP integration standards and required tool patterns
  - Added comprehensive MCP Implementation Error Prevention section
Added sections: 
  - MCP Implementation Error Prevention with critical validation requirements
  - Response structure testing patterns and error prevention checklists
Removed sections: None
Templates requiring updates: 
  ✅ .specify/templates/plan-template.md - Update to include MCP Inspector validation and Claude Desktop testing
Follow-up TODOs: 
  - Validate current implementation against new MCP response structure requirements
  - Ensure all tools return content arrays instead of data fields at result level
  - Add MCP Inspector testing to validation pipeline
  - Verify Claude Desktop integration with absolute paths
  - Update all tool response patterns to match constitutional requirements
-->

## Core Principles

### I. MCP Protocol First - Airtight Response Structure
The MonicaHQ MCP Server MUST prioritize proper MCP protocol implementation above all other concerns. Every operation MUST follow JSON-RPC 2.0 over STDIO specification with EXACT response structure compliance.

**CRITICAL MCP RESPONSE STRUCTURE REQUIREMENT:**
ALL MCP tool responses MUST use this EXACT structure:
```json
{
  "jsonrpc": "2.0",
  "id": "<request-id>",
  "result": {
    "content": [
      {
        "type": "text",
        "text": "<response-data>"
      }
    ]
  }
}
```

**FORBIDDEN PATTERNS:**
- NEVER use a "data" field at the result level
- NEVER return plain strings or objects without content wrapper  
- NEVER mix stdio and HTTP transport configurations
- NEVER use relative paths in Claude Desktop configuration

**MANDATORY PATTERNS:**
- ALL responses MUST be wrapped in content arrays with type="text"
- ALL paths in configurations MUST be absolute
- ALL tools MUST include comprehensive error handling with proper MCP error responses
- ALL tools MUST be properly categorized and discoverable through standard MCP tool listing

Protocol compliance is non-negotiable for Claude Desktop integration. Any implementation that fails MCP Inspector or Claude Desktop validation is incomplete and MUST be fixed before deployment.

### II. Test-Driven Development (NON-NEGOTIABLE)
All functionality MUST be implemented following strict TDD methodology. Tests MUST be written before implementation, MUST fail initially, then implementation MUST make them pass. The current standard is 147/147 tests passing (100%). Any new feature reducing test coverage below 100% is forbidden without explicit architectural justification.

**MCP-SPECIFIC TESTING REQUIREMENTS:**
- ALL tools MUST have response structure validation tests confirming content array format
- ALL tools MUST be tested with actual MCP Inspector for protocol compliance
- ALL tools MUST include error case testing with proper MCP error response validation
- ALL tools MUST have integration tests verifying Claude Desktop compatibility
- NO tool implementation is complete without passing both unit tests AND MCP Inspector validation

### III. Spring Boot Architecture Excellence - MCP Integration Standards
All code MUST follow Spring Boot 3.x patterns with appropriate reactive design and MCP integration requirements.

**SPRING BOOT MCP SERVER IMPLEMENTATION REQUIREMENTS:**
- Use Spring Boot MCP library for tool annotations and response handling
- ALL tool methods MUST use @McpTool annotations with clear names and descriptions
- ALL tool parameters MUST use @McpToolParameter with proper validation
- ALL responses MUST follow the McpToolResponse pattern with content arrays

**REQUIRED TOOL METHOD PATTERN:**
```java
@McpTool(name="tool_name", description="Clear description") 
public Map<String, Object> handleRequest(Map<String, Object> arguments) {
    try {
        // Validation and business logic
        String responseText = formatResponse(data);
        return Map.of("content", List.of(Map.of("type", "text", "text", responseText)));
    } catch (Exception e) {
        return Map.of("content", List.of(Map.of("type", "text", "text", "Error: " + e.getMessage())));
    }
}
```

**TRANSPORT CONFIGURATION:**
- STDIO transport MANDATORY for Claude Desktop integration
- WebFlux MANDATORY for external API clients (MonicaHQ API calls)
- Circuit breaker patterns with Resilience4j MANDATORY for external API calls
- All services MUST use dependency injection and proper component scanning

**ERROR HANDLING REQUIREMENTS:**
- ALL tools MUST include comprehensive input validation
- ALL errors MUST return properly formatted MCP responses with descriptive messages
- ALL external API failures MUST be handled gracefully with circuit breaker patterns
- ALL responses MUST include success/error indicators in human-readable format

### IV. Production-Ready Deployment
Every feature MUST support Docker containerization and Claude Desktop integration from day one. Environment variable configuration is MANDATORY for all external dependencies. Health checks, logging, and monitoring capabilities MUST be included. The deployment MUST work seamlessly in both local development and production environments.

### V. Type Safety and Code Generation
All data transfer objects MUST use MapStruct for type-safe mapping. Lombok MUST be used for reducing boilerplate code. All API interactions MUST be strongly typed with proper validation. Manual mapping or reflection-based approaches are forbidden where type-safe alternatives exist.

### VI. MCP Response Content Visibility & Complete Monica API Data Access
All MCP tool responses MUST include COMPLETE Monica API data as escaped JSON within the `content` field for Claude Desktop accessibility. Since Claude Desktop only processes the `content` field of responses, ALL raw Monica API response data MUST be provided as escaped JSON strings within the `content` field to enable direct data parsing and manipulation by Claude.

Every tool operation MUST:
- Include the complete raw Monica API response data as escaped JSON in the `content` field
- Preserve ALL fields returned by the Monica API without any filtering or transformation
- Use proper JSON escaping to ensure the data can be parsed by Claude Desktop
- Maintain the original Monica API response structure and field names
- Include both individual items and list responses as escaped JSON

**REQUIRED CONTENT FORMAT:**
```json
{
  "type": "text",
  "text": "{\"id\":123,\"name\":\"John Doe\",\"email\":\"john@example.com\",\"created_at\":\"2024-01-01T10:00:00Z\",...}"
}
```

**USER GUIDANCE REQUIREMENTS:**
When operations fail due to missing required values (like contactFieldTypeId), responses MUST include guidance on using discovery tools. Error messages MUST suggest relevant discovery endpoints (e.g., "Use gender_list to see valid gender options"). All tools requiring enum/type IDs MUST reference their corresponding discovery tools in descriptions.

This principle ensures complete programmatic data access where:
- Claude Desktop can directly parse and manipulate Monica API data from the content field
- No data transformation or summarization occurs between Monica API and Claude
- Structured data remains in its original JSON format for programmatic access
- Complex nested objects and arrays are preserved exactly as returned by Monica API
- Claude can perform direct JSON operations on the complete dataset

This principle applies to ALL operations across all entity types. No tool may return Monica API data only in the `data` field that Claude Desktop cannot access. The `content` field MUST contain the complete, unmodified Monica API response as escaped JSON. Tests MUST verify that the `content` field contains valid escaped JSON that matches the Monica API response exactly.

### VII. API Discovery and Completeness
All MCP tools MUST leverage Monica API's native discovery capabilities to provide complete, dynamic functionality rather than hardcoded limitations. The MCP server MUST expose ALL capabilities that Monica API provides.

**DISCOVERY REQUIREMENTS:**
- MANDATORY discovery tools for all enum/type endpoints: genders, contact field types, activity types, relationship types, countries, currencies
- Tool schemas MUST be dynamically informed by Monica API responses where possible
- NEVER hardcode enum values (like gender 1/2/3) when Monica provides discovery endpoints
- ALL entity types supported by Monica API MUST be implemented (life events, gifts, documents, addresses, relationships)

**COMPLETENESS REQUIREMENTS:**
- If Monica API supports an operation, the MCP server MUST support it
- Missing CRUD operations (like conversation/message delete) MUST be added if Monica supports them
- Feature coverage MUST match Monica's actual capabilities, not assumptions
- Discovery tools MUST be provided to eliminate user guesswork about valid values

**GUIDANCE INTEGRATION:**
- All tools requiring enum/type IDs MUST reference discovery tools in their descriptions
- Error messages MUST direct users to appropriate discovery tools when validation fails
- Tool documentation MUST include examples of using discovery tools to find valid values

This principle ensures the MCP server provides complete Monica API coverage with self-documenting capabilities, eliminating the need for users to guess valid values or discover limitations through trial and error.

## API Integration Standards

All MonicaHQ API integrations MUST follow OAuth2 Bearer token authentication. API clients MUST implement proper error handling, retries, and timeout configurations. All external API calls MUST be wrapped with circuit breaker patterns to prevent cascading failures. Rate limiting and backoff strategies are MANDATORY for production stability.

## Code Quality Requirements

Code MUST follow consistent naming conventions matching Spring Boot standards. All public APIs MUST be documented with appropriate JavaDoc. Configuration properties MUST be externalized and documented. Security best practices MUST be followed - no secrets in code, proper environment variable handling, and secure defaults for all configurations.

## MCP Implementation Error Prevention

**CRITICAL VALIDATION REQUIREMENTS:**
Before considering any implementation complete, ALL of the following MUST be verified:

**1. MCP Inspector Validation:**
- Test with `npx @modelcontextprotocol/inspector`
- Verify all responses show proper content array structure
- Confirm tool discovery and parameter schemas work correctly

**2. Claude Desktop Integration Testing:**
- Create test configuration in ~/claude_desktop_config.json with absolute paths
- Test tool discovery and execution in actual Claude Desktop environment
- Verify responses appear correctly in Claude interface

**3. Response Structure Testing:**
```java
@Test
public void testMcpResponseStructure() {
    Map<String, Object> response = tool.execute(arguments);
    assertThat(response).containsKey("content");
    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
    assertThat(content).hasSize(1);
    assertThat(content.get(0)).containsEntry("type", "text");
    assertThat(content.get(0)).containsKey("text");
    // NEVER assert on response.get("data") - this field must not exist at result level
}
```

**4. Error Prevention Checklist:**
❌ NEVER DO:
- Put response data in a "data" field at result level
- Return plain strings or objects without content wrapper
- Use relative paths in Claude Desktop configuration
- Return human-readable formatted text instead of escaped JSON in text field
- Transform or filter Monica API data before including in content
- Ignore error handling

✅ ALWAYS DO:
- Wrap ALL responses in content arrays with type="text"
- Use absolute paths in all file configurations  
- Return complete Monica API response as escaped JSON within text content
- Preserve all original Monica API fields and structure
- Include comprehensive error handling with descriptive messages
- Test with both MCP Inspector and Claude Desktop

**CODE REVIEW VALIDATION:**
Every tool implementation MUST pass this validation: "Does this response follow the exact MCP content array structure? Is the Monica API data provided as properly escaped JSON in the text field? Will Claude Desktop be able to parse and manipulate this data directly? Have I tested this with the MCP Inspector?"

## Governance

This constitution supersedes all other development practices and guidelines. All pull requests MUST be reviewed for constitutional compliance before merge. Any deviation from these principles MUST be explicitly justified in writing with architectural rationale. Breaking changes MUST include migration plans and backward compatibility considerations.

**Amendment Process**: Constitutional changes require documentation of impact analysis, approval from project maintainers, and update of dependent templates. All changes MUST maintain the production-ready status of the system.

**Version**: 2.1.0 | **Ratified**: 2025-09-20 | **Last Amended**: 2025-09-20