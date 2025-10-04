package com.monicahq.mcp.contract;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Contract test for parameter validation error handling.
 * Tests detailed error messages for invalid parameters across all operations.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 * Detailed error messages and validation aren't implemented yet.
 */
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ParameterValidationErrorTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldProvideDetailedErrorForMissingRequiredFields() throws Exception {
        // Given: Contact create request missing required fields
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "lastName", "Smith"
                    // Missing required: firstName, genderId
                )
            ),
            "id", 1
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect detailed validation error
        // This will FAIL because detailed error messages aren't implemented
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params
        
        String errorMessage = (String) error.get("message");
        
        // Should contain specific field names that are missing
        assertTrue(errorMessage.contains("firstName"), 
            "Error should mention missing firstName field");
        assertTrue(errorMessage.contains("genderId"), 
            "Error should mention missing genderId field");
        assertTrue(errorMessage.contains("required"), 
            "Error should indicate fields are required");
        
        // Should not contain fields that are present
        assertFalse(errorMessage.contains("lastName"), 
            "Error should not mention lastName as it was provided");
    }

    @Test
    void shouldProvideDetailedErrorForInvalidFieldTypes() throws Exception {
        // Given: Activity create request with invalid field types
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", "not-a-number", // Should be integer
                    "activityTypeId", true, // Should be integer
                    "summary", 12345, // Should be string
                    "happenedAt", "invalid-date-format" // Should be ISO date
                )
            ),
            "id", 2
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect detailed type validation errors
        // This will FAIL because type validation isn't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should specify which fields have type errors
        assertTrue(errorMessage.contains("contactId") && errorMessage.contains("integer"), 
            "Error should specify contactId type issue");
        assertTrue(errorMessage.contains("activityTypeId") && errorMessage.contains("integer"), 
            "Error should specify activityTypeId type issue");
        assertTrue(errorMessage.contains("summary") && errorMessage.contains("string"), 
            "Error should specify summary type issue");
        assertTrue(errorMessage.contains("happenedAt") && 
                  (errorMessage.contains("date") || errorMessage.contains("ISO")), 
            "Error should specify happenedAt date format issue");
    }

    @Test
    void shouldProvideDetailedErrorForInvalidFieldValues() throws Exception {
        // Given: Task create request with invalid field values
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_create",
                "arguments", Map.of(
                    "contactId", -1, // Invalid: negative ID
                    "title", "", // Invalid: empty string
                    "isCompleted", "maybe", // Invalid: not boolean
                    "dueDate", "2023-13-45T25:70:90Z" // Invalid: impossible date
                )
            ),
            "id", 3
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect detailed value validation errors
        // This will FAIL because value validation isn't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should specify value constraint violations
        assertTrue(errorMessage.contains("contactId") && 
                  (errorMessage.contains("positive") || errorMessage.contains("greater than 0")), 
            "Error should specify contactId value constraint");
        assertTrue(errorMessage.contains("title") && errorMessage.contains("empty"), 
            "Error should specify title cannot be empty");
        assertTrue(errorMessage.contains("isCompleted") && errorMessage.contains("boolean"), 
            "Error should specify isCompleted boolean requirement");
        assertTrue(errorMessage.contains("dueDate") && errorMessage.contains("valid"), 
            "Error should specify dueDate validity issue");
    }

    @Test
    void shouldProvideDetailedErrorForInvalidArrayElements() throws Exception {
        // Given: Activity create with invalid attendees array elements
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Test Activity",
                    "attendees", List.of(
                        "Valid Attendee", // Valid string
                        123, // Invalid: number instead of string or object
                        Map.of("invalidField", "value"), // Invalid: object without contactId
                        "", // Invalid: empty string
                        Map.of("contactId", "not-a-number") // Invalid: contactId not integer
                    )
                )
            ),
            "id", 4
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect detailed array validation errors
        // This will FAIL because array validation isn't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should specify array element issues with indices
        assertTrue(errorMessage.contains("attendees"), 
            "Error should mention attendees field");
        assertTrue(errorMessage.contains("element") || errorMessage.contains("index"), 
            "Error should reference array elements");
        assertTrue(errorMessage.contains("string") || errorMessage.contains("object"), 
            "Error should specify expected types for attendees");
    }

    @Test
    void shouldProvideDetailedErrorForUnknownFields() throws Exception {
        // Given: Note create request with unknown fields
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "Test note content",
                    "unknownField1", "should be rejected",
                    "invalidProperty", 42,
                    "anotherUnknownField", true
                )
            ),
            "id", 5
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect detailed unknown field errors
        // This will FAIL because unknown field validation isn't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should list the unknown fields
        assertTrue(errorMessage.contains("unknownField1"), 
            "Error should mention unknownField1");
        assertTrue(errorMessage.contains("invalidProperty"), 
            "Error should mention invalidProperty");
        assertTrue(errorMessage.contains("anotherUnknownField"), 
            "Error should mention anotherUnknownField");
        assertTrue(errorMessage.contains("unknown") || errorMessage.contains("unexpected"), 
            "Error should indicate fields are unknown");
        
        // Should not mention valid fields
        assertFalse(errorMessage.contains("contactId"), 
            "Error should not mention valid contactId field");
        assertFalse(errorMessage.contains("body"), 
            "Error should not mention valid body field");
    }

    @Test
    void shouldProvideDetailedErrorForComplexValidationFailures() throws Exception {
        // Given: Complex request with multiple validation issues
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    // Missing required firstName
                    "lastName", "", // Invalid: empty string
                    "genderId", "invalid", // Invalid: not integer
                    "email", "not-an-email", // Invalid: malformed email
                    "unknownField", "value", // Invalid: unknown field
                    "phoneNumbers", List.of(
                        "valid-phone",
                        123, // Invalid: not string
                        "" // Invalid: empty string
                    )
                )
            ),
            "id", 6
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect comprehensive validation error summary
        // This will FAIL because comprehensive validation isn't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should categorize different types of validation failures
        assertTrue(errorMessage.contains("firstName") && errorMessage.contains("required"), 
            "Should mention missing required field");
        assertTrue(errorMessage.contains("lastName") && errorMessage.contains("empty"), 
            "Should mention empty field validation");
        assertTrue(errorMessage.contains("genderId") && errorMessage.contains("integer"), 
            "Should mention type validation");
        assertTrue(errorMessage.contains("email") && 
                  (errorMessage.contains("format") || errorMessage.contains("valid")), 
            "Should mention format validation");
        assertTrue(errorMessage.contains("unknownField") && errorMessage.contains("unknown"), 
            "Should mention unknown field");
        assertTrue(errorMessage.contains("phoneNumbers") && 
                  (errorMessage.contains("array") || errorMessage.contains("element")), 
            "Should mention array validation");
    }

    @Test
    void shouldProvideHelpfulSuggestionsInErrors() throws Exception {
        // Given: Common mistake in field naming
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_get",
                "arguments", Map.of(
                    "id", 12345 // Should be "contactId"
                )
            ),
            "id", 7
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect helpful suggestions in error message
        // This will FAIL because helpful suggestions aren't implemented
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        
        String errorMessage = (String) error.get("message");
        
        // Should suggest correct field name
        assertTrue(errorMessage.contains("contactId"), 
            "Error should suggest correct field name 'contactId'");
        assertTrue(errorMessage.contains("did you mean") || errorMessage.contains("suggestion"), 
            "Error should provide helpful suggestion");
    }
}