package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.monicahq.mcp.config.StdioMcpBaseTest;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for STDIO activity creation workflow.
 * Tests full STDIO communication flow for activity creation.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 * STDIO integration isn't complete yet.
 */
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class StdioActivityCreateTest extends StdioMcpBaseTest {

    @Test
    void shouldCreateActivityViaStdioProtocol() throws Exception {
        // Given: Verify activity_create tool is available
        JsonNode toolsResult = listToolsAndAssertAvailable("activity_create");
        
        // Verify tool schema is properly defined
        JsonNode tools = toolsResult.get("tools");
        JsonNode activityCreateTool = null;
        for (JsonNode tool : tools) {
            if ("activity_create".equals(tool.get("name").asText())) {
                activityCreateTool = tool;
                break;
            }
        }
        
        assertNotNull(activityCreateTool, "activity_create tool should be available");
        assertTrue(activityCreateTool.has("inputSchema"), "Tool should have input schema");
        
        // This will FAIL because STDIO integration isn't complete
        JsonNode inputSchema = activityCreateTool.get("inputSchema");
        assertJsonStructure(inputSchema, "properties.contactId", "object");
        assertJsonStructure(inputSchema, "properties.activityTypeId", "object");
        assertJsonStructure(inputSchema, "properties.summary", "object");
        assertJsonStructure(inputSchema, "properties.attendees", "object");

        // When: Create activity via STDIO
        Map<String, Object> activityArgs = Map.of(
            "contactId", 12345,
            "activityTypeId", 1,
            "summary", "STDIO Test Meeting",
            "description", "Testing activity creation via STDIO protocol",
            "happenedAt", "2025-09-13T10:30:00Z",
            "attendees", List.of("John Doe", "Jane Smith")
        );
        
        // This will FAIL because STDIO protocol implementation is incomplete
        JsonNode result = callToolAndAssertSuccess("activity_create", activityArgs, 1);
        
        // Then: Verify activity creation response
        assertTrue(result.has("data"), "Result should contain data field");
        JsonNode data = result.get("data");
        
        assertJsonStructure(data, "id", "number");
        assertJsonStructure(data, "summary", "string");
        assertJsonStructure(data, "description", "string");
        assertJsonStructure(data, "happenedAt", "string");
        
        assertEquals("STDIO Test Meeting", data.get("summary").asText());
        assertEquals("Testing activity creation via STDIO protocol", data.get("description").asText());
        assertEquals("2025-09-13T10:30:00Z", data.get("happenedAt").asText());
    }

    @Test
    void shouldHandleStdioActivityCreationWithMinimalFields() throws Exception {
        // Given: Minimal required fields for activity creation
        Map<String, Object> minimalArgs = Map.of(
            "contactId", 54321,
            "activityTypeId", 2,
            "summary", "Minimal STDIO Activity"
        );
        
        // When: Create activity with minimal fields via STDIO
        // This will FAIL because STDIO protocol implementation is incomplete
        JsonNode result = callToolAndAssertSuccess("activity_create", minimalArgs, 2);
        
        // Then: Verify minimal activity creation
        assertTrue(result.has("data"), "Result should contain data field");
        JsonNode data = result.get("data");
        
        assertJsonStructure(data, "id", "number");
        assertJsonStructure(data, "summary", "string");
        assertJsonStructure(data, "contactId", "number");
        assertJsonStructure(data, "activityTypeId", "number");
        
        assertEquals("Minimal STDIO Activity", data.get("summary").asText());
        assertEquals(54321, data.get("contactId").asInt());
        assertEquals(2, data.get("activityTypeId").asInt());
    }

    @Test
    void shouldValidateStdioActivityCreationErrors() throws Exception {
        // Given: Invalid activity creation arguments (missing required fields)
        Map<String, Object> invalidArgs = Map.of(
            "summary", "Invalid Activity"
            // Missing contactId and activityTypeId
        );
        
        // When: Attempt to create activity with invalid args via STDIO
        // This will FAIL because detailed STDIO error handling isn't implemented
        JsonNode error = callToolAndExpectError("activity_create", invalidArgs, 3);
        
        // Then: Verify proper error response structure
        assertJsonStructure(error, "code", "number");
        assertJsonStructure(error, "message", "string");
        
        assertEquals(-32602, error.get("code").asInt()); // Invalid params
        
        String errorMessage = error.get("message").asText();
        assertTrue(errorMessage.contains("contactId") || errorMessage.contains("required"),
            "Error message should indicate missing required fields");
    }

    @Test
    void shouldHandleStdioActivityCreationWithComplexAttendees() throws Exception {
        // Given: Activity with complex attendees structure
        Map<String, Object> complexArgs = Map.of(
            "contactId", 98765,
            "activityTypeId", 3,
            "summary", "Complex STDIO Activity",
            "description", "Activity with complex attendees via STDIO",
            "happenedAt", "2025-09-13T15:45:00Z",
            "attendees", List.of(
                Map.of("contactId", 101, "role", "presenter"),
                "External Guest",
                Map.of("contactId", 102, "role", "participant")
            )
        );
        
        // When: Create activity with complex attendees via STDIO
        // This will FAIL because complex attendees handling via STDIO isn't implemented
        JsonNode result = callToolAndAssertSuccess("activity_create", complexArgs, 4);
        
        // Then: Verify complex activity creation
        assertTrue(result.has("data"), "Result should contain data field");
        JsonNode data = result.get("data");
        
        assertJsonStructure(data, "id", "number");
        assertJsonStructure(data, "summary", "string");
        assertJsonStructure(data, "attendees", "array");
        
        assertEquals("Complex STDIO Activity", data.get("summary").asText());
        
        JsonNode attendees = data.get("attendees");
        assertTrue(attendees.size() >= 3, "Should have at least 3 attendees");
    }

    @Test
    void shouldMaintainStdioProtocolCompliance() throws Exception {
        // Given: Multiple sequential STDIO requests to test protocol state
        Map<String, Object> firstActivity = Map.of(
            "contactId", 11111,
            "activityTypeId", 1,
            "summary", "First STDIO Activity"
        );
        
        Map<String, Object> secondActivity = Map.of(
            "contactId", 22222,
            "activityTypeId", 2,
            "summary", "Second STDIO Activity"
        );
        
        // When: Create multiple activities sequentially via STDIO
        // This will FAIL because STDIO protocol state management isn't complete
        JsonNode firstResult = callToolAndAssertSuccess("activity_create", firstActivity, 5);
        JsonNode secondResult = callToolAndAssertSuccess("activity_create", secondActivity, 6);
        
        // Then: Verify both activities were created independently
        assertTrue(firstResult.has("data"), "First result should contain data");
        assertTrue(secondResult.has("data"), "Second result should contain data");
        
        JsonNode firstData = firstResult.get("data");
        JsonNode secondData = secondResult.get("data");
        
        assertEquals("First STDIO Activity", firstData.get("summary").asText());
        assertEquals("Second STDIO Activity", secondData.get("summary").asText());
        
        // Verify different contact IDs were preserved
        assertEquals(11111, firstData.get("contactId").asInt());
        assertEquals(22222, secondData.get("contactId").asInt());
        
        // Verify different activity type IDs were preserved
        assertEquals(1, firstData.get("activityTypeId").asInt());
        assertEquals(2, secondData.get("activityTypeId").asInt());
    }

    @Test
    void shouldHandleStdioConnectionTimeout() throws Exception {
        // Given: Activity creation that might timeout
        Map<String, Object> timeoutArgs = Map.of(
            "contactId", 99999,
            "activityTypeId", 1,
            "summary", "Timeout Test Activity",
            "description", "Testing STDIO timeout handling"
        );
        
        // When: Attempt activity creation that might timeout
        // This will FAIL because STDIO timeout handling isn't implemented
        try {
            JsonNode result = callToolAndAssertSuccess("activity_create", timeoutArgs, 7);
            
            // Then: If successful, verify normal response
            assertTrue(result.has("data"), "Result should contain data field");
            assertJsonStructure(result.get("data"), "summary", "string");
            
        } catch (Exception e) {
            // Or verify proper timeout error handling
            assertTrue(e.getMessage().contains("timeout") || e.getMessage().contains("connection"),
                "Exception should indicate timeout or connection issue");
        }
    }
}