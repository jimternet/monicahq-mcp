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
 * Contract test for activity attendees array format support.
 * Tests both array ["John Doe"] and object [{"contactId": 123}] formats.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 * ActivityService doesn't support both formats yet.
 */
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ActivityAttendeesFormatTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldSupportStringArrayAttendeesFormat() throws Exception {
        // Given: MCP request with string array attendees format
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Team meeting",
                    "description", "Weekly standup meeting",
                    "happenedAt", "2025-09-13T10:30:00Z",
                    "attendees", List.of("John Doe", "Jane Smith", "Bob Wilson")
                )
            ),
            "id", 1
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure and attendees are properly processed
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        
        // Verify attendees are properly processed and normalized to object format
        assertTrue(data.containsKey("attendees"), "Activity should include attendees field");
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> attendees = (List<Map<String, Object>>) data.get("attendees");
        assertEquals(3, attendees.size());
        
        // Verify each attendee has been normalized to object format with name property
        assertEquals("John Doe", attendees.get(0).get("name"));
        assertEquals("Jane Smith", attendees.get(1).get("name"));
        assertEquals("Bob Wilson", attendees.get(2).get("name"));
    }

    @Test
    void shouldSupportObjectArrayAttendeesFormat() throws Exception {
        // Given: MCP request with object array attendees format (contactId references)
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Client presentation",
                    "description", "Quarterly review with stakeholders",
                    "happenedAt", "2025-09-13T14:00:00Z",
                    "attendees", List.of(
                        Map.of("contactId", 101),
                        Map.of("contactId", 102),
                        Map.of("contactId", 103)
                    )
                )
            ),
            "id", 2
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure and attendees are properly processed
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        
        // This will FAIL because ActivityService doesn't handle object array format yet
        assertTrue(data.containsKey("attendees"), "Activity should include attendees field");
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> attendees = (List<Map<String, Object>>) data.get("attendees");
        assertEquals(3, attendees.size());
        
        // Verify each attendee has contactId
        for (Map<String, Object> attendee : attendees) {
            assertTrue(attendee.containsKey("contactId"));
            assertTrue(attendee.get("contactId") instanceof Integer);
        }
    }

    @Test
    void shouldSupportMixedAttendeesFormat() throws Exception {
        // Given: MCP request with mixed attendees format (both strings and objects)
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Conference call",
                    "description", "Mixed internal and external participants",
                    "happenedAt", "2025-09-13T16:00:00Z",
                    "attendees", List.of(
                        "External Consultant", // String format
                        Map.of("contactId", 201), // Object format
                        "Anonymous Participant", // String format
                        Map.of("contactId", 202) // Object format
                    )
                )
            ),
            "id", 3
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure and mixed attendees are properly processed
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        
        // Verify mixed format is normalized properly to all objects
        assertTrue(data.containsKey("attendees"), "Activity should include attendees field");
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> attendees = (List<Map<String, Object>>) data.get("attendees");
        assertEquals(4, attendees.size());
        
        // Verify mixed format is normalized: strings→{name:...}, objects→{contactId:...}
        boolean hasNameProperty = false;
        boolean hasContactIdProperty = false;
        
        for (Map<String, Object> attendee : attendees) {
            if (attendee.containsKey("name")) {
                hasNameProperty = true;
                // Should be from string inputs: "External Consultant" or "Anonymous Participant"
                String name = (String) attendee.get("name");
                assertTrue(name.equals("External Consultant") || name.equals("Anonymous Participant"));
            } else if (attendee.containsKey("contactId")) {
                hasContactIdProperty = true;
                // Should be from object inputs: 201 or 202
                Integer contactId = (Integer) attendee.get("contactId");
                assertTrue(contactId.equals(201) || contactId.equals(202));
            }
        }
        
        assertTrue(hasNameProperty, "Should have attendees with name property (from string inputs)");
        assertTrue(hasContactIdProperty, "Should have attendees with contactId property (from object inputs)");
    }

    @Test
    void shouldRejectInvalidAttendeesFormat() throws Exception {
        // Given: MCP request with invalid attendees format
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "activity_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "activityTypeId", 1,
                    "summary", "Invalid attendees test",
                    "description", "Testing invalid attendees format",
                    "happenedAt", "2025-09-13T18:00:00Z",
                    "attendees", List.of(
                        123, // Invalid: raw number
                        true, // Invalid: boolean
                        Map.of("invalidField", "value") // Invalid: object without contactId
                    )
                )
            ),
            "id", 4
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Expect validation error for invalid format
        // This will FAIL because detailed validation isn't implemented yet
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params
        
        String errorMessage = (String) error.get("message");
        assertTrue(errorMessage.contains("attendees"), 
            "Error message should mention attendees validation issue");
        assertTrue(errorMessage.contains("format") || errorMessage.contains("invalid"), 
            "Error message should indicate format validation failure");
    }
}