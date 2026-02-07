package com.monicahq.mcp.contract;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Contract test for lifeevent_create MCP operation.
 * Tests creation of life events for contacts.
 * Part of Phase 1: Life Event Management implementation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class LifeEventCreateTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCreateLifeEventViaMcpProtocol() {
        // Given: MCP request to create a life event
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "lifeEventTypeId", 1,
                    "name", "Graduated from university",
                    "happenedAt", "2020-06-15",
                    "note", "Completed BS in Computer Science"
                )
            ),
            "id", 1
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldValidateRequiredFieldsForLifeEvent() {
        // Given: MCP request missing required fields
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_create",
                "arguments", Map.of(
                    "name", "Missing contact and type"
                )
            ),
            "id", 2
        );

        // When: Send invalid MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Verify validation error
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("error"));

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
    }

    @Test
    void shouldCreateLifeEventWithPartialDate() {
        // Given: MCP request with unknown day/month flags
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "lifeEventTypeId", 2,
                    "name", "Started new job",
                    "happenedAt", "2021-00-00",
                    "happenedAtMonthUnknown", true,
                    "happenedAtDayUnknown", true
                )
            ),
            "id", 3
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should allow partial dates
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldCreateLifeEventWithReminder() {
        // Given: MCP request with reminder ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "lifeEventTypeId", 3,
                    "name", "Anniversary",
                    "happenedAt", "2015-08-20",
                    "reminderId", 100
                )
            ),
            "id", 4
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should associate reminder
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }
}
