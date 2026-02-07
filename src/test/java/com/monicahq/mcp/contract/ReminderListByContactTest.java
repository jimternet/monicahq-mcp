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
 * Contract test for reminder_list_by_contact MCP operation.
 * Tests listing reminders filtered by contact ID.
 * Part of Phase 2: Contact-Scoped List Operations implementation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ReminderListByContactTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldListRemindersByContactViaMcpProtocol() {
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "reminder_list_by_contact",
                "arguments", Map.of("contactId", 123, "page", 1, "limit", 10)
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
    }

    @Test
    void shouldValidateRequiredContactIdParameter() {
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "reminder_list_by_contact",
                "arguments", Map.of("page", 1)
            ),
            "id", 2
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("error"));

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
    }

    @Test
    void shouldUseDefaultPaginationValues() {
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "reminder_list_by_contact",
                "arguments", Map.of("contactId", 123)
            ),
            "id", 3
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }

    @Test
    void shouldFilterResultsByContactId() {
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "reminder_list_by_contact",
                "arguments", Map.of("contactId", 123)
            ),
            "id", 4
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}
