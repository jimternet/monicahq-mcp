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
 * Contract test for gender_create MCP operation.
 * Tests creation of custom gender types in MonicaHQ.
 * Part of Phase 1: Reference Data Management implementation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class GenderCreateTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCreateGenderViaMcpProtocol() {
        // Given: MCP request to create a custom gender type
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_create",
                "arguments", Map.of(
                    "name", "Non-binary"
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
    void shouldValidateRequiredFieldsForGender() {
        // Given: MCP request missing required 'name' field
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_create",
                "arguments", Map.of()
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
    void shouldCreateGenderWithCustomName() {
        // Given: MCP request with custom gender name
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_create",
                "arguments", Map.of(
                    "name", "Genderqueer"
                )
            ),
            "id", 3
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Verify successful creation
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }
}
