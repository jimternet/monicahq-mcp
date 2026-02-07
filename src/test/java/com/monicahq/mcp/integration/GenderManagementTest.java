package com.monicahq.mcp.integration;

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
 * Integration test for gender management workflow.
 * Tests the complete gender lifecycle: create → get → update → delete.
 * Part of Phase 1: Reference Data Management validation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class GenderManagementTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompleteGenderLifecycle() {
        // Test 1: Create custom gender
        Map<String, Object> createRequest = Map.of(
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

        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Should create gender successfully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);
        assertEquals("Non-binary", data.get("name"));

        // Test 2: Get the created gender
        Object genderId = data.get("id");
        Map<String, Object> getRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_get",
                "arguments", Map.of("id", genderId)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(getRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should retrieve gender successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        result = (Map<String, Object>) response.get("result");
        data = (Map<String, Object>) result.get("data");
        assertEquals("Non-binary", data.get("name"));

        // Test 3: Update gender
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_update",
                "arguments", Map.of(
                    "id", genderId,
                    "name", "Non-binary (Updated)"
                )
            ),
            "id", 3
        );

        requestNode = objectMapper.valueToTree(updateRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should update gender successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        result = (Map<String, Object>) response.get("result");
        data = (Map<String, Object>) result.get("data");
        assertEquals("Non-binary (Updated)", data.get("name"));

        // Test 4: Delete gender
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "gender_delete",
                "arguments", Map.of("id", genderId)
            ),
            "id", 4
        );

        requestNode = objectMapper.valueToTree(deleteRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should delete successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}
