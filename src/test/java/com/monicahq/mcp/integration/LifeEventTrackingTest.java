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
 * Integration test for life event tracking workflow.
 * Tests the complete life event lifecycle: create → get → update → delete.
 * Part of Phase 1: Life Event Management validation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class LifeEventTrackingTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompleteLifeEventLifecycle() {
        // Test 1: Create life event
        Map<String, Object> createRequest = Map.of(
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

        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Should create life event successfully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);
        assertEquals("Graduated from university", data.get("name"));

        // Test 2: Get the created life event
        Object eventId = data.get("id");
        Map<String, Object> getRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_get",
                "arguments", Map.of("id", eventId)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(getRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should retrieve life event successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        Map<String, Object> getResult = (Map<String, Object>) response.get("result");
        Map<String, Object> getData = (Map<String, Object>) getResult.get("data");
        assertEquals("Graduated from university", getData.get("name"));
        assertEquals("Completed BS in Computer Science", getData.get("note"));

        // Test 3: Update life event
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_update",
                "arguments", Map.of(
                    "id", eventId,
                    "name", "Graduated from MIT",
                    "note", "Completed BS in CS with honors"
                )
            ),
            "id", 3
        );

        requestNode = objectMapper.valueToTree(updateRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should update life event successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        Map<String, Object> updateResult = (Map<String, Object>) response.get("result");
        Map<String, Object> updateData = (Map<String, Object>) updateResult.get("data");
        assertEquals("Graduated from MIT", updateData.get("name"));
        assertEquals("Completed BS in CS with honors", updateData.get("note"));

        // Test 4: Delete life event
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "lifeevent_delete",
                "arguments", Map.of("id", eventId)
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
