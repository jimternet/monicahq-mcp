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
 * Integration test for place/location management workflow.
 * Tests the complete place lifecycle: create → get → update → list → delete.
 * Part of Phase 1: Geographic Location Management validation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class PlaceLifecycleTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompletePlaceLifecycle() {
        // Test 1: Create place with full address
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_create",
                "arguments", Map.of(
                    "street", "123 Integration Test Street",
                    "city", "San Francisco",
                    "province", "California",
                    "postalCode", "94102",
                    "country", "USA",
                    "latitude", 37.7749,
                    "longitude", -122.4194
                )
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Should create place successfully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);
        assertEquals("San Francisco", data.get("city"));

        // Test 2: Get the created place
        Object placeId = data.get("id");
        Map<String, Object> getRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_get",
                "arguments", Map.of("id", placeId)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(getRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should retrieve place successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        Map<String, Object> getResult = (Map<String, Object>) response.get("result");
        Map<String, Object> getdata = (Map<String, Object>) getResult.get("data");
        assertEquals("123 Integration Test Street", getdata.get("street"));
        assertEquals("San Francisco", getdata.get("city"));

        // Test 3: Update place
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_update",
                "arguments", Map.of(
                    "id", placeId,
                    "street", "456 Updated Street"
                )
            ),
            "id", 3
        );

        requestNode = objectMapper.valueToTree(updateRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should update place successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        Map<String, Object> updateResult = (Map<String, Object>) response.get("result");
        Map<String, Object> updateData = (Map<String, Object>) updateResult.get("data");
        assertEquals("456 Updated Street", updateData.get("street"));

        // Test 4: List places
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_list",
                "arguments", Map.of("limit", 10)
            ),
            "id", 4
        );

        requestNode = objectMapper.valueToTree(listRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should list places successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        // Test 5: Delete place
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_delete",
                "arguments", Map.of("id", placeId)
            ),
            "id", 5
        );

        requestNode = objectMapper.valueToTree(deleteRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should delete successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}
