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
 * Contract test for place_create MCP operation.
 * Tests creation of geographic places/locations in MonicaHQ.
 * Part of Phase 1: Geographic Location Management implementation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class PlaceCreateTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCreatePlaceWithFullAddressViaMcpProtocol() {
        // Given: MCP request to create a place with complete address
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_create",
                "arguments", Map.of(
                    "street", "123 Main Street",
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
    void shouldCreateMinimalPlaceWithJustCity() {
        // Given: MCP request with minimal place data (city only)
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_create",
                "arguments", Map.of(
                    "city", "Seattle"
                )
            ),
            "id", 2
        );

        // When: Send MCP request
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Verify successful creation with minimal data
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldCreatePlaceWithCoordinatesOnly() {
        // Given: MCP request with GPS coordinates only
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "place_create",
                "arguments", Map.of(
                    "latitude", 40.7128,
                    "longitude", -74.0060
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
