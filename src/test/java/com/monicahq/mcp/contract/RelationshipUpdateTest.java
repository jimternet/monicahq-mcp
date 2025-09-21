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
 * Contract test for relationship_update MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class RelationshipUpdateTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldUpdateRelationshipViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed relationship update response
        
        // When: Send MCP tools/call request to update relationship
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_update",
                "arguments", Map.of(
                    "id", 1L,
                    "contactIs", 1L,
                    "ofContact", 2L,
                    "relationshipTypeId", 2L,
                    "notes", "Updated relationship notes"
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldValidateRequiredFieldsForRelationshipUpdate() throws Exception {
        // Given: MCP request missing required fields
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_update",
                "arguments", Map.of(
                    "notes", "Some notes"  // Missing required id and other fields
                )
            ),
            "id", 2
        );

        // When: Send MCP request with missing required fields
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Expect validation error response
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
        assertTrue(error.get("message").toString().contains("Invalid params"));
    }
}