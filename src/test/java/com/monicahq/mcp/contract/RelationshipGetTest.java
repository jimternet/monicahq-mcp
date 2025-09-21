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
 * Contract test for relationship_get MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class RelationshipGetTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldGetRelationshipViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed relationship response
        
        // When: Send MCP tools/call request to get relationship
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_get",
                "arguments", Map.of(
                    "id", 1L
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
    void shouldValidateRequiredIdForRelationshipGet() throws Exception {
        // Given: MCP request missing required ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_get",
                "arguments", Map.of()  // Missing required id
            ),
            "id", 2
        );

        // When: Send MCP request with missing required ID
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