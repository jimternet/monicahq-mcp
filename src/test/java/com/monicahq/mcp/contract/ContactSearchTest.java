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
 * Contract test for contact_search MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 * Uses direct invocation instead of stdio communication for reliability.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactSearchTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldSearchContactsViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed contact search response
        
        // When: Send MCP tools/call request to search contacts
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "limit", 10
                )
            ),
            "id", "test-search-contacts"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Response should include search results
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        // Should have JSON-RPC 2.0 response structure
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-search-contacts", response.get("id").asText());
        
        // Should have result field with search data
        assertTrue(response.has("result"));
        JsonNode result = response.get("result");
        
        // Should have content field for Claude Desktop
        assertTrue(result.has("content"));
        assertTrue(result.get("content").isArray());
        
        // Should have data field with search results
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        
        // Content should be text type for Claude Desktop visibility
        JsonNode content = result.get("content").get(0);
        assertEquals("text", content.get("type").asText());
        assertTrue(content.get("text").asText().length() > 0);
    }

    @Test
    void shouldHandleEmptySearchQuery() throws Exception {
        // Given: Empty search query
        
        // When: Send MCP tools/call request with empty query
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "",
                    "limit", 10
                )
            ),
            "id", "test-empty-search"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return empty results, not error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-empty-search", response.get("id").asText());
        assertTrue(response.has("result"));
    }

    @Test
    void shouldValidateSearchParameters() throws Exception {
        // Given: Invalid limit parameter
        
        // When: Send MCP tools/call request with invalid limit
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "limit", -1  // Invalid limit
                )
            ),
            "id", "test-invalid-limit"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should handle gracefully with default limit
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-invalid-limit", response.get("id").asText());
        // Should still have result (with default limit applied)
        assertTrue(response.has("result"));
    }
}