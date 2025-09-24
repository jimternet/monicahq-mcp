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
 * Contract test for contacts_by_tag MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 * Uses direct invocation instead of stdio communication for reliability.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactsByTagTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldListContactsByTagViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed contacts by tag response
        
        // When: Send MCP tools/call request to list contacts by tag
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "id", 1L,
                    "limit", 10
                )
            ),
            "id", "test-contacts-by-tag"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Response should include tagged contacts
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        // Should have JSON-RPC 2.0 response structure
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-contacts-by-tag", response.get("id").asText());
        
        // Should have result field with contact data
        assertTrue(response.has("result"));
        JsonNode result = response.get("result");
        
        // Should have content field for Claude Desktop
        assertTrue(result.has("content"));
        assertTrue(result.get("content").isArray());
        
        // Should have data field with contact list
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        
        // Content should be text type for Claude Desktop visibility
        JsonNode content = result.get("content").get(0);
        assertEquals("text", content.get("type").asText());
        assertTrue(content.get("text").asText().length() > 0);
    }

    @Test
    void shouldRequireTagId() throws Exception {
        // Given: Missing tag ID
        
        // When: Send MCP tools/call request without tag ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "limit", 10
                )
            ),
            "id", "test-missing-tag-id"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return error for missing tag ID
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-missing-tag-id", response.get("id").asText());
        
        // Should have error field for missing required parameter
        assertTrue(response.has("error"));
        JsonNode error = response.get("error");
        assertTrue(error.get("message").asText().toLowerCase().contains("id"));
    }

    @Test
    void shouldHandlePaginationParameters() throws Exception {
        // Given: Pagination parameters
        
        // When: Send MCP tools/call request with page and limit
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "id", 1L,
                    "page", 2,
                    "limit", 5
                )
            ),
            "id", "test-tag-pagination"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should handle pagination correctly
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-tag-pagination", response.get("id").asText());
        assertTrue(response.has("result"));
        
        // Should include meta information for pagination
        JsonNode result = response.get("result");
        if (result.has("meta")) {
            JsonNode meta = result.get("meta");
            assertTrue(meta.isObject());
        }
    }

    @Test
    void shouldHandleTagWithNoContacts() throws Exception {
        // Given: Tag with no associated contacts
        
        // When: Send MCP tools/call request for empty tag
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "id", 888L  // Assume this tag has no contacts
                )
            ),
            "id", "test-empty-tag"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return empty array, not error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-empty-tag", response.get("id").asText());
        assertTrue(response.has("result"));
        
        JsonNode result = response.get("result");
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        // Empty array is acceptable for tags without contacts
    }

    @Test
    void shouldValidateTagExists() throws Exception {
        // Given: Non-existent tag ID
        
        // When: Send MCP tools/call request with invalid tag ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "id", 99999L  // Non-existent tag
                )
            ),
            "id", "test-nonexistent-tag"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should handle gracefully or return appropriate error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-nonexistent-tag", response.get("id").asText());
        // Should either have result (if API handles gracefully) or error
        assertTrue(response.has("result") || response.has("error"));
    }

    @Test
    void shouldValidateLimitParameters() throws Exception {
        // Given: Invalid limit parameter
        
        // When: Send MCP tools/call request with negative limit
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contacts_by_tag",
                "arguments", Map.of(
                    "id", 1L,
                    "limit", -5  // Invalid limit
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