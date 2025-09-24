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
 * Contract test for contact_audit_logs MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 * Uses direct invocation instead of stdio communication for reliability.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactAuditLogTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldGetContactAuditLogsViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed audit logs response
        
        // When: Send MCP tools/call request to get contact audit logs
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_audit_logs",
                "arguments", Map.of(
                    "id", 1L,
                    "limit", 20
                )
            ),
            "id", "test-audit-logs"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Response should include audit log entries
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        // Should have JSON-RPC 2.0 response structure
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-audit-logs", response.get("id").asText());
        
        // Should have result field with audit log data
        assertTrue(response.has("result"));
        JsonNode result = response.get("result");
        
        // Should have content field for Claude Desktop
        assertTrue(result.has("content"));
        assertTrue(result.get("content").isArray());
        
        // Should have data field with audit log entries
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        
        // Content should be text type for Claude Desktop visibility
        JsonNode content = result.get("content").get(0);
        assertEquals("text", content.get("type").asText());
        assertTrue(content.get("text").asText().length() > 0);
    }

    @Test
    void shouldRequireContactId() throws Exception {
        // Given: Missing contact ID
        
        // When: Send MCP tools/call request without contact ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_audit_logs",
                "arguments", Map.of(
                    "limit", 20
                )
            ),
            "id", "test-missing-id"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return error for missing ID
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-missing-id", response.get("id").asText());
        
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
                "name", "contact_audit_logs",
                "arguments", Map.of(
                    "id", 1L,
                    "page", 2,
                    "limit", 10
                )
            ),
            "id", "test-pagination"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should handle pagination correctly
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-pagination", response.get("id").asText());
        assertTrue(response.has("result"));
        
        // Should include meta information for pagination
        JsonNode result = response.get("result");
        if (result.has("meta")) {
            JsonNode meta = result.get("meta");
            // May include pagination info like total, current_page, etc.
            assertTrue(meta.isObject());
        }
    }

    @Test
    void shouldHandleEmptyAuditLog() throws Exception {
        // Given: Contact with no audit history
        
        // When: Send MCP tools/call request for contact without history
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_audit_logs",
                "arguments", Map.of(
                    "id", 999L  // Assume this contact has no audit history
                )
            ),
            "id", "test-empty-audit"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return empty array, not error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-empty-audit", response.get("id").asText());
        assertTrue(response.has("result"));
        
        JsonNode result = response.get("result");
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        // Empty array is acceptable for contacts without history
    }

    @Test
    void shouldValidateContactExists() throws Exception {
        // Given: Non-existent contact ID
        
        // When: Send MCP tools/call request with invalid contact ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_audit_logs",
                "arguments", Map.of(
                    "id", 99999L  // Non-existent contact
                )
            ),
            "id", "test-nonexistent-contact"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should handle gracefully or return appropriate error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-nonexistent-contact", response.get("id").asText());
        // Should either have result (if API handles gracefully) or error
        assertTrue(response.has("result") || response.has("error"));
    }
}