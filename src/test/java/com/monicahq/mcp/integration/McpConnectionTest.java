package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class McpConnectionTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldHandleMcpInitializeMessage() {
        Map<String, Object> initRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(
                    "tools", Map.of(),
                    "logging", Map.of()
                ),
                "clientInfo", Map.of(
                    "name", "test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(initRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertNotNull(result.get("protocolVersion"));
        assertNotNull(result.get("serverInfo"));
        assertNotNull(result.get("capabilities"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> serverInfo = (Map<String, Object>) result.get("serverInfo");
        assertNotNull(serverInfo.get("name"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> capabilities = (Map<String, Object>) result.get("capabilities");
        assertNotNull(capabilities.get("tools"));
    }

    @Test
    void shouldListAvailableTools() {
        Map<String, Object> toolsListRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/list",
            "params", Map.of(),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsListRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("tools"));
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> tools = (List<Map<String, Object>>) result.get("tools");
        assertNotNull(tools);
        assertEquals(136, tools.size()); // Phase 4.1-4.3 + API Gap Fixes: Total operations including new entities (Debt, Document, Photo, Gift, AuditLog, Country, Currency) + 14 API gap fix operations
    }

    @Test
    void shouldValidateJsonRpcFormat() {
        Map<String, Object> invalidRequest = Map.of(
            "method", "tools/call", // Missing jsonrpc field
            "params", Map.of("name", "contact_list"),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(invalidRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32600, error.get("code")); // Invalid Request
    }

    @Test
    void shouldHandleInvalidMethod() {
        Map<String, Object> invalidMethodRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "invalid_method",
            "params", Map.of(),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(invalidMethodRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32601, error.get("code")); // Method not found
    }

    @Test
    void shouldHandleInvalidToolName() {
        Map<String, Object> invalidToolRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "non_existent_tool",
                "arguments", Map.of()
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(invalidToolRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params
    }

    @Test
    void shouldMaintainCorrelationIds() {
        Map<String, Object> mcpRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/list",
            "params", Map.of(),
            "id", "correlation-test-123"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(mcpRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals("correlation-test-123", response.get("id"));
        assertTrue(response.containsKey("result"));
    }
}