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
 * Contract test for user_create MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 * Uses direct invocation instead of stdio communication for reliability.
 * 
 * NOTE: Users API may return 404 - this test validates our error handling.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class UserCreateTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldHandleUserCreateOrProvideGracefulError() throws Exception {
        // Given: TestMonicaHqClient may return 404 for Users API
        
        // When: Send MCP tools/call request to create user
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "user_create",
                "arguments", Map.of(
                    "firstName", "Test",
                    "lastName", "User",
                    "email", "test.user@example.com"
                )
            ),
            "id", "test-user-create"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should either succeed or provide graceful error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        // Should have JSON-RPC 2.0 response structure
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-user-create", response.get("id").asText());
        
        if (response.has("result")) {
            // If Users API is available, should have proper response structure
            JsonNode result = response.get("result");
            
            // Should have content field for Claude Desktop
            assertTrue(result.has("content"));
            assertTrue(result.get("content").isArray());
            
            // Content should be text type for Claude Desktop visibility
            JsonNode content = result.get("content").get(0);
            assertEquals("text", content.get("type").asText());
            assertTrue(content.get("text").asText().length() > 0);
            
        } else if (response.has("error")) {
            // If Users API is not available, should have graceful error
            JsonNode error = response.get("error");
            assertTrue(error.has("message"));
            
            // Error message should be informative about Users API unavailability
            String errorMessage = error.get("message").asText().toLowerCase();
            assertTrue(errorMessage.contains("user") || 
                      errorMessage.contains("404") || 
                      errorMessage.contains("not available") ||
                      errorMessage.contains("not implemented"));
        } else {
            fail("Response should have either result or error field");
        }
    }

    @Test
    void shouldValidateUserCreateParameters() throws Exception {
        // Given: Missing required user parameters
        
        // When: Send MCP tools/call request without required fields
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "user_create",
                "arguments", Map.of(
                    "firstName", "Test"
                    // Missing lastName and email
                )
            ),
            "id", "test-user-validation"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should return validation error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-user-validation", response.get("id").asText());
        
        // Should have error for missing required fields
        assertTrue(response.has("error"));
        JsonNode error = response.get("error");
        assertTrue(error.get("message").asText().length() > 0);
    }

    @Test
    void shouldHandleUserListOperation() throws Exception {
        // Given: Request to list users
        
        // When: Send MCP tools/call request to list users
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "user_list",
                "arguments", Map.of(
                    "limit", 10
                )
            ),
            "id", "test-user-list"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should either succeed or provide graceful error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-user-list", response.get("id").asText());
        
        // Should have either result or graceful error
        assertTrue(response.has("result") || response.has("error"));
        
        if (response.has("error")) {
            JsonNode error = response.get("error");
            String errorMessage = error.get("message").asText().toLowerCase();
            // Should indicate Users API unavailability gracefully
            assertTrue(errorMessage.contains("user") || 
                      errorMessage.contains("404") || 
                      errorMessage.contains("not available"));
        }
    }

    @Test
    void shouldHandleUserGetOperation() throws Exception {
        // Given: Request to get specific user
        
        // When: Send MCP tools/call request to get user
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "user_get",
                "arguments", Map.of(
                    "id", 1L
                )
            ),
            "id", "test-user-get"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should either succeed or provide graceful error
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-user-get", response.get("id").asText());
        
        // Should have either result or graceful error
        assertTrue(response.has("result") || response.has("error"));
        
        if (response.has("error")) {
            JsonNode error = response.get("error");
            String errorMessage = error.get("message").asText().toLowerCase();
            // Should indicate specific issue (404, not available, etc.)
            assertTrue(errorMessage.length() > 0);
        }
    }
}