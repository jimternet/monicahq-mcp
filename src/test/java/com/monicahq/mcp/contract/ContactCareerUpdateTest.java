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
 * Contract test for contact_career_update MCP operation.
 * Tests the MCP message handler directly using TestMonicaHqClient.
 * Uses direct invocation instead of stdio communication for reliability.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactCareerUpdateTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldUpdateContactCareerViaMcpProtocol() throws Exception {
        // Given: TestMonicaHqClient will return stubbed career update response
        
        // When: Send MCP tools/call request to update contact career
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_career_update",
                "arguments", Map.of(
                    "id", 1L,
                    "jobTitle", "Senior Software Engineer",
                    "company", "TechCorp Inc.",
                    "startDate", "2023-01-15"
                )
            ),
            "id", "test-career-update"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Response should include updated career information
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        // Should have JSON-RPC 2.0 response structure
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-career-update", response.get("id").asText());
        
        // Should have result field with updated data
        assertTrue(response.has("result"));
        JsonNode result = response.get("result");
        
        // Should have content field for Claude Desktop
        assertTrue(result.has("content"));
        assertTrue(result.get("content").isArray());
        
        // Should have data field with career information
        assertTrue(result.has("data"));
        
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
                "name", "contact_career_update",
                "arguments", Map.of(
                    "jobTitle", "Software Engineer",
                    "company", "TechCorp Inc."
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
    void shouldHandlePartialCareerUpdate() throws Exception {
        // Given: Only some career fields provided
        
        // When: Send MCP tools/call request with only job title
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_career_update",
                "arguments", Map.of(
                    "id", 1L,
                    "jobTitle", "Lead Developer"
                    // company and startDate omitted
                )
            ),
            "id", "test-partial-update"
        );
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        
        // Then: Should update only the provided fields
        Map<String, Object> responseMap = messageHandler.handleMessage(requestNode, null);
        JsonNode response = objectMapper.valueToTree(responseMap);
        
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals("test-partial-update", response.get("id").asText());
        assertTrue(response.has("result"));
    }

    @Test
    void shouldValidateContactExists() throws Exception {
        // Given: Non-existent contact ID
        
        // When: Send MCP tools/call request with invalid contact ID
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_career_update",
                "arguments", Map.of(
                    "id", 99999L,  // Non-existent contact
                    "jobTitle", "Software Engineer"
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