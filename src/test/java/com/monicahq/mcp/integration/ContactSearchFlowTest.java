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
 * Integration test for contact search workflow.
 * Tests the complete flow from MCP request to formatted response.
 * Validates search functionality, pagination, and error handling.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactSearchFlowTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCompleteContactSearchWorkflow() throws Exception {
        // Given: Contact search operation is registered and available
        
        // When: Execute complete search workflow
        
        // Step 1: Search for contacts with specific query
        Map<String, Object> searchRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "limit", 5
                )
            ),
            "id", "workflow-search"
        );
        
        JsonNode searchRequestNode = objectMapper.valueToTree(searchRequest);
        Map<String, Object> searchResponseMap = messageHandler.handleMessage(searchRequestNode, null);
        JsonNode searchResponse = objectMapper.valueToTree(searchResponseMap);
        
        // Then: Search should return results
        assertEquals("2.0", searchResponse.get("jsonrpc").asText());
        assertEquals("workflow-search", searchResponse.get("id").asText());
        assertTrue(searchResponse.has("result"));
        
        JsonNode searchResult = searchResponse.get("result");
        assertTrue(searchResult.has("data"));
        assertTrue(searchResult.has("content"));
        
        // Step 2: Verify pagination works with different page
        Map<String, Object> pageRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "page", 2,
                    "limit", 3
                )
            ),
            "id", "workflow-pagination"
        );
        
        JsonNode pageRequestNode = objectMapper.valueToTree(pageRequest);
        Map<String, Object> pageResponseMap = messageHandler.handleMessage(pageRequestNode, null);
        JsonNode pageResponse = objectMapper.valueToTree(pageResponseMap);
        
        // Then: Pagination should work
        assertEquals("2.0", pageResponse.get("jsonrpc").asText());
        assertEquals("workflow-pagination", pageResponse.get("id").asText());
        assertTrue(pageResponse.has("result"));
        
        // Step 3: Verify empty search handling
        Map<String, Object> emptyRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "NonExistentContactName12345",
                    "limit", 10
                )
            ),
            "id", "workflow-empty"
        );
        
        JsonNode emptyRequestNode = objectMapper.valueToTree(emptyRequest);
        Map<String, Object> emptyResponseMap = messageHandler.handleMessage(emptyRequestNode, null);
        JsonNode emptyResponse = objectMapper.valueToTree(emptyResponseMap);
        
        // Then: Empty search should return valid response
        assertEquals("2.0", emptyResponse.get("jsonrpc").asText());
        assertEquals("workflow-empty", emptyResponse.get("id").asText());
        assertTrue(emptyResponse.has("result"));
        
        JsonNode emptyResult = emptyResponse.get("result");
        assertTrue(emptyResult.has("data"));
        assertTrue(emptyResult.get("data").isArray());
        // Empty results are acceptable
    }

    @Test
    void shouldIntegrateWithContactDetailRetrieval() throws Exception {
        // Given: Contact search returns results
        
        // Step 1: Search for contacts
        Map<String, Object> searchRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "limit", 1
                )
            ),
            "id", "integration-search"
        );
        
        JsonNode searchRequestNode = objectMapper.valueToTree(searchRequest);
        Map<String, Object> searchResponseMap = messageHandler.handleMessage(searchRequestNode, null);
        JsonNode searchResponse = objectMapper.valueToTree(searchResponseMap);
        
        // Then: Should have search results
        assertTrue(searchResponse.has("result"));
        JsonNode searchResult = searchResponse.get("result");
        assertTrue(searchResult.has("data"));
        
        JsonNode contacts = searchResult.get("data");
        if (contacts.isArray() && contacts.size() > 0) {
            // Step 2: Get detailed contact information
            JsonNode firstContact = contacts.get(0);
            if (firstContact.has("id")) {
                Long contactId = firstContact.get("id").asLong();
                
                Map<String, Object> detailRequest = Map.of(
                    "jsonrpc", "2.0",
                    "method", "tools/call",
                    "params", Map.of(
                        "name", "contact_get",
                        "arguments", Map.of(
                            "id", contactId
                        )
                    ),
                    "id", "integration-detail"
                );
                
                JsonNode detailRequestNode = objectMapper.valueToTree(detailRequest);
                Map<String, Object> detailResponseMap = messageHandler.handleMessage(detailRequestNode, null);
                JsonNode detailResponse = objectMapper.valueToTree(detailResponseMap);
                
                // Then: Should get detailed contact information
                assertEquals("2.0", detailResponse.get("jsonrpc").asText());
                assertEquals("integration-detail", detailResponse.get("id").asText());
                assertTrue(detailResponse.has("result"));
                
                JsonNode detailResult = detailResponse.get("result");
                assertTrue(detailResult.has("data"));
                assertTrue(detailResult.has("content"));
            }
        }
    }

    @Test
    void shouldValidateSearchInputParameters() throws Exception {
        // Given: Various invalid search parameters
        
        // Test invalid limit
        Map<String, Object> invalidLimitRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "limit", 1000  // Too large
                )
            ),
            "id", "validation-limit"
        );
        
        JsonNode invalidLimitNode = objectMapper.valueToTree(invalidLimitRequest);
        Map<String, Object> limitResponseMap = messageHandler.handleMessage(invalidLimitNode, null);
        JsonNode limitResponse = objectMapper.valueToTree(limitResponseMap);
        
        // Should handle large limits gracefully (cap at maximum)
        assertEquals("2.0", limitResponse.get("jsonrpc").asText());
        assertEquals("validation-limit", limitResponse.get("id").asText());
        assertTrue(limitResponse.has("result"));
        
        // Test negative page
        Map<String, Object> invalidPageRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "John",
                    "page", -1  // Invalid page
                )
            ),
            "id", "validation-page"
        );
        
        JsonNode invalidPageNode = objectMapper.valueToTree(invalidPageRequest);
        Map<String, Object> pageResponseMap = messageHandler.handleMessage(invalidPageNode, null);
        JsonNode pageResponse = objectMapper.valueToTree(pageResponseMap);
        
        // Should handle invalid page gracefully (default to page 1)
        assertEquals("2.0", pageResponse.get("jsonrpc").asText());
        assertEquals("validation-page", pageResponse.get("id").asText());
        assertTrue(pageResponse.has("result"));
    }

    @Test
    void shouldFormatResponseForClaudeDesktop() throws Exception {
        // Given: Contact search request
        
        // When: Execute search operation
        Map<String, Object> searchRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_search",
                "arguments", Map.of(
                    "query", "Test",
                    "limit", 5
                )
            ),
            "id", "format-test"
        );
        
        JsonNode searchRequestNode = objectMapper.valueToTree(searchRequest);
        Map<String, Object> searchResponseMap = messageHandler.handleMessage(searchRequestNode, null);
        JsonNode searchResponse = objectMapper.valueToTree(searchResponseMap);
        
        // Then: Response should be properly formatted for Claude Desktop
        assertTrue(searchResponse.has("result"));
        JsonNode result = searchResponse.get("result");
        
        // Must have content field for Claude Desktop visibility
        assertTrue(result.has("content"));
        JsonNode content = result.get("content");
        assertTrue(content.isArray());
        assertTrue(content.size() > 0);
        
        // Content should be text type with non-empty text
        JsonNode firstContent = content.get(0);
        assertEquals("text", firstContent.get("type").asText());
        assertTrue(firstContent.has("text"));
        assertTrue(firstContent.get("text").asText().length() > 0);
        
        // Must have data field for programmatic access
        assertTrue(result.has("data"));
        assertTrue(result.get("data").isArray());
        
        // May have meta field for pagination information
        if (result.has("meta")) {
            assertTrue(result.get("meta").isObject());
        }
    }
}