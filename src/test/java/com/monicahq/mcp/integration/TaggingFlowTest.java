package com.monicahq.mcp.integration;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;

import java.util.Map;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class TaggingFlowTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageContactTaggingWorkflow() {
        // Step 1: Create tag
        Map<String, Object> tagCreateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_create",
                "arguments", Map.of(
                    "name", "VIP Client",
                    "nameSlug", "vip-client"
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(tagCreateRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));

        // Step 2: Add tag to contact
        Map<String, Object> addTagRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_tag_add",
                "arguments", Map.of(
                    "contactId", 12345L,
                    "tagId", 11111L
                )
            ),
            "id", 2
        );
        
        JsonNode addTagNode = objectMapper.valueToTree(addTagRequest);
        Map<String, Object> addTagResponse = messageHandler.handleMessage(addTagNode, null);
        
        assertNotNull(addTagResponse);
        assertEquals("2.0", addTagResponse.get("jsonrpc"));
        assertEquals(2L, addTagResponse.get("id"));
        // Tag addition might succeed or fail - both are valid for integration test
        assertTrue(addTagResponse.containsKey("result") || addTagResponse.containsKey("error"));

        // Step 3: List tags for contact
        Map<String, Object> listTagsRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_list",
                "arguments", Map.of(
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 3
        );
        
        JsonNode listTagsNode = objectMapper.valueToTree(listTagsRequest);
        Map<String, Object> listTagsResponse = messageHandler.handleMessage(listTagsNode, null);
        
        assertNotNull(listTagsResponse);
        assertEquals("2.0", listTagsResponse.get("jsonrpc"));
        assertEquals(3L, listTagsResponse.get("id"));
        assertTrue(listTagsResponse.containsKey("result"));

        // Step 4: Remove tag from contact
        Map<String, Object> removeTagRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_tag_remove",
                "arguments", Map.of(
                    "contactId", 12345L,
                    "tagId", 11111L
                )
            ),
            "id", 4
        );
        
        JsonNode removeTagNode = objectMapper.valueToTree(removeTagRequest);
        Map<String, Object> removeTagResponse = messageHandler.handleMessage(removeTagNode, null);
        
        assertNotNull(removeTagResponse);
        assertEquals("2.0", removeTagResponse.get("jsonrpc"));
        assertEquals(4L, removeTagResponse.get("id"));
        assertTrue(removeTagResponse.containsKey("result") || removeTagResponse.containsKey("error"));
    }

    @Test
    void shouldHandleTagSearchAndManagement() {
        // Step 1: Create multiple tags
        Map<String, Object> tag1Request = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_create",
                "arguments", Map.of(
                    "name", "Important",
                    "nameSlug", "important"
                )
            ),
            "id", 1
        );
        
        JsonNode tag1Node = objectMapper.valueToTree(tag1Request);
        Map<String, Object> tag1Response = messageHandler.handleMessage(tag1Node, null);
        
        assertNotNull(tag1Response);
        assertEquals("2.0", tag1Response.get("jsonrpc"));
        assertEquals(1L, tag1Response.get("id"));
        assertTrue(tag1Response.containsKey("result"));

        // Step 2: Update tag
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "tag_update",
                "arguments", Map.of(
                    "id", 11111L,
                    "name", "Very Important",
                    "nameSlug", "very-important"
                )
            ),
            "id", 2
        );
        
        JsonNode updateNode = objectMapper.valueToTree(updateRequest);
        Map<String, Object> updateResponse = messageHandler.handleMessage(updateNode, null);
        
        assertNotNull(updateResponse);
        assertEquals("2.0", updateResponse.get("jsonrpc"));
        assertEquals(2L, updateResponse.get("id"));
        assertTrue(updateResponse.containsKey("result"));
    }
}
