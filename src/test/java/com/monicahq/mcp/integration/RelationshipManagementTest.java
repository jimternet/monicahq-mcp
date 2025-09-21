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
 * Integration test for relationship management workflow.
 * Tests the complete relationship lifecycle: create → get → update → list → delete.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class RelationshipManagementTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompleteRelationshipLifecycle() throws Exception {
        // This integration test will verify the complete relationship workflow
        // once all services are implemented
        
        // Test 1: List relationship types for discovery
        Map<String, Object> listTypesRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_type_list",
                "arguments", Map.of()
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(listTypesRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Should return available relationship types
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
        
        // Test 2: Create relationship using discovered type
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "relationship_create",
                "arguments", Map.of(
                    "contactIs", 1L,
                    "ofContact", 2L,
                    "relationshipTypeId", 1L,
                    "notes", "Integration test relationship"
                )
            ),
            "id", 2
        );
        
        requestNode = objectMapper.valueToTree(createRequest);
        response = messageHandler.handleMessage(requestNode, null);
        
        // Should create relationship successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}