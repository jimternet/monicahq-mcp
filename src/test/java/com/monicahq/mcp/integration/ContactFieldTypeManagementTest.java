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
 * Integration test for contact field type management workflow.
 * Tests the complete lifecycle: create → get → update → delete.
 * Part of Phase 2: Contact Field Type Management validation.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ContactFieldTypeManagementTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompleteContactFieldTypeLifecycle() {
        // Test 1: Create custom contact field type
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contactfieldtype_create",
                "arguments", Map.of(
                    "name", "LinkedIn Profile",
                    "protocol", "url"
                )
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Should create contact field type successfully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);
        assertEquals("LinkedIn Profile", data.get("name"));
        assertEquals("url", data.get("protocol"));

        // Test 2: Get the created contact field type
        Object fieldTypeId = data.get("id");
        Map<String, Object> getRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contactfieldtype_get",
                "arguments", Map.of("id", fieldTypeId)
            ),
            "id", 2
        );

        requestNode = objectMapper.valueToTree(getRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should retrieve contact field type successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> getResult = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> getData = (Map<String, Object>) getResult.get("data");
        assertEquals("LinkedIn Profile", getData.get("name"));
        assertEquals("url", getData.get("protocol"));

        // Test 3: Update contact field type
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contactfieldtype_update",
                "arguments", Map.of(
                    "id", fieldTypeId,
                    "name", "LinkedIn URL (Updated)",
                    "protocol", "url"
                )
            ),
            "id", 3
        );

        requestNode = objectMapper.valueToTree(updateRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should update contact field type successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));

        @SuppressWarnings("unchecked")
        Map<String, Object> updateResult = (Map<String, Object>) response.get("result");
        @SuppressWarnings("unchecked")
        Map<String, Object> updateData = (Map<String, Object>) updateResult.get("data");
        assertEquals("LinkedIn URL (Updated)", updateData.get("name"));

        // Test 4: Delete contact field type
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contactfieldtype_delete",
                "arguments", Map.of("id", fieldTypeId)
            ),
            "id", 4
        );

        requestNode = objectMapper.valueToTree(deleteRequest);
        response = messageHandler.handleMessage(requestNode, null);

        // Should delete successfully
        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}
