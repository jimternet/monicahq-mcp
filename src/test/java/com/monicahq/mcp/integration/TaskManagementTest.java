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
public class TaskManagementTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageTaskLifecycleWorkflow() {
        // Step 1: Create task
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_create",
                "arguments", Map.of(
                    "contactId", 12345L,
                    "title", "Call about project update",
                    "description", "Discuss Q4 milestones",
                    "completed", false
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> taskData = (Map<String, Object>) result.get("data");
        assertNotNull(taskData.get("id"));

        // Step 2: Update task as completed
        Map<String, Object> updateRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_update",
                "arguments", Map.of(
                    "id", 78901L,
                    "title", "Call about project update - DONE",
                    "completed", true
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
        
        @SuppressWarnings("unchecked")
        Map<String, Object> updateResult = (Map<String, Object>) updateResponse.get("result");
        assertTrue(updateResult.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> updatedTaskData = (Map<String, Object>) updateResult.get("data");
        assertEquals(true, updatedTaskData.get("completed"));

        // Step 3: List completed tasks
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_list",
                "arguments", Map.of(
                    "completed", true,
                    "page", 1,
                    "limit", 10
                )
            ),
            "id", 3
        );
        
        JsonNode listNode = objectMapper.valueToTree(listRequest);
        Map<String, Object> listResponse = messageHandler.handleMessage(listNode, null);
        
        assertNotNull(listResponse);
        assertEquals("2.0", listResponse.get("jsonrpc"));
        assertEquals(3L, listResponse.get("id"));
        assertTrue(listResponse.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> listResult = (Map<String, Object>) listResponse.get("result");
        assertTrue(listResult.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> tasks = (List<Map<String, Object>>) listResult.get("data");
        assertNotNull(tasks);
        assertTrue(tasks instanceof List);

        // Step 4: Delete completed task
        Map<String, Object> deleteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_delete",
                "arguments", Map.of(
                    "id", 78901L
                )
            ),
            "id", 4
        );
        
        JsonNode deleteNode = objectMapper.valueToTree(deleteRequest);
        Map<String, Object> deleteResponse = messageHandler.handleMessage(deleteNode, null);
        
        assertNotNull(deleteResponse);
        assertEquals("2.0", deleteResponse.get("jsonrpc"));
        assertEquals(4L, deleteResponse.get("id"));
        assertTrue(deleteResponse.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> deleteResult = (Map<String, Object>) deleteResponse.get("result");
        assertTrue(deleteResult.containsKey("content"));
        
        @SuppressWarnings("unchecked")
        Object[] content = (Object[]) deleteResult.get("content");
        assertTrue(content.length > 0);
        Map<String, Object> firstContent = (Map<String, Object>) content[0];
        String text = (String) firstContent.get("text");
        assertTrue(text.contains("deleted") || text.contains("Delete"));
    }

    @Test
    void shouldValidateTaskRequiredFields() {
        Map<String, Object> invalidRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "task_create",
                "arguments", Map.of(
                    "description", "Missing required fields"
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(invalidRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"));
    }
}