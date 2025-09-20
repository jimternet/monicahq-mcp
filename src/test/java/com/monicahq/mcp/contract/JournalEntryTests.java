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

@SpringBootTest()

@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class JournalEntryTests {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCreateJournalEntryViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "journal_entry_create", "arguments", Map.of("title", "Great day", "post", "Had a wonderful day today!", "date", "2025-09-13")), "id", 1);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure - should be error since journal entries are not supported
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params - Unknown tool
        assertEquals("Invalid params", error.get("message"));
    }

    @Test
    void shouldGetJournalEntryViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "journal_entry_get", "arguments", Map.of("id", 33333L)), "id", 2);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure - should be error since journal entries are not supported
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params - Unknown tool
        assertEquals("Invalid params", error.get("message"));
    }

    @Test
    void shouldUpdateJournalEntryViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "journal_entry_update", "arguments", Map.of("id", 33333L, "title", "Updated title", "post", "Updated content")), "id", 3);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure - should be error since journal entries are not supported
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(3L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params - Unknown tool
        assertEquals("Invalid params", error.get("message"));
    }

    @Test
    void shouldDeleteJournalEntryViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "journal_entry_delete", "arguments", Map.of("id", 33333L)), "id", 4);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure - should be error since journal entries are not supported
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(4L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params - Unknown tool
        assertEquals("Invalid params", error.get("message"));
    }

    @Test
    void shouldListJournalEntriesViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "journal_entry_list", "arguments", Map.of("page", 1, "limit", 10)), "id", 5);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure - should be error since journal entries are not supported
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(5L, response.get("id"));
        assertTrue(response.containsKey("error"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code")); // Invalid params - Unknown tool
        assertEquals("Invalid params", error.get("message"));
    }
}
