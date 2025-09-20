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
public class ContactFieldTests {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldCreateContactFieldViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contact_field_create", "arguments", Map.of("contactId", 12345L, "contactFieldTypeId", 1L, "data", "john@example.com")), "id", 1);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(1L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldGetContactFieldViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contact_field_get", "arguments", Map.of("id", 55555L)), "id", 2);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(2L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldUpdateContactFieldViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contact_field_update", "arguments", Map.of("id", 55555L, "data", "newemail@example.com")), "id", 3);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(3L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }

    @Test
    void shouldListContactFieldsViaMcpProtocol() throws Exception {
        Map<String, Object> toolsCallRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contact_field_list", "arguments", Map.of("contactId", 12345L, "page", 1, "limit", 10)), "id", 4);
        
        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Then: Verify response structure
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertEquals(4L, response.get("id"));
        assertTrue(response.containsKey("result"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> result = (Map<String, Object>) response.get("result");
        assertTrue(result.containsKey("data"));
    }
}
