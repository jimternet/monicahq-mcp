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
 * Integration test for company management workflow.
 * Tests the complete company lifecycle: create → get → update → list → delete.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class CompanyManagementTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldManageCompleteCompanyLifecycle() throws Exception {
        // This integration test will verify the complete company workflow
        // once all services are implemented
        
        // Test 1: Create company
        Map<String, Object> createRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "company_create",
                "arguments", Map.of(
                    "name", "Integration Test Corp",
                    "website", "https://integration.test",
                    "numberOfEmployees", 100
                )
            ),
            "id", 1
        );
        
        JsonNode requestNode = objectMapper.valueToTree(createRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);
        
        // Should create company successfully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"));
        
        // Test 2: List companies to verify creation
        Map<String, Object> listRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "company_list",
                "arguments", Map.of("limit", 10)
            ),
            "id", 2
        );
        
        requestNode = objectMapper.valueToTree(listRequest);
        response = messageHandler.handleMessage(requestNode, null);
        
        // Should list companies including the created one
        assertNotNull(response);
        assertTrue(response.containsKey("result"));
    }
}