package com.monicahq.mcp.config;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.controller.McpMessageHandler;
import com.monicahq.mcp.controller.McpToolRegistry;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Simple test to verify the Spring context loads correctly with test configuration.
 * This tests the core MCP components without stdio communication complexity.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class SimpleMcpTest {

    @Autowired
    private McpMessageHandler messageHandler;
    
    @Autowired  
    private McpToolRegistry toolRegistry;
    
    @Autowired
    private MonicaHqClient monicaHqClient;
    
    @Test
    void contextLoads() {
        assertNotNull(messageHandler);
        assertNotNull(toolRegistry); 
        assertNotNull(monicaHqClient);
        
        // Verify we're using the test client
        assertTrue(monicaHqClient instanceof TestMonicaHqClient);
    }
    
    @Test 
    void toolRegistryHasExpectedTools() {
        // This should work since TestMonicaHqClient is being used
        var tools = toolRegistry.getAllTools();
        
        assertNotNull(tools);
        assertTrue(tools.size() > 45, "Expected at least 45+ MCP tools (50 - 5 journal entries), got " + tools.size());
        
        // Check for some key tools
        assertTrue(tools.stream().anyMatch(tool -> "contact_list".equals(tool.get("name"))));
        assertTrue(tools.stream().anyMatch(tool -> "contact_create".equals(tool.get("name"))));
        assertTrue(tools.stream().anyMatch(tool -> "note_create".equals(tool.get("name"))));
    }
}