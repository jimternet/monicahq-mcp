package com.monicahq.mcp.config;

import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Base test class for MCP stdio tests.
 * Provides common setup, teardown, and utility methods.
 */
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public abstract class StdioMcpBaseTest {

    protected StdioMcpTestClient mcpClient;
    
    @BeforeEach
    void setupMcpClient() throws Exception {
        mcpClient = new StdioMcpTestClient();
        mcpClient.setup();
        
        // Initialize MCP connection
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse);
        assertEquals("2.0", initResponse.get("jsonrpc").asText());
        assertNotNull(initResponse.get("result"));
    }
    
    @AfterEach
    void teardownMcpClient() throws Exception {
        if (mcpClient != null) {
            mcpClient.teardown();
        }
    }
    
    /**
     * Helper method to call a tool and assert success
     */
    protected JsonNode callToolAndAssertSuccess(String toolName, Map<String, Object> arguments, int requestId) throws Exception {
        JsonNode response = mcpClient.callTool(toolName, arguments, requestId);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(requestId, response.get("id").asInt());
        
        if (response.has("error")) {
            fail("Tool call failed with error: " + response.get("error"));
        }
        
        assertTrue(response.has("result"), "Response should have result field");
        return response.get("result");
    }
    
    /**
     * Helper method to call a tool and expect an error
     */
    protected JsonNode callToolAndExpectError(String toolName, Map<String, Object> arguments, int requestId) throws Exception {
        JsonNode response = mcpClient.callTool(toolName, arguments, requestId);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(requestId, response.get("id").asInt());
        
        assertTrue(response.has("error"), "Response should have error field");
        return response.get("error");
    }
    
    /**
     * Helper method to list tools and verify they exist
     */
    protected JsonNode listToolsAndAssertAvailable(String... expectedTools) throws Exception {
        JsonNode response = mcpClient.listTools(100);
        
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertTrue(response.has("result"));
        
        JsonNode result = response.get("result");
        assertTrue(result.has("tools"));
        
        JsonNode tools = result.get("tools");
        assertTrue(tools.isArray());
        
        // Check that expected tools are present
        for (String expectedTool : expectedTools) {
            boolean found = false;
            for (JsonNode tool : tools) {
                if (expectedTool.equals(tool.get("name").asText())) {
                    found = true;
                    break;
                }
            }
            assertTrue(found, "Expected tool not found: " + expectedTool);
        }
        
        return result;
    }
    
    /**
     * Helper method to assert JSON structure
     */
    protected void assertJsonStructure(JsonNode node, String path, String expectedType) {
        String[] parts = path.split("\\.");
        JsonNode current = node;
        
        for (String part : parts) {
            assertTrue(current.has(part), "Missing field: " + part + " in path: " + path);
            current = current.get(part);
        }
        
        switch (expectedType.toLowerCase()) {
            case "string":
                assertTrue(current.isTextual(), "Expected string at path: " + path);
                break;
            case "number":
            case "integer":
                assertTrue(current.isNumber(), "Expected number at path: " + path);
                break;
            case "boolean":
                assertTrue(current.isBoolean(), "Expected boolean at path: " + path);
                break;
            case "array":
                assertTrue(current.isArray(), "Expected array at path: " + path);
                break;
            case "object":
                assertTrue(current.isObject(), "Expected object at path: " + path);
                break;
        }
    }
}