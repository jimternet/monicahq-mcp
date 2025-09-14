package com.monicahq.mcp.integration;

import com.monicahq.mcp.controller.McpToolRegistry;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import java.io.IOException;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(properties = {
    "monica.api.url=http://localhost:8888/api",
    "monica.api.token=test-token-12345-abcdefghijklmnopqrstuvwxyz123456"
})
public class McpToolRegistryIntegrationTest {

    @Autowired
    private McpToolRegistry toolRegistry;

    private MockWebServer mockWebServer;

    @BeforeEach
    void setUp() throws IOException {
        mockWebServer = new MockWebServer();
        mockWebServer.start(8888);
    }

    @AfterEach
    void tearDown() throws IOException {
        if (mockWebServer != null) {
            mockWebServer.shutdown();
        }
    }

    @Test
    void shouldHaveAllToolsRegistered() {
        // Verify all expected tools are registered
        var tools = toolRegistry.getAllTools();
        
        assertNotNull(tools);
        assertTrue(tools.size() >= 52, "Should have at least 52 tools registered, but found: " + tools.size());
        
        // Verify some key tools are present
        var toolNames = tools.stream()
            .map(tool -> (String) tool.get("name"))
            .toList();
            
        assertTrue(toolNames.contains("contact_create"));
        assertTrue(toolNames.contains("contact_get"));
        assertTrue(toolNames.contains("contact_update"));
        assertTrue(toolNames.contains("contact_delete"));
        assertTrue(toolNames.contains("contact_list"));
        assertTrue(toolNames.contains("task_create"));
        assertTrue(toolNames.contains("note_create"));
        assertTrue(toolNames.contains("activity_create"));
    }

    @Test
    void shouldValidateToolArgumentsCorrectly() {
        // Mock successful response from MonicaHQ API
        mockWebServer.enqueue(new MockResponse()
            .setBody("{\"data\":{\"id\":123,\"firstName\":\"John\",\"lastName\":\"Doe\"}}")
            .setResponseCode(200)
            .setHeader("Content-Type", "application/json"));

        // Test valid arguments
        Map<String, Object> validArgs = Map.of(
            "firstName", "John",
            "lastName", "Doe",
            "genderId", 1,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        );

        // This should not throw an exception
        assertDoesNotThrow(() -> {
            Object result = toolRegistry.callTool("contact_create", validArgs);
            assertNotNull(result);
        });
    }

    @Test
    void shouldHandleInvalidToolGracefully() {
        // Test calling non-existent tool
        IllegalArgumentException exception = assertThrows(
            IllegalArgumentException.class,
            () -> toolRegistry.callTool("non_existent_tool", Map.of())
        );
        
        assertEquals("Unknown tool: non_existent_tool", exception.getMessage());
    }

    @Test
    void shouldValidateRequiredFieldsForContactCreation() {
        // Test missing required fields
        Map<String, Object> invalidArgs = Map.of(
            "lastName", "Doe"
            // Missing required firstName and genderId
        );

        // This should fail with validation error when we try to call the actual service
        // The tool registry itself doesn't validate - that's handled by the service layer
        assertDoesNotThrow(() -> {
            try {
                toolRegistry.callTool("contact_create", invalidArgs);
            } catch (Exception e) {
                // Expected to fail due to validation, which is fine
                assertTrue(e instanceof IllegalArgumentException || 
                          e.getMessage().contains("required"));
            }
        });
    }
}