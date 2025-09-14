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

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
@ActiveProfiles("test")
public class ContactCreationTest {

    @Autowired
    private McpToolRegistry toolRegistry;

    private MockWebServer mockWebServer;

    @BeforeEach
    void setUp() throws Exception {
        mockWebServer = new MockWebServer();
        mockWebServer.start(8888);
    }

    @AfterEach
    void tearDown() throws Exception {
        if (mockWebServer != null) {
            mockWebServer.shutdown();
        }
    }

    @Test
    void shouldCreateContactWithOAuth2Authentication() {
        // Mock successful MonicaHQ API response
        mockWebServer.enqueue(new MockResponse()
            .setBody("{\"data\":{\"id\":12345,\"first_name\":\"John\",\"last_name\":\"Doe\",\"gender_id\":1}}")
            .setResponseCode(200)
            .setHeader("Content-Type", "application/json"));

        Map<String, Object> arguments = Map.of(
            "firstName", "John",
            "lastName", "Doe",
            "genderId", 1,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        );
        
        // Test the tool execution directly
        Object result = toolRegistry.callTool("contact_create", arguments);
        
        assertNotNull(result);
        assertTrue(result instanceof Map);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> resultMap = (Map<String, Object>) result;
        
        assertTrue(resultMap.containsKey("data"));
        
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) resultMap.get("data");
        assertEquals("John", data.get("firstName"));
        assertEquals("Doe", data.get("lastName"));
    }

    @Test
    void shouldValidateRequiredFields() {
        Map<String, Object> invalidArguments = Map.of(
            "lastName", "Smith"
            // Missing required firstName and genderId
        );
        
        // This should throw an IllegalArgumentException due to validation
        IllegalArgumentException exception = assertThrows(
            IllegalArgumentException.class,
            () -> toolRegistry.callTool("contact_create", invalidArguments)
        );
        
        assertTrue(exception.getMessage().contains("firstName is required"));
    }

    @Test
    void shouldHandleMonicaApiUnavailable() {
        // Simulate API error using TestMonicaHqClient
        com.monicahq.mcp.config.TestMonicaHqClient.setSimulateApiError(true);
        
        try {
            Map<String, Object> arguments = Map.of(
                "firstName", "Test",
                "genderId", 1,
                "isBirthdateKnown", false,
                "isDeceased", false,
                "isDeceasedDateKnown", false
            );
            
            // This should throw an exception due to API error
            Exception exception = assertThrows(
                Exception.class,
                () -> toolRegistry.callTool("contact_create", arguments)
            );
            
            assertNotNull(exception);
        } finally {
            // Always clean up the error simulation
            com.monicahq.mcp.config.TestMonicaHqClient.setSimulateApiError(false);
        }
    }
}
