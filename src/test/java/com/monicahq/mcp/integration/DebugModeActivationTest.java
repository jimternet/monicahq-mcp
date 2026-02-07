package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.monicahq.mcp.config.StdioMcpTestClient;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for debug mode activation and logging.
 * Tests debug logging to stderr only without contaminating stdout.
 *
 * FUTURE ENHANCEMENT: Debug mode functionality not yet implemented.
 *
 * These tests define the specification for future debug mode features:
 * - Environment variable activation
 * - MCP capability negotiation for debug mode
 * - Enhanced logging for tool calls, errors, and complex operations
 * - Sensitive data redaction in debug logs
 * - Verbosity level control
 * - Performance validation
 *
 * Tests are disabled until debug mode implementation is scheduled.
 * This is TDD "RED phase" - tests written first, implementation follows later.
 */
@Disabled("Debug mode functionality not yet implemented - future enhancement")
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class DebugModeActivationTest {

    private StdioMcpTestClient mcpClient;
    private ByteArrayOutputStream stderrCapture;
    private PrintStream originalStderr;

    @BeforeEach
    void setUp() throws Exception {
        mcpClient = new StdioMcpTestClient();
        
        // Capture stderr for debug log verification
        stderrCapture = new ByteArrayOutputStream();
        originalStderr = System.err;
        System.setErr(new PrintStream(stderrCapture));
    }

    @AfterEach
    void tearDown() throws Exception {
        // Restore original stderr
        if (originalStderr != null) {
            System.setErr(originalStderr);
        }
        
        if (mcpClient != null) {
            mcpClient.teardown();
        }
    }

    @Test
    void shouldActivateDebugModeViaEnvironmentVariable() throws Exception {
        // Given: Debug mode activated via environment variable
        // This will FAIL because debug mode environment handling isn't implemented
        mcpClient.setup();
        
        // When: Initialize with debug mode environment
        JsonNode initResponse = mcpClient.initialize();
        
        // Then: Should accept initialization
        assertNotNull(initResponse);
        assertEquals("2.0", initResponse.get("jsonrpc").asText());
        assertTrue(initResponse.has("result"));
        
        // And: Should produce debug logs on stderr
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") || stderrOutput.contains("debug"), 
            "Debug mode should produce debug logs on stderr");
        
        // And: Should not contaminate stdout with debug logs
        assertFalse(initResponse.toString().contains("DEBUG"), 
            "Stdout response should not contain debug logs");
    }

    @Test
    void shouldActivateDebugModeViaInitializeCapability() throws Exception {
        // Given: MCP client requesting debug mode via capabilities
        mcpClient.setup();
        
        Map<String, Object> debugInitRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(
                    "debug", Map.of("enabled", true) // Request debug mode
                ),
                "clientInfo", Map.of(
                    "name", "debug-test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        // When: Initialize with debug capability request
        // This will FAIL because debug capability handling isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(debugInitRequest);
        
        // Then: Should accept debug mode request
        assertNotNull(response);
        assertTrue(response.has("result"));
        
        JsonNode result = response.get("result");
        assertTrue(result.has("capabilities"));
        
        // Should confirm debug mode is enabled
        JsonNode capabilities = result.get("capabilities");
        if (capabilities.has("debug")) {
            JsonNode debug = capabilities.get("debug");
            assertTrue(debug.get("enabled").asBoolean(), 
                "Server should confirm debug mode is enabled");
        }
        
        // And: Should produce enhanced debug logs
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") && stderrOutput.contains("initialize"), 
            "Should have debug logs for initialization");
    }

    @Test
    void shouldProduceDebugLogsForToolCalls() throws Exception {
        // Given: Debug mode enabled and successful initialization
        mcpClient.setup();
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse.get("result"));
        
        // When: Call a tool with debug mode active
        Map<String, Object> contactArgs = Map.of(
            "firstName", "Debug",
            "lastName", "Test",
            "genderId", 1,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        );
        
        // This will FAIL because debug logging for tool calls isn't implemented
        JsonNode toolResponse = mcpClient.callTool("contact_create", contactArgs, 2);
        
        // Then: Should execute tool successfully
        assertNotNull(toolResponse);
        assertTrue(toolResponse.has("result"));
        
        // And: Should produce debug logs for tool execution
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") && stderrOutput.contains("contact_create"), 
            "Should have debug logs for tool call");
        assertTrue(stderrOutput.contains("arguments") || stderrOutput.contains("parameters"), 
            "Debug logs should include parameter information");
        
        // And: Should log tool execution timing
        assertTrue(stderrOutput.contains("duration") || stderrOutput.contains("elapsed") || 
                  stderrOutput.contains("ms") || stderrOutput.contains("time"), 
            "Debug logs should include timing information");
    }

    @Test
    void shouldLogDebugInfoForFailedOperations() throws Exception {
        // Given: Debug mode enabled
        mcpClient.setup();
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse.get("result"));
        
        // When: Call tool with invalid arguments to trigger failure
        Map<String, Object> invalidArgs = Map.of(
            "invalidField", "should fail"
            // Missing required fields
        );
        
        // This will FAIL because debug logging for errors isn't implemented
        JsonNode errorResponse = mcpClient.callTool("contact_create", invalidArgs, 3);
        
        // Then: Should return error response
        assertNotNull(errorResponse);
        assertTrue(errorResponse.has("error"));
        
        // And: Should produce detailed debug logs for the failure
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") && stderrOutput.contains("error"), 
            "Should have debug logs for error");
        assertTrue(stderrOutput.contains("validation") || stderrOutput.contains("invalid"), 
            "Debug logs should explain the validation failure");
        assertTrue(stderrOutput.contains("contact_create"), 
            "Debug logs should identify the failing operation");
    }

    @Test
    void shouldProvideDetailedDebugInfoForComplexOperations() throws Exception {
        // Given: Debug mode enabled
        mcpClient.setup();
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse.get("result"));
        
        // When: Perform complex operation with attendees
        Map<String, Object> activityArgs = Map.of(
            "contactId", 12345,
            "activityTypeId", 1,
            "summary", "Debug Test Activity",
            "description", "Testing debug logging for complex operations",
            "happenedAt", "2025-09-13T10:30:00Z",
            "attendees", List.of(
                "John Doe",
                Map.of("contactId", 101),
                "Jane Smith"
            )
        );
        
        // This will FAIL because detailed debug logging isn't implemented
        JsonNode activityResponse = mcpClient.callTool("activity_create", activityArgs, 4);
        
        // Then: Should execute operation
        assertNotNull(activityResponse);
        
        // And: Should provide detailed debug information
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") && stderrOutput.contains("activity_create"), 
            "Should have debug logs for activity creation");
        assertTrue(stderrOutput.contains("attendees") && stderrOutput.contains("processing"), 
            "Should log attendees processing details");
        assertTrue(stderrOutput.contains("API") || stderrOutput.contains("request"), 
            "Should log API interaction details");
    }

    @Test
    void shouldNotLogSensitiveInformationInDebugMode() throws Exception {
        // Given: Debug mode enabled
        mcpClient.setup();
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse.get("result"));
        
        // When: Perform operation with potentially sensitive data
        Map<String, Object> contactArgs = Map.of(
            "firstName", "Sensitive",
            "lastName", "Information", 
            "email", "secret@company.com",
            "genderId", 1,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        );
        
        // This will FAIL because sensitive data filtering isn't implemented
        JsonNode contactResponse = mcpClient.callTool("contact_create", contactArgs, 5);
        
        // Then: Should execute operation
        assertNotNull(contactResponse);
        
        // And: Should log operation but filter sensitive data
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("DEBUG") && stderrOutput.contains("contact_create"), 
            "Should have debug logs for contact creation");
        
        // Should not expose sensitive email in logs
        assertFalse(stderrOutput.contains("secret@company.com"), 
            "Debug logs should not contain sensitive email addresses");
        
        // Should use sanitized representations
        assertTrue(stderrOutput.contains("***") || stderrOutput.contains("[REDACTED]") || 
                  stderrOutput.contains("[FILTERED]"), 
            "Debug logs should show sanitized representations of sensitive data");
    }

    @Test
    void shouldControlDebugVerbosityLevels() throws Exception {
        // Given: Debug mode with specific verbosity level
        mcpClient.setup();
        
        Map<String, Object> verboseInitRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(
                    "debug", Map.of(
                        "enabled", true,
                        "level", "TRACE" // Maximum verbosity
                    )
                ),
                "clientInfo", Map.of(
                    "name", "verbose-debug-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        // When: Initialize with high verbosity debug mode
        // This will FAIL because debug verbosity levels aren't implemented
        JsonNode response = mcpClient.sendMcpRequest(verboseInitRequest);
        
        // Then: Should accept verbosity level
        assertNotNull(response);
        assertTrue(response.has("result"));
        
        // When: Perform operation with high verbosity logging
        Map<String, Object> simpleArgs = Map.of(
            "firstName", "Verbose",
            "genderId", 1,
            "isBirthdateKnown", false,
            "isDeceased", false,
            "isDeceasedDateKnown", false
        );
        
        JsonNode verboseResponse = mcpClient.callTool("contact_create", simpleArgs, 2);
        assertNotNull(verboseResponse);
        
        // Then: Should produce very detailed logs
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("TRACE") || stderrOutput.contains("DEBUG"), 
            "Should have trace/debug level logs");
        assertTrue(stderrOutput.contains("validation") && stderrOutput.contains("mapping"), 
            "Verbose mode should log internal processing steps");
        assertTrue(stderrOutput.contains("HTTP") || stderrOutput.contains("request") || 
                  stderrOutput.contains("response"), 
            "Verbose mode should log HTTP interaction details");
    }

    @Test
    void shouldMaintainPerformanceInDebugMode() throws Exception {
        // Given: Debug mode enabled
        mcpClient.setup();
        JsonNode initResponse = mcpClient.initialize();
        assertNotNull(initResponse.get("result"));
        
        // When: Perform multiple operations to test performance impact
        long startTime = System.currentTimeMillis();
        
        for (int i = 1; i <= 5; i++) {
            Map<String, Object> contactArgs = Map.of(
                "firstName", "Performance" + i,
                "genderId", 1,
                "isBirthdateKnown", false,
                "isDeceased", false,
                "isDeceasedDateKnown", false
            );
            
            JsonNode response = mcpClient.callTool("contact_create", contactArgs, i + 10);
            assertNotNull(response);
        }
        
        long duration = System.currentTimeMillis() - startTime;
        
        // Then: Should maintain reasonable performance even with debug logging
        // This will FAIL because debug performance optimization isn't implemented
        assertTrue(duration < 10000, 
            "Debug mode should not severely impact performance (took " + duration + "ms)");
        
        // And: Should still produce debug logs for all operations
        String stderrOutput = mcpClient.getStderrOutput();
        assertTrue(stderrOutput.contains("Performance1") && stderrOutput.contains("Performance5"), 
            "Should have debug logs for all operations");
    }
}