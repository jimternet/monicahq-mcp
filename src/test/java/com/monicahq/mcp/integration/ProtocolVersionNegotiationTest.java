package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.monicahq.mcp.config.StdioMcpTestClient;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test for MCP protocol version negotiation.
 * Tests protocol version handling and compatibility.
 * 
 * This test MUST FAIL initially (RED phase of TDD).
 * Protocol version negotiation isn't implemented yet.
 */
@SpringBootTest()
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class ProtocolVersionNegotiationTest {

    private StdioMcpTestClient mcpClient;

    @BeforeEach
    void setUp() throws Exception {
        mcpClient = new StdioMcpTestClient();
    }

    @AfterEach
    void tearDown() throws Exception {
        if (mcpClient != null) {
            mcpClient.teardown();
        }
    }

    @Test
    void shouldNegotiateValidProtocolVersion() throws Exception {
        // Given: MCP client with valid protocol version
        mcpClient.setup();
        
        // When: Initialize with supported protocol version
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(
                    "roots", Map.of("listChanged", true),
                    "sampling", Map.of()
                ),
                "clientInfo", Map.of(
                    "name", "test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 1
        );
        
        // This will FAIL because protocol version negotiation isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should accept valid protocol version
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(1, response.get("id").asInt());
        assertTrue(response.has("result"), "Should have result for valid protocol version");
        
        JsonNode result = response.get("result");
        assertTrue(result.has("protocolVersion"), "Should include protocol version in response");
        assertTrue(result.has("serverInfo"), "Should include server info");
        assertTrue(result.has("capabilities"), "Should include server capabilities");
        
        // Verify server reports supported protocol version
        String serverProtocolVersion = result.get("protocolVersion").asText();
        assertTrue(serverProtocolVersion.matches("\\d{4}-\\d{2}-\\d{2}"), 
            "Protocol version should be in YYYY-MM-DD format");
    }

    @Test
    void shouldRejectUnsupportedProtocolVersion() throws Exception {
        // Given: MCP client with unsupported protocol version
        mcpClient.setup();
        
        // When: Initialize with unsupported protocol version
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2020-01-01", // Very old version
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 2
        );
        
        // This will FAIL because version rejection isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should reject with proper error
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(2, response.get("id").asInt());
        assertTrue(response.has("error"), "Should have error for unsupported version");
        
        JsonNode error = response.get("error");
        assertEquals(-32602, error.get("code").asInt()); // Invalid params
        
        String errorMessage = error.get("message").asText();
        assertTrue(errorMessage.contains("protocol") && errorMessage.contains("version"), 
            "Error should mention protocol version issue");
        assertTrue(errorMessage.contains("unsupported") || errorMessage.contains("supported"), 
            "Error should indicate version is unsupported");
    }

    @Test
    void shouldHandleFutureProtocolVersion() throws Exception {
        // Given: MCP client with future protocol version
        mcpClient.setup();
        
        // When: Initialize with future protocol version
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2030-12-31", // Future version
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "future-client",
                    "version", "2.0.0"
                )
            ),
            "id", 3
        );
        
        // This will FAIL because future version handling isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should either accept with compatibility or reject gracefully
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(3, response.get("id").asInt());
        
        if (response.has("result")) {
            // If accepted, should downgrade to supported version
            JsonNode result = response.get("result");
            String serverVersion = result.get("protocolVersion").asText();
            assertTrue(serverVersion.compareTo("2030-12-31") <= 0, 
                "Server should not claim to support future version");
        } else {
            // If rejected, should provide clear error
            assertTrue(response.has("error"));
            JsonNode error = response.get("error");
            String errorMessage = error.get("message").asText();
            assertTrue(errorMessage.contains("version") && 
                      (errorMessage.contains("future") || errorMessage.contains("supported")), 
                "Error should explain version compatibility issue");
        }
    }

    @Test
    void shouldHandleMissingProtocolVersion() throws Exception {
        // Given: MCP client without protocol version
        mcpClient.setup();
        
        // When: Initialize without protocol version parameter
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "no-version-client",
                    "version", "1.0.0"
                )
                // Missing protocolVersion
            ),
            "id", 4
        );
        
        // This will FAIL because missing version handling isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should either use default version or reject
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(4, response.get("id").asInt());
        
        if (response.has("result")) {
            // If accepted with default, should specify version used
            JsonNode result = response.get("result");
            assertTrue(result.has("protocolVersion"), 
                "Should specify protocol version when defaulting");
        } else {
            // If rejected, should explain requirement
            assertTrue(response.has("error"));
            JsonNode error = response.get("error");
            String errorMessage = error.get("message").asText();
            assertTrue(errorMessage.contains("protocolVersion") && errorMessage.contains("required"), 
                "Error should explain protocolVersion is required");
        }
    }

    @Test
    void shouldProvideVersionCompatibilityInfo() throws Exception {
        // Given: Successful protocol negotiation
        mcpClient.setup();
        
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "compatibility-test-client",
                    "version", "1.0.0"
                )
            ),
            "id", 5
        );
        
        // When: Initialize successfully
        // This will FAIL because version compatibility info isn't provided
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should provide compatibility information
        assertNotNull(response);
        assertTrue(response.has("result"));
        
        JsonNode result = response.get("result");
        assertTrue(result.has("serverInfo"), "Should include server info");
        
        JsonNode serverInfo = result.get("serverInfo");
        assertTrue(serverInfo.has("name"), "Server info should include name");
        assertTrue(serverInfo.has("version"), "Server info should include version");
        
        // Should provide capability information for version compatibility
        assertTrue(result.has("capabilities"), "Should include server capabilities");
        JsonNode capabilities = result.get("capabilities");
        assertTrue(capabilities.isObject(), "Capabilities should be object");
        
        // Should indicate which MCP features are supported
        if (capabilities.has("tools")) {
            JsonNode tools = capabilities.get("tools");
            assertTrue(tools.isObject(), "Tools capability should be object");
        }
        
        if (capabilities.has("resources")) {
            JsonNode resources = capabilities.get("resources");
            assertTrue(resources.isObject(), "Resources capability should be object");
        }
    }

    @Test
    void shouldHandleInvalidProtocolVersionFormat() throws Exception {
        // Given: MCP client with malformed protocol version
        mcpClient.setup();
        
        // When: Initialize with invalid protocol version format
        Map<String, Object> initializeRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "invalid-format", // Should be YYYY-MM-DD
                "capabilities", Map.of(),
                "clientInfo", Map.of(
                    "name", "invalid-version-client",
                    "version", "1.0.0"
                )
            ),
            "id", 6
        );
        
        // This will FAIL because version format validation isn't implemented
        JsonNode response = mcpClient.sendMcpRequest(initializeRequest);
        
        // Then: Should reject with format error
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc").asText());
        assertEquals(6, response.get("id").asInt());
        assertTrue(response.has("error"), "Should have error for invalid format");
        
        JsonNode error = response.get("error");
        assertEquals(-32602, error.get("code").asInt()); // Invalid params
        
        String errorMessage = error.get("message").asText();
        assertTrue(errorMessage.contains("format") && errorMessage.contains("protocol"), 
            "Error should mention protocol version format issue");
        assertTrue(errorMessage.contains("YYYY-MM-DD") || errorMessage.contains("date"), 
            "Error should specify expected format");
    }

    @Test
    void shouldMaintainVersionConsistency() throws Exception {
        // Given: Multiple initialize requests with same version
        mcpClient.setup();
        
        Map<String, Object> firstRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(),
                "clientInfo", Map.of("name", "consistency-test", "version", "1.0.0")
            ),
            "id", 7
        );
        
        // When: Send multiple initialize requests
        // This will FAIL because version consistency isn't maintained
        JsonNode firstResponse = mcpClient.sendMcpRequest(firstRequest);
        
        // Reinitialize with same version
        Map<String, Object> secondRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "initialize",
            "params", Map.of(
                "protocolVersion", "2024-11-05",
                "capabilities", Map.of(),
                "clientInfo", Map.of("name", "consistency-test", "version", "1.0.0")
            ),
            "id", 8
        );
        
        JsonNode secondResponse = mcpClient.sendMcpRequest(secondRequest);
        
        // Then: Should provide consistent version information
        assertTrue(firstResponse.has("result"));
        assertTrue(secondResponse.has("result"));
        
        String firstVersion = firstResponse.get("result").get("protocolVersion").asText();
        String secondVersion = secondResponse.get("result").get("protocolVersion").asText();
        
        assertEquals(firstVersion, secondVersion, 
            "Protocol version should be consistent across multiple initializations");
    }
}