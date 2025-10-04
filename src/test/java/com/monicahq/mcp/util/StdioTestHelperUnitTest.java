package com.monicahq.mcp.util;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for StdioTestHelper utility methods.
 * Tests the helper methods without starting the actual MCP server.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class StdioTestHelperUnitTest {

    private StdioTestHelper stdioHelper;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        stdioHelper = new StdioTestHelper();
        objectMapper = new ObjectMapper();
    }

    @Test
    void shouldCreateValidMcpRequest() {
        // Given
        String method = "tools/list";
        Map<String, Object> params = Map.of("limit", 100);
        int id = 123;

        // When
        Map<String, Object> request = stdioHelper.createMcpRequest(method, params, id);

        // Then
        assertEquals("2.0", request.get("jsonrpc"));
        assertEquals(method, request.get("method"));
        assertEquals(params, request.get("params"));
        assertEquals(id, request.get("id"));
    }

    @Test
    void shouldCreateValidMcpNotification() {
        // Given
        String method = "initialized";
        Map<String, Object> params = Map.of();

        // When
        Map<String, Object> notification = stdioHelper.createMcpNotification(method, params);

        // Then
        assertEquals("2.0", notification.get("jsonrpc"));
        assertEquals(method, notification.get("method"));
        assertEquals(params, notification.get("params"));
        assertFalse(notification.containsKey("id"));
    }

    @Test
    void shouldGenerateUniqueRequestIds() throws InterruptedException {
        // When
        int id1 = stdioHelper.generateRequestId();
        Thread.sleep(1); // Ensure different timestamp
        int id2 = stdioHelper.generateRequestId();

        // Then
        assertNotEquals(id1, id2);
        assertTrue(id1 > 0);
        assertTrue(id2 > 0);
    }

    @Test
    void shouldValidateJsonRpcResponse() throws Exception {
        // Given
        String validResponse = """
            {
                "jsonrpc": "2.0",
                "id": 1,
                "result": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {},
                    "serverInfo": {
                        "name": "monica-mcp",
                        "version": "1.0.0"
                    }
                }
            }
            """;
        JsonNode response = objectMapper.readTree(validResponse);

        // When/Then - should not throw
        stdioHelper.validateJsonRpcResponse(response);
    }

    @Test
    void shouldValidateInitializeResponse() throws Exception {
        // Given
        String initResponse = """
            {
                "jsonrpc": "2.0",
                "id": 1,
                "result": {
                    "protocolVersion": "2024-11-05",
                    "capabilities": {
                        "tools": {}
                    },
                    "serverInfo": {
                        "name": "monica-mcp",
                        "version": "1.0.0"
                    }
                }
            }
            """;
        JsonNode response = objectMapper.readTree(initResponse);

        // When/Then - should not throw
        stdioHelper.validateInitializeResponse(response);
    }

    @Test
    void shouldValidateToolsListResponse() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact",
                            "inputSchema": {
                                "type": "object",
                                "properties": {
                                    "firstName": {"type": "string"}
                                }
                            }
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When/Then - should not throw
        stdioHelper.validateToolsListResponse(response);
    }

    @Test
    void shouldValidateToolCallResponse() throws Exception {
        // Given
        String toolCallResponse = """
            {
                "jsonrpc": "2.0",
                "id": 3,
                "result": {
                    "content": [
                        {
                            "type": "text",
                            "text": "Contact created successfully"
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolCallResponse);

        // When/Then - should not throw
        stdioHelper.validateToolCallResponse(response);
    }

    @Test
    void shouldValidateErrorResponse() throws Exception {
        // Given
        String errorResponse = """
            {
                "jsonrpc": "2.0",
                "id": 4,
                "error": {
                    "code": -32602,
                    "message": "Invalid params",
                    "data": "firstName is required"
                }
            }
            """;
        JsonNode response = objectMapper.readTree(errorResponse);

        // When/Then - should not throw
        stdioHelper.validateErrorResponse(response, StdioTestHelper.INVALID_PARAMS);
    }

    @Test
    void shouldFindToolInToolsList() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact"
                        },
                        {
                            "name": "contact_list",
                            "description": "List contacts"
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When
        JsonNode tool = stdioHelper.findTool(response, "contact_create");

        // Then
        assertNotNull(tool);
        assertEquals("contact_create", tool.get("name").asText());
        assertEquals("Create a new contact", tool.get("description").asText());
    }

    @Test
    void shouldReturnNullForNonExistentTool() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact"
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When
        JsonNode tool = stdioHelper.findTool(response, "nonexistent_tool");

        // Then
        assertNull(tool);
    }

    @Test
    void shouldAssertToolExists() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact"
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When/Then - should not throw
        stdioHelper.assertToolExists(response, "contact_create");
    }

    @Test
    void shouldThrowWhenToolDoesNotExist() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": []
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When/Then
        assertThrows(AssertionError.class, () -> {
            stdioHelper.assertToolExists(response, "nonexistent_tool");
        });
    }

    @Test
    void shouldAssertToolProperties() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact",
                            "inputSchema": {
                                "type": "object"
                            }
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When/Then - should not throw
        stdioHelper.assertToolProperties(response, "contact_create", "name", "description", "inputSchema");
    }

    @Test
    void shouldThrowWhenToolMissingProperty() throws Exception {
        // Given
        String toolsResponse = """
            {
                "jsonrpc": "2.0",
                "id": 2,
                "result": {
                    "tools": [
                        {
                            "name": "contact_create",
                            "description": "Create a new contact"
                        }
                    ]
                }
            }
            """;
        JsonNode response = objectMapper.readTree(toolsResponse);

        // When/Then
        assertThrows(AssertionError.class, () -> {
            stdioHelper.assertToolProperties(response, "contact_create", "name", "description", "inputSchema");
        });
    }

    @Test
    void shouldRejectInvalidJsonRpcResponse() throws Exception {
        // Given - missing jsonrpc field
        String invalidResponse = """
            {
                "id": 1,
                "result": {}
            }
            """;
        JsonNode response = objectMapper.readTree(invalidResponse);

        // When/Then
        assertThrows(AssertionError.class, () -> {
            stdioHelper.validateJsonRpcResponse(response);
        });
    }

    @Test
    void shouldRejectResponseWithBothResultAndError() throws Exception {
        // Given - has both result and error
        String invalidResponse = """
            {
                "jsonrpc": "2.0",
                "id": 1,
                "result": {},
                "error": {
                    "code": -32603,
                    "message": "Internal error"
                }
            }
            """;
        JsonNode response = objectMapper.readTree(invalidResponse);

        // When/Then
        assertThrows(AssertionError.class, () -> {
            stdioHelper.validateJsonRpcResponse(response);
        });
    }
}