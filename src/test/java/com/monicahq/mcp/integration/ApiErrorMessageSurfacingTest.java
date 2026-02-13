package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.controller.McpMessageHandler;
import com.monicahq.mcp.exception.MonicaApiException;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.TestPropertySource;
import reactor.core.publisher.Mono;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.when;

/**
 * Integration test verifying that upstream API error messages are surfaced in MCP responses.
 * Tests the complete flow from MonicaApiException -> GlobalExceptionHandler -> MCP error response.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
@DisplayName("API Error Message Surfacing Integration Test")
class ApiErrorMessageSurfacingTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @MockBean
    private MonicaHqClient monicaClient;

    @Test
    @DisplayName("422 Validation error from Monica API is surfaced in MCP response with full details")
    void validationErrorSurfacesApiDetails() throws Exception {
        // Given: Monica API returns a 422 validation error
        String apiErrorBody = """
            {
                "message": "The given data was invalid.",
                "errors": {
                    "contact_field_type_id": ["The contact field type id is invalid."],
                    "data": ["The data field is required."]
                }
            }
            """;

        MonicaApiException apiException = new MonicaApiException(
            422,
            "Invalid contactFieldTypeId. Use the contact_field_type_list tool to see available field types in your Monica instance.",
            apiErrorBody
        );

        when(monicaClient.post(eq("/contactfields"), any()))
            .thenReturn(Mono.error(apiException));

        // When: MCP tool call is made
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_field_create",
                "arguments", Map.of(
                    "contactId", 123,
                    "contactFieldTypeId", 999,
                    "data", ""
                )
            ),
            "id", 1
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: MCP response contains detailed error information
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("error"), "Response should contain error field");

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");

        // Verify MCP error code
        assertEquals(-32003, error.get("code"), "Should use VALIDATION_ERROR code");

        // Verify error message contains helpful guidance
        String errorMessage = (String) error.get("message");
        assertTrue(errorMessage.contains("Invalid contactFieldTypeId"),
            "Error message should contain helpful guidance");
        assertTrue(errorMessage.contains("contact_field_type_list"),
            "Error message should suggest using contact_field_type_list tool");

        // Verify error data contains API details
        assertTrue(error.containsKey("data"), "Error should contain data field");
        @SuppressWarnings("unchecked")
        Map<String, Object> errorData = (Map<String, Object>) error.get("data");

        assertEquals(422, errorData.get("statusCode"), "Should include HTTP status code");
        assertTrue(errorData.containsKey("apiResponse"), "Should include raw API response");

        String apiResponse = (String) errorData.get("apiResponse");
        assertTrue(apiResponse.contains("contact_field_type_id"),
            "API response should contain original field names");
        assertTrue(apiResponse.contains("invalid"),
            "API response should contain validation error details");
    }

    @Test
    @DisplayName("404 Not Found error from Monica API includes status code and response body")
    void notFoundErrorSurfacesApiDetails() throws Exception {
        // Given: Monica API returns a 404 error
        String apiErrorBody = """
            {
                "message": "No query results for model [App\\\\Models\\\\Contact] 99999"
            }
            """;

        MonicaApiException apiException = new MonicaApiException(
            404,
            "Resource not found: The requested item does not exist in your MonicaHQ account",
            apiErrorBody
        );

        when(monicaClient.get(eq("/contacts/99999"), isNull()))
            .thenReturn(Mono.error(apiException));

        // When: MCP tool call is made
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_get",
                "arguments", Map.of("contactId", 99999)
            ),
            "id", 2
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: MCP response contains API error details
        assertNotNull(response);
        assertTrue(response.containsKey("error"));

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");

        // Verify error code is MONICA_API_ERROR
        assertEquals(-32000, error.get("code"));

        // Verify error message
        String errorMessage = (String) error.get("message");
        assertTrue(errorMessage.contains("Resource not found"));

        // Verify error data
        @SuppressWarnings("unchecked")
        Map<String, Object> errorData = (Map<String, Object>) error.get("data");
        assertEquals(404, errorData.get("statusCode"));
        assertTrue(errorData.containsKey("apiResponse"));

        String apiResponse = (String) errorData.get("apiResponse");
        assertTrue(apiResponse.contains("No query results"),
            "Should include Monica's original error message");
    }

    @Test
    @DisplayName("401 Authentication error includes status code and guidance")
    void authenticationErrorSurfacesApiDetails() throws Exception {
        // Given: Monica API returns a 401 error
        String apiErrorBody = """
            {
                "error": "Unauthenticated."
            }
            """;

        MonicaApiException apiException = new MonicaApiException(
            401,
            "Authentication failed: Invalid or expired API token",
            apiErrorBody
        );

        when(monicaClient.get(anyString(), any()))
            .thenReturn(Mono.error(apiException));

        // When: MCP tool call is made
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_list",
                "arguments", Map.of("limit", 10)
            ),
            "id", 3
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: MCP response contains authentication error
        assertNotNull(response);
        assertTrue(response.containsKey("error"));

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");

        // Verify error code is AUTHENTICATION_ERROR
        assertEquals(-32002, error.get("code"));

        // Verify helpful error message
        String errorMessage = (String) error.get("message");
        assertTrue(errorMessage.contains("Authentication failed"));
        assertTrue(errorMessage.contains("API token"));

        // Verify error data includes status code and API response
        @SuppressWarnings("unchecked")
        Map<String, Object> errorData = (Map<String, Object>) error.get("data");
        assertEquals(401, errorData.get("statusCode"));
        assertTrue(errorData.containsKey("apiResponse"));
    }

    @Test
    @DisplayName("500 Server error includes status code and response body")
    void serverErrorSurfacesApiDetails() throws Exception {
        // Given: Monica API returns a 500 error
        String apiErrorBody = """
            {
                "message": "Server Error",
                "exception": "Exception",
                "file": "/var/www/monica/app/Http/Controllers/Api/ContactController.php",
                "line": 42
            }
            """;

        MonicaApiException apiException = new MonicaApiException(
            500,
            "MonicaHQ API server error (500)",
            apiErrorBody
        );

        when(monicaClient.post(anyString(), any()))
            .thenReturn(Mono.error(apiException));

        // When: MCP tool call is made
        Map<String, Object> toolsCallRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Test",
                    "lastName", "User",
                    "genderId", 1
                )
            ),
            "id", 4
        );

        JsonNode requestNode = objectMapper.valueToTree(toolsCallRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: MCP response contains server error details
        assertNotNull(response);
        assertTrue(response.containsKey("error"));

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");

        // Verify error code is INTERNAL_ERROR
        assertEquals(-32603, error.get("code"));

        // Verify error message
        String errorMessage = (String) error.get("message");
        assertTrue(errorMessage.contains("server error"));

        // Verify error data includes full API response
        @SuppressWarnings("unchecked")
        Map<String, Object> errorData = (Map<String, Object>) error.get("data");
        assertEquals(500, errorData.get("statusCode"));
        assertTrue(errorData.containsKey("apiResponse"));

        String apiResponse = (String) errorData.get("apiResponse");
        assertTrue(apiResponse.contains("Server Error"),
            "Should preserve Monica's original error message");
    }
}
