package com.monicahq.mcp.exception;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for GlobalExceptionHandler's handling of MonicaApiException.
 * Verifies that API error details are properly surfaced in MCP error responses.
 */
@DisplayName("MonicaApiException Handler Tests")
class MonicaApiExceptionHandlerTest {

    private GlobalExceptionHandler exceptionHandler;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        objectMapper = new ObjectMapper();
        exceptionHandler = new GlobalExceptionHandler(objectMapper);
    }

    @Test
    @DisplayName("422 Validation error includes status code and API response body")
    void validationErrorIncludesApiDetails() {
        // Given: A 422 validation error from Monica API
        String apiErrorBody = """
            {
                "message": "The given data was invalid.",
                "errors": {
                    "contact_field_type_id": ["The contact field type id is invalid."],
                    "data": ["The data field is required."]
                }
            }
            """;

        MonicaApiException exception = new MonicaApiException(
            422,
            "Invalid contactFieldTypeId. Use the contact_field_type_list tool.",
            apiErrorBody
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Response contains detailed error information
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());

                Map<String, Object> body = responseEntity.getBody();
                assertNotNull(body);
                assertEquals("2.0", body.get("jsonrpc"));

                // Verify error object
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");
                assertNotNull(error);

                // Should use VALIDATION_ERROR code
                assertEquals(-32003, error.get("code"));

                // Should include helpful message
                String message = (String) error.get("message");
                assertTrue(message.contains("Invalid contactFieldTypeId"));

                // Should include error data with status code and API response
                @SuppressWarnings("unchecked")
                Map<String, Object> errorData = (Map<String, Object>) error.get("data");
                assertNotNull(errorData);
                assertEquals(422, errorData.get("statusCode"));
                assertEquals(apiErrorBody, errorData.get("apiResponse"));

                // Should include parsed error details
                assertTrue(errorData.containsKey("details"));
                @SuppressWarnings("unchecked")
                Map<String, Object> details = (Map<String, Object>) errorData.get("details");
                assertTrue(details.containsKey("errors"));
            })
            .verifyComplete();
    }

    @Test
    @DisplayName("404 Not Found error includes status code and response body")
    void notFoundErrorIncludesApiDetails() {
        // Given: A 404 error from Monica API
        String apiErrorBody = """
            {
                "message": "No query results for model [App\\\\Models\\\\Contact] 99999"
            }
            """;

        MonicaApiException exception = new MonicaApiException(
            404,
            "Resource not found: The requested item does not exist in your MonicaHQ account",
            apiErrorBody
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Response contains API error details
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                assertEquals(HttpStatus.BAD_REQUEST, responseEntity.getStatusCode());

                Map<String, Object> body = responseEntity.getBody();
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");

                // Should use MONICA_API_ERROR code for 404
                assertEquals(-32000, error.get("code"));

                // Should include resource not found message
                String message = (String) error.get("message");
                assertTrue(message.contains("Resource not found"));

                // Should include error data
                @SuppressWarnings("unchecked")
                Map<String, Object> errorData = (Map<String, Object>) error.get("data");
                assertEquals(404, errorData.get("statusCode"));
                assertEquals(apiErrorBody, errorData.get("apiResponse"));

                // Should parse the JSON response
                assertTrue(errorData.containsKey("details"));
                @SuppressWarnings("unchecked")
                Map<String, Object> details = (Map<String, Object>) errorData.get("details");
                assertTrue(details.containsKey("message"));
            })
            .verifyComplete();
    }

    @Test
    @DisplayName("401 Authentication error uses AUTHENTICATION_ERROR code")
    void authenticationErrorUseCorrectCode() {
        // Given: A 401 error from Monica API
        MonicaApiException exception = new MonicaApiException(
            401,
            "Authentication failed: Invalid or expired API token",
            "{\"error\": \"Unauthenticated.\"}"
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Uses AUTHENTICATION_ERROR code
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                Map<String, Object> body = responseEntity.getBody();
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");
                assertEquals(-32002, error.get("code")); // AUTHENTICATION_ERROR
            })
            .verifyComplete();
    }

    @Test
    @DisplayName("500 Server error uses INTERNAL_ERROR code")
    void serverErrorUsesInternalErrorCode() {
        // Given: A 500 error from Monica API
        String apiErrorBody = """
            {
                "message": "Server Error",
                "exception": "Exception",
                "file": "/var/www/monica/app/Http/Controllers/Api/ContactController.php"
            }
            """;

        MonicaApiException exception = new MonicaApiException(
            500,
            "MonicaHQ API server error (500)",
            apiErrorBody
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Uses INTERNAL_ERROR code and BAD_GATEWAY status
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                assertEquals(HttpStatus.BAD_GATEWAY, responseEntity.getStatusCode());

                Map<String, Object> body = responseEntity.getBody();
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");
                assertEquals(-32603, error.get("code")); // INTERNAL_ERROR

                @SuppressWarnings("unchecked")
                Map<String, Object> errorData = (Map<String, Object>) error.get("data");
                assertEquals(500, errorData.get("statusCode"));
                assertTrue(errorData.containsKey("apiResponse"));
            })
            .verifyComplete();
    }

    @Test
    @DisplayName("Empty API response body is handled gracefully")
    void emptyResponseBodyHandledGracefully() {
        // Given: Exception with empty response body
        MonicaApiException exception = new MonicaApiException(
            404,
            "Resource not found",
            ""
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Response is created without apiResponse field
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                Map<String, Object> body = responseEntity.getBody();
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");
                @SuppressWarnings("unchecked")
                Map<String, Object> errorData = (Map<String, Object>) error.get("data");

                // apiResponse should not be present for empty response body
                assertFalse(errorData.containsKey("apiResponse"));
                // But status code should still be present
                assertEquals(404, errorData.get("statusCode"));
            })
            .verifyComplete();
    }

    @Test
    @DisplayName("Non-JSON response body is included as-is")
    void nonJsonResponseBodyIncluded() {
        // Given: Exception with plain text response body
        String plainTextError = "Internal Server Error";
        MonicaApiException exception = new MonicaApiException(
            500,
            "MonicaHQ API server error (500)",
            plainTextError
        );

        // When: Handler processes the exception
        Mono<ResponseEntity<Map<String, Object>>> responseMono =
            exceptionHandler.handleMonicaApiException(exception, null);

        // Then: Response includes the plain text as apiResponse
        StepVerifier.create(responseMono)
            .assertNext(responseEntity -> {
                Map<String, Object> body = responseEntity.getBody();
                @SuppressWarnings("unchecked")
                Map<String, Object> error = (Map<String, Object>) body.get("error");
                @SuppressWarnings("unchecked")
                Map<String, Object> errorData = (Map<String, Object>) error.get("data");

                assertEquals(plainTextError, errorData.get("apiResponse"));
                // details should be absent since parsing failed
                assertFalse(errorData.containsKey("details"));
            })
            .verifyComplete();
    }
}
