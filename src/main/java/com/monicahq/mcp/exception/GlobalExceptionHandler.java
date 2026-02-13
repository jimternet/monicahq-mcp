package com.monicahq.mcp.exception;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.config.LoggingConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.reactive.function.client.WebClientException;
import org.springframework.web.reactive.function.client.WebClientRequestException;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import org.springframework.web.server.ServerWebExchange;
import reactor.core.publisher.Mono;

import java.time.Instant;
import java.util.HashMap;
import java.util.Map;

@ControllerAdvice
@Slf4j
@RequiredArgsConstructor
public class GlobalExceptionHandler {

    private final ObjectMapper objectMapper;

    @ExceptionHandler(MonicaApiException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleMonicaApiException(
            MonicaApiException ex, ServerWebExchange exchange) {

        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleMonicaApiException");

        log.error("MonicaHQ API error: HTTP {} - {}", ex.getStatusCode(), ex.getMessage());

        // Parse the response body to extract structured error details
        Map<String, Object> apiErrorDetails = parseApiErrorResponse(ex.getResponseBody());

        // Build detailed error data for MCP response
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("statusCode", ex.getStatusCode());
        errorData.put("apiMessage", ex.getMessage());

        // Include the raw API response body if it's not empty
        if (ex.getResponseBody() != null && !ex.getResponseBody().trim().isEmpty()) {
            errorData.put("apiResponse", ex.getResponseBody());
        }

        // Add parsed error details if available
        if (apiErrorDetails != null && !apiErrorDetails.isEmpty()) {
            errorData.put("details", apiErrorDetails);
        }

        // Determine MCP error code based on HTTP status
        int mcpErrorCode = mapHttpStatusToMcpErrorCode(ex.getStatusCode());

        Map<String, Object> errorResponse = createMcpErrorResponse(
            mcpErrorCode,
            ex.getMessage(),
            errorData
        );

        LoggingConfig.McpLoggingContext.clearMcpContext();

        // Use appropriate HTTP status for the response
        HttpStatus httpStatus = ex.isClientError() ? HttpStatus.BAD_REQUEST : HttpStatus.BAD_GATEWAY;
        return Mono.just(ResponseEntity.status(httpStatus).body(errorResponse));
    }

    @ExceptionHandler(McpException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleMcpException(McpException ex, ServerWebExchange exchange) {
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleMcpException");

        log.error("MCP Exception: {} - {}", ex.getErrorCode(), ex.getMessage(), ex);

        Map<String, Object> errorResponse = createMcpErrorResponse(
            ex.getErrorCode(),
            ex.getMessage(),
            ex.getDetails()
        );

        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse));
    }

    @ExceptionHandler(WebClientResponseException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleWebClientResponseException(
            WebClientResponseException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleWebClientResponseException");
        
        log.error("MonicaHQ API error: {} - {}", ex.getStatusCode(), ex.getMessage(), ex);
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32000, // Internal error
            "MonicaHQ API error: " + ex.getStatusText(),
            Map.of(
                "statusCode", ex.getStatusCode().value(),
                "responseBody", ex.getResponseBodyAsString()
            )
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.BAD_GATEWAY).body(errorResponse));
    }

    @ExceptionHandler(WebClientRequestException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleWebClientRequestException(
            WebClientRequestException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleWebClientRequestException");
        
        log.error("MonicaHQ API connection error: {}", ex.getMessage(), ex);
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32001, // Connection error
            "Failed to connect to MonicaHQ API",
            Map.of("cause", ex.getMessage())
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(errorResponse));
    }

    @ExceptionHandler(WebClientException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleWebClientException(
            WebClientException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleWebClientException");
        
        log.error("WebClient error: {}", ex.getMessage(), ex);
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32000, // Internal error
            "HTTP client error",
            Map.of("cause", ex.getMessage())
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse));
    }

    @ExceptionHandler(IllegalArgumentException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleIllegalArgumentException(
            IllegalArgumentException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleIllegalArgumentException");
        
        log.warn("Invalid argument: {}", ex.getMessage());
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32602, // Invalid params
            "Invalid parameters: " + ex.getMessage(),
            null
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.BAD_REQUEST).body(errorResponse));
    }

    @ExceptionHandler(UnsupportedOperationException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleUnsupportedOperationException(
            UnsupportedOperationException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleUnsupportedOperationException");
        
        log.warn("Unsupported operation: {}", ex.getMessage());
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32601, // Method not found
            "Unsupported operation: " + ex.getMessage(),
            null
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.NOT_FOUND).body(errorResponse));
    }

    @ExceptionHandler(IllegalStateException.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleIllegalStateException(
            IllegalStateException ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleIllegalStateException");
        
        // Check if this is a known API availability issue
        if (ex.getMessage().contains("API is not available") || 
            ex.getMessage().contains("may not be available") ||
            ex.getMessage().contains("admin-only") ||
            ex.getMessage().contains("not implemented")) {
            
            log.warn("API availability issue: {}", ex.getMessage());
            
            Map<String, Object> errorResponse = createMcpErrorResponse(
                McpErrorCodes.API_NOT_AVAILABLE, // Custom error code for API availability
                "API endpoint not available: " + ex.getMessage(),
                Map.of(
                    "type", "availability_error",
                    "suggestion", "This API may require administrator privileges or may not be implemented in this Monica version"
                )
            );
            
            LoggingConfig.McpLoggingContext.clearMcpContext();
            return Mono.just(ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(errorResponse));
        }
        
        // Handle other IllegalStateException cases
        log.error("Illegal state: {}", ex.getMessage(), ex);
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32603, // Internal error
            "System in illegal state: " + ex.getMessage(),
            Map.of("type", "illegal_state")
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse));
    }

    @ExceptionHandler(Exception.class)
    public Mono<ResponseEntity<Map<String, Object>>> handleGenericException(
            Exception ex, ServerWebExchange exchange) {
        
        LoggingConfig.McpLoggingContext.setServiceContext("GlobalExceptionHandler", "handleGenericException");
        
        log.error("Unexpected error: {}", ex.getMessage(), ex);
        
        Map<String, Object> errorResponse = createMcpErrorResponse(
            -32603, // Internal error
            "Internal server error",
            Map.of("type", ex.getClass().getSimpleName())
        );
        
        LoggingConfig.McpLoggingContext.clearMcpContext();
        return Mono.just(ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(errorResponse));
    }

    private Map<String, Object> createMcpErrorResponse(int errorCode, String message, Object details) {
        Map<String, Object> error = new HashMap<>();
        error.put("code", errorCode);
        error.put("message", message);
        
        if (details != null) {
            error.put("data", details);
        }
        
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("error", error);
        response.put("id", null); // Will be set by the calling context if available
        response.put("timestamp", Instant.now());
        response.put("correlationId", LoggingConfig.getCorrelationId());
        
        return response;
    }

    public static Map<String, Object> createMcpErrorResponse(Object id, int code, String message, Object data) {
        Map<String, Object> error = new HashMap<>();
        error.put("code", code);
        error.put("message", message);
        
        if (data != null) {
            error.put("data", data);
        }
        
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("error", error);
        response.put("id", id);
        response.put("timestamp", Instant.now());
        response.put("correlationId", LoggingConfig.getCorrelationId());
        
        return response;
    }

    public static Map<String, Object> createMcpSuccessResponse(Object id, Object result) {
        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("result", result);
        response.put("id", id);
        response.put("timestamp", Instant.now());
        response.put("correlationId", LoggingConfig.getCorrelationId());
        
        return response;
    }

    /**
     * Parses the API error response body to extract structured error details.
     * Attempts to parse as JSON; returns null if parsing fails.
     *
     * @param responseBody the raw API error response
     * @return parsed error details or null
     */
    @SuppressWarnings("unchecked")
    private Map<String, Object> parseApiErrorResponse(String responseBody) {
        if (responseBody == null || responseBody.trim().isEmpty()) {
            return null;
        }

        try {
            // Try to parse as JSON
            return objectMapper.readValue(responseBody, Map.class);
        } catch (JsonProcessingException e) {
            // If not JSON, just log and return null
            log.debug("API response body is not valid JSON: {}", responseBody);
            return null;
        }
    }

    /**
     * Maps HTTP status codes to MCP error codes.
     *
     * @param httpStatus the HTTP status code
     * @return the corresponding MCP error code
     */
    private int mapHttpStatusToMcpErrorCode(int httpStatus) {
        switch (httpStatus) {
            case 401:
                return McpErrorCodes.AUTHENTICATION_ERROR;
            case 403:
                return McpErrorCodes.AUTHENTICATION_ERROR; // Authorization also uses auth error
            case 404:
                return McpErrorCodes.MONICA_API_ERROR; // Resource not found
            case 422:
                return McpErrorCodes.VALIDATION_ERROR;
            case 429:
                return McpErrorCodes.MONICA_API_ERROR; // Rate limit
            default:
                if (httpStatus >= 500) {
                    return McpErrorCodes.INTERNAL_ERROR; // Server error
                } else {
                    return McpErrorCodes.MONICA_API_ERROR; // Generic API error
                }
        }
    }

    // Common MCP error codes
    public static final class McpErrorCodes {
        public static final int PARSE_ERROR = -32700;
        public static final int INVALID_REQUEST = -32600;
        public static final int METHOD_NOT_FOUND = -32601;
        public static final int INVALID_PARAMS = -32602;
        public static final int INTERNAL_ERROR = -32603;
        public static final int SERVER_ERROR_START = -32000;
        public static final int SERVER_ERROR_END = -32099;

        // Custom application error codes
        public static final int MONICA_API_ERROR = -32000;
        public static final int MONICA_CONNECTION_ERROR = -32001;
        public static final int AUTHENTICATION_ERROR = -32002;
        public static final int VALIDATION_ERROR = -32003;
        public static final int TOOL_EXECUTION_ERROR = -32004;
        public static final int WEBSOCKET_ERROR = -32005;
        public static final int API_NOT_AVAILABLE = -32006; // API Gap Fix: Not available APIs
    }
}