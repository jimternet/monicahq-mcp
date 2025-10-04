package com.monicahq.mcp.exception;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;

/**
 * Builder class for creating detailed MCP error responses
 * Provides consistent error formatting across all MCP operations
 */
@Component
@Slf4j
public class DetailedErrorBuilder {

    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));

    /**
     * Creates a detailed error response for parameter validation failures
     * @param id Message ID
     * @param paramName Parameter name that failed validation
     * @param paramValue Parameter value
     * @param validationMessage Validation error message
     * @param suggestions Optional suggestions for fixing the error
     * @return Formatted error response
     */
    public Map<String, Object> createParameterValidationError(Object id, String paramName, Object paramValue, 
                                                              String validationMessage, String suggestions) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating parameter validation error for '{}': {}", paramName, validationMessage);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("parameter", paramName);
        errorData.put("validation_error", validationMessage);
        
        if (paramValue != null) {
            errorData.put("received_value", truncateValue(paramValue));
            errorData.put("received_type", paramValue.getClass().getSimpleName());
        }
        
        if (suggestions != null && !suggestions.trim().isEmpty()) {
            errorData.put("suggestions", suggestions);
        }
        
        // Add helpful examples based on parameter name
        String examples = generateParameterExamples(paramName);
        if (examples != null) {
            errorData.put("examples", examples);
        }
        
        return createErrorResponse(id, -32602, "Invalid params", errorData);
    }

    /**
     * Creates a detailed error response for tool execution failures
     * @param id Message ID
     * @param toolName Tool name
     * @param errorMessage Error message
     * @param cause Optional root cause
     * @return Formatted error response
     */
    public Map<String, Object> createToolExecutionError(Object id, String toolName, String errorMessage, Throwable cause) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating tool execution error for '{}': {}", toolName, errorMessage);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("tool", toolName);
        errorData.put("error_message", errorMessage);
        
        if (cause != null) {
            errorData.put("cause", cause.getClass().getSimpleName());
            errorData.put("cause_message", cause.getMessage());
            
            if (DEBUG_MODE) {
                errorData.put("stack_trace", getStackTraceString(cause));
            }
        }
        
        // Add troubleshooting guidance
        String troubleshooting = generateTroubleshootingGuidance(toolName, errorMessage);
        if (troubleshooting != null) {
            errorData.put("troubleshooting", troubleshooting);
        }
        
        // Determine appropriate error code
        int errorCode = determineErrorCode(errorMessage, cause);
        String errorType = getErrorType(errorCode);
        
        return createErrorResponse(id, errorCode, errorType, errorData);
    }

    /**
     * Creates a detailed error response for authentication failures
     * @param id Message ID
     * @param operation Operation that failed
     * @param authMethod Authentication method used
     * @return Formatted error response
     */
    public Map<String, Object> createAuthenticationError(Object id, String operation, String authMethod) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating authentication error for operation: {}", operation);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("operation", operation);
        errorData.put("auth_method", authMethod != null ? authMethod : "unknown");
        errorData.put("error_message", "Authentication failed or insufficient permissions");
        errorData.put("troubleshooting", 
            "Please check your MONICA_API_TOKEN environment variable and ensure it has the required permissions for this operation");
        
        return createErrorResponse(id, -32000, "Authentication failed", errorData);
    }

    /**
     * Creates a detailed error response for protocol violations
     * @param id Message ID
     * @param protocolVersion Expected protocol version
     * @param receivedVersion Received protocol version
     * @return Formatted error response
     */
    public Map<String, Object> createProtocolError(Object id, String protocolVersion, String receivedVersion) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating protocol error - expected: {}, received: {}", protocolVersion, receivedVersion);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("expected_version", protocolVersion);
        errorData.put("received_version", receivedVersion != null ? receivedVersion : "missing");
        errorData.put("error_message", "Protocol version mismatch or unsupported");
        errorData.put("troubleshooting", 
            "Please ensure your MCP client supports protocol version " + protocolVersion + " or later");
        
        return createErrorResponse(id, -32600, "Invalid Request", errorData);
    }

    /**
     * Creates a detailed error response for resource not found errors
     * @param id Message ID
     * @param resourceType Type of resource (e.g., "contact", "activity")
     * @param resourceId ID of the resource
     * @return Formatted error response
     */
    public Map<String, Object> createResourceNotFoundError(Object id, String resourceType, Object resourceId) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating resource not found error - type: {}, id: {}", resourceType, resourceId);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("resource_type", resourceType);
        errorData.put("resource_id", resourceId);
        errorData.put("error_message", String.format("%s with ID %s not found", 
            capitalize(resourceType), resourceId));
        errorData.put("troubleshooting", 
            String.format("Please verify the %s ID exists and you have permission to access it", resourceType));
        
        return createErrorResponse(id, -32000, "Resource not found", errorData);
    }

    /**
     * Creates a detailed error response for rate limiting
     * @param id Message ID
     * @param operation Operation that was rate limited
     * @param retryAfter Seconds to wait before retry
     * @return Formatted error response
     */
    public Map<String, Object> createRateLimitError(Object id, String operation, Integer retryAfter) {
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Creating rate limit error for operation: {}", operation);
        }
        
        Map<String, Object> errorData = new HashMap<>();
        errorData.put("operation", operation);
        errorData.put("error_message", "Rate limit exceeded");
        
        if (retryAfter != null) {
            errorData.put("retry_after_seconds", retryAfter);
            errorData.put("troubleshooting", 
                String.format("Please wait %d seconds before retrying this operation", retryAfter));
        } else {
            errorData.put("troubleshooting", "Please wait before retrying this operation");
        }
        
        return createErrorResponse(id, -32000, "Rate limit exceeded", errorData);
    }

    private Map<String, Object> createErrorResponse(Object id, int code, String message, Object data) {
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
        
        return response;
    }

    private Object truncateValue(Object value) {
        if (value == null) return null;
        
        String strValue = value.toString();
        if (strValue.length() > 100) {
            return strValue.substring(0, 97) + "...";
        }
        
        return value;
    }

    private String generateParameterExamples(String paramName) {
        return switch (paramName.toLowerCase()) {
            case "attendees" -> "[\"John Doe\", \"Jane Smith\"] or [{\"contactId\": 123}, {\"contactId\": 456}]";
            case "contactid" -> "123 (numeric ID of an existing contact)";
            case "summary" -> "\"Meeting with client about project requirements\"";
            case "description" -> "\"Detailed notes about the activity or event\"";
            case "date", "happenedat" -> "\"2024-01-15T10:30:00Z\" (ISO 8601 format)";
            case "duration" -> "60 (minutes as a number)";
            case "page" -> "1 (page number, starting from 1)";
            case "limit" -> "10 (number of items per page, max 100)";
            case "query" -> "\"search terms\" (text to search for)";
            default -> null;
        };
    }

    private String generateTroubleshootingGuidance(String toolName, String errorMessage) {
        if (errorMessage == null) return null;
        
        String lowerError = errorMessage.toLowerCase();
        
        if (lowerError.contains("authentication") || lowerError.contains("unauthorized")) {
            return "Check your MONICA_API_TOKEN environment variable and ensure it's valid";
        }
        
        if (lowerError.contains("not found") || lowerError.contains("404")) {
            return "Verify the resource ID exists and you have permission to access it";
        }
        
        if (lowerError.contains("validation") || lowerError.contains("invalid")) {
            return "Check the parameter format and ensure all required fields are provided";
        }
        
        if (lowerError.contains("timeout") || lowerError.contains("connection")) {
            return "Check your network connection and MONICA_API_URL environment variable";
        }
        
        if (toolName != null && toolName.contains("_create")) {
            return "Ensure all required fields are provided and have valid values";
        }
        
        if (toolName != null && toolName.contains("_update")) {
            return "Ensure the resource exists and you have permission to modify it";
        }
        
        return null;
    }

    private int determineErrorCode(String errorMessage, Throwable cause) {
        if (errorMessage == null) return -32000;
        
        String lowerError = errorMessage.toLowerCase();
        
        if (lowerError.contains("parse") || lowerError.contains("json")) {
            return -32700; // Parse error
        }
        
        if (lowerError.contains("invalid request") || lowerError.contains("malformed")) {
            return -32600; // Invalid Request
        }
        
        if (lowerError.contains("method not found") || lowerError.contains("unknown method")) {
            return -32601; // Method not found
        }
        
        if (lowerError.contains("invalid params") || lowerError.contains("validation")) {
            return -32602; // Invalid params
        }
        
        if (lowerError.contains("internal error") || cause instanceof RuntimeException) {
            return -32603; // Internal error
        }
        
        return -32000; // Server error
    }

    private String getErrorType(int errorCode) {
        return switch (errorCode) {
            case -32700 -> "Parse error";
            case -32600 -> "Invalid Request";
            case -32601 -> "Method not found";
            case -32602 -> "Invalid params";
            case -32603 -> "Internal error";
            default -> "Server error";
        };
    }

    private String getStackTraceString(Throwable throwable) {
        if (throwable == null) return null;
        
        StringBuilder sb = new StringBuilder();
        for (StackTraceElement element : throwable.getStackTrace()) {
            if (sb.length() > 0) sb.append("\n");
            sb.append(element.toString());
            
            // Limit stack trace length in debug mode
            if (sb.length() > 1000) {
                sb.append("\n... (truncated)");
                break;
            }
        }
        
        return sb.toString();
    }

    private String capitalize(String str) {
        if (str == null || str.isEmpty()) return str;
        return str.substring(0, 1).toUpperCase() + str.substring(1);
    }
}