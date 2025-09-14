package com.monicahq.mcp.exception;

import java.util.Map;

public class McpException extends RuntimeException {
    
    private final int errorCode;
    private final Object details;

    public McpException(int errorCode, String message) {
        super(message);
        this.errorCode = errorCode;
        this.details = null;
    }

    public McpException(int errorCode, String message, Object details) {
        super(message);
        this.errorCode = errorCode;
        this.details = details;
    }

    public McpException(int errorCode, String message, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
        this.details = null;
    }

    public McpException(int errorCode, String message, Object details, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
        this.details = details;
    }

    public int getErrorCode() {
        return errorCode;
    }

    public Object getDetails() {
        return details;
    }

    // Factory methods for common MCP errors
    public static McpException parseError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.PARSE_ERROR, message);
    }

    public static McpException invalidRequest(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.INVALID_REQUEST, message);
    }

    public static McpException methodNotFound(String method) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.METHOD_NOT_FOUND, 
            "Method not found: " + method);
    }

    public static McpException invalidParams(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.INVALID_PARAMS, message);
    }

    public static McpException invalidParams(String message, Object details) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.INVALID_PARAMS, message, details);
    }

    public static McpException internalError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.INTERNAL_ERROR, message);
    }

    public static McpException internalError(String message, Throwable cause) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.INTERNAL_ERROR, message, cause);
    }

    public static McpException monicaApiError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.MONICA_API_ERROR, message);
    }

    public static McpException monicaApiError(String message, Object details) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.MONICA_API_ERROR, message, details);
    }

    public static McpException monicaConnectionError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.MONICA_CONNECTION_ERROR, message);
    }

    public static McpException authenticationError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.AUTHENTICATION_ERROR, message);
    }

    public static McpException validationError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.VALIDATION_ERROR, message);
    }

    public static McpException validationError(String message, Object details) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.VALIDATION_ERROR, message, details);
    }

    public static McpException toolExecutionError(String toolName, String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.TOOL_EXECUTION_ERROR, 
            "Tool execution error for " + toolName + ": " + message);
    }

    public static McpException toolExecutionError(String toolName, String message, Object details) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.TOOL_EXECUTION_ERROR, 
            "Tool execution error for " + toolName + ": " + message, details);
    }

    public static McpException webSocketError(String message) {
        return new McpException(GlobalExceptionHandler.McpErrorCodes.WEBSOCKET_ERROR, message);
    }

    @Override
    public String toString() {
        return String.format("McpException{errorCode=%d, message='%s', details=%s}", 
            errorCode, getMessage(), details);
    }
}