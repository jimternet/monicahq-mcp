# Error Handling Standardization Proposal

**Version**: 1.0
**Date**: 2026-02-06
**Status**: PROPOSAL - Requires Team Approval
**Impact**: Medium - Requires refactoring of exception handlers

---

## Problem Statement

### Current Issues

The MonicaHQ MCP Server currently has **overlapping and inconsistent error code schemes**:

**Problem 1: Duplicate Error Code -32000**
```java
// GlobalExceptionHandler.java
public static final int SERVER_ERROR_START = -32000;  // Generic range start
public static final int MONICA_API_ERROR = -32000;    // Specific error

// CONFLICT: Same code used for two purposes!
```

**Problem 2: Multiple Error Schemes**
1. **JSON-RPC 2.0 Standard** (-32700 to -32603)
2. **Custom Application Codes** (-32000 to -32006)
3. **Range-based Codes** (SERVER_ERROR_START to SERVER_ERROR_END)

**Problem 3: Error Code Gaps**
- Monica API errors: -32000
- Monica connection errors: -32001
- Authentication errors: -32002
- *Gap at -32003*
- Validation errors: -32003
- Tool execution: -32004
- WebSocket: -32005
- API not available: -32006

This makes it **difficult to debug** and **confusing for clients** consuming the MCP server.

---

## Proposed Solution

### Unified Error Code Scheme

Redesign error codes to be **non-overlapping**, **semantically clear**, and **JSON-RPC compliant**.

---

## Error Code Architecture

### Tier 1: JSON-RPC 2.0 Standard Errors (MANDATORY)

These codes are **defined by the JSON-RPC 2.0 spec** and MUST NOT be changed:

```java
/**
 * JSON-RPC 2.0 standard error codes.
 * These are protocol-level errors defined by the spec.
 */
public static final class JsonRpcErrors {
    /** Invalid JSON was received. */
    public static final int PARSE_ERROR = -32700;

    /** The JSON sent is not a valid Request object. */
    public static final int INVALID_REQUEST = -32600;

    /** The method does not exist or is not available. */
    public static final int METHOD_NOT_FOUND = -32601;

    /** Invalid method parameter(s). */
    public static final int INVALID_PARAMS = -32602;

    /** Internal JSON-RPC error. */
    public static final int INTERNAL_ERROR = -32603;

    // Reserved range: -32768 to -32000 for future JSON-RPC spec use
}
```

**Usage**: Protocol parsing, MCP message validation, JSON deserialization errors.

---

### Tier 2: MCP-Specific Errors (Application Layer)

Application-specific errors that don't fit JSON-RPC categories.

**Range**: -32100 to -32199 (100 error codes available)

```java
/**
 * MCP Server application-specific error codes.
 * Range: -32100 to -32199
 */
public static final class McpApplicationErrors {

    // === Authentication & Authorization (001-010) ===
    /** Monica API authentication failed (invalid token). */
    public static final int AUTHENTICATION_FAILED = -32101;

    /** Insufficient permissions for the requested operation. */
    public static final int AUTHORIZATION_FAILED = -32102;

    /** API token has expired or been revoked. */
    public static final int TOKEN_EXPIRED = -32103;

    // === External API Errors (011-030) ===
    /** Failed to connect to Monica API (network/timeout). */
    public static final int MONICA_API_UNREACHABLE = -32111;

    /** Monica API returned an error response. */
    public static final int MONICA_API_ERROR = -32112;

    /** Rate limit exceeded on Monica API. */
    public static final int RATE_LIMIT_EXCEEDED = -32113;

    /** Monica API endpoint not implemented or unavailable. */
    public static final int API_ENDPOINT_UNAVAILABLE = -32114;

    /** Circuit breaker is open due to repeated failures. */
    public static final int CIRCUIT_BREAKER_OPEN = -32115;

    // === Data Validation (031-050) ===
    /** Required parameter is missing. */
    public static final int MISSING_REQUIRED_PARAMETER = -32131;

    /** Parameter has invalid format or value. */
    public static final int INVALID_PARAMETER_VALUE = -32132;

    /** Data failed business logic validation. */
    public static final int VALIDATION_ERROR = -32133;

    /** Resource already exists (duplicate creation). */
    public static final int RESOURCE_ALREADY_EXISTS = -32134;

    /** Resource not found in Monica API. */
    public static final int RESOURCE_NOT_FOUND = -32135;

    // === Tool Execution (051-070) ===
    /** Tool execution failed due to internal error. */
    public static final int TOOL_EXECUTION_FAILED = -32151;

    /** Tool operation timed out. */
    public static final int TOOL_TIMEOUT = -32152;

    /** Tool not found or not available. */
    public static final int TOOL_NOT_FOUND = -32153;

    // === WebSocket / Transport (071-090) ===
    /** WebSocket connection error. */
    public static final int WEBSOCKET_ERROR = -32171;

    /** Maximum WebSocket sessions exceeded. */
    public static final int MAX_SESSIONS_EXCEEDED = -32172;

    /** WebSocket session disconnected unexpectedly. */
    public static final int SESSION_DISCONNECTED = -32173;

    // === Configuration / System (091-099) ===
    /** System is misconfigured or missing required config. */
    public static final int CONFIGURATION_ERROR = -32191;

    /** System resource exhausted (memory, disk, etc). */
    public static final int RESOURCE_EXHAUSTED = -32192;

    /** Feature is not supported in this version. */
    public static final int FEATURE_NOT_SUPPORTED = -32193;

    /** Service is temporarily unavailable (maintenance). */
    public static final int SERVICE_UNAVAILABLE = -32194;
}
```

---

## Error Response Format

### Standard MCP Error Response

All errors MUST follow this format:

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32112,
    "message": "Monica API returned an error",
    "data": {
      "details": "HTTP 404: Contact not found",
      "monicaStatusCode": 404,
      "endpoint": "/contacts/999",
      "timestamp": "2026-02-06T23:15:00Z",
      "correlationId": "abc-123-def",
      "suggestedAction": "Verify the contact ID exists in your Monica instance"
    }
  },
  "id": 42
}
```

**Required Fields**:
- `code`: Integer error code (from tables above)
- `message`: Human-readable error description

**Optional Fields (in `data`)**:
- `details`: Detailed error explanation
- `monicaStatusCode`: Original HTTP status from Monica API
- `endpoint`: API endpoint that failed
- `timestamp`: When the error occurred
- `correlationId`: For tracing across logs
- `suggestedAction`: What the user should do to fix it
- `retryable`: Boolean indicating if retry might succeed

---

## Implementation Plan

### Phase 1: Update Error Code Definitions

**File**: `src/main/java/com/monicahq/mcp/exception/McpErrorCodes.java`

```java
package com.monicahq.mcp.exception;

/**
 * Standardized error codes for MonicaHQ MCP Server.
 *
 * <p>Error code ranges:</p>
 * <ul>
 *   <li>-32768 to -32000: Reserved by JSON-RPC 2.0 spec</li>
 *   <li>-32100 to -32199: MCP application errors</li>
 * </ul>
 *
 * @see <a href="https://www.jsonrpc.org/specification#error_object">JSON-RPC 2.0 Error Object</a>
 */
public final class McpErrorCodes {

    private McpErrorCodes() {
        // Utility class - no instantiation
    }

    // === JSON-RPC 2.0 Standard Errors ===
    public static final class JsonRpc {
        public static final int PARSE_ERROR = -32700;
        public static final int INVALID_REQUEST = -32600;
        public static final int METHOD_NOT_FOUND = -32601;
        public static final int INVALID_PARAMS = -32602;
        public static final int INTERNAL_ERROR = -32603;
    }

    // === MCP Application Errors ===
    public static final class Application {
        // Authentication (101-110)
        public static final int AUTHENTICATION_FAILED = -32101;
        public static final int AUTHORIZATION_FAILED = -32102;
        public static final int TOKEN_EXPIRED = -32103;

        // Monica API (111-130)
        public static final int MONICA_API_UNREACHABLE = -32111;
        public static final int MONICA_API_ERROR = -32112;
        public static final int RATE_LIMIT_EXCEEDED = -32113;
        public static final int API_ENDPOINT_UNAVAILABLE = -32114;
        public static final int CIRCUIT_BREAKER_OPEN = -32115;

        // Validation (131-150)
        public static final int MISSING_REQUIRED_PARAMETER = -32131;
        public static final int INVALID_PARAMETER_VALUE = -32132;
        public static final int VALIDATION_ERROR = -32133;
        public static final int RESOURCE_ALREADY_EXISTS = -32134;
        public static final int RESOURCE_NOT_FOUND = -32135;

        // Tool Execution (151-170)
        public static final int TOOL_EXECUTION_FAILED = -32151;
        public static final int TOOL_TIMEOUT = -32152;
        public static final int TOOL_NOT_FOUND = -32153;

        // WebSocket (171-190)
        public static final int WEBSOCKET_ERROR = -32171;
        public static final int MAX_SESSIONS_EXCEEDED = -32172;
        public static final int SESSION_DISCONNECTED = -32173;

        // System (191-199)
        public static final int CONFIGURATION_ERROR = -32191;
        public static final int RESOURCE_EXHAUSTED = -32192;
        public static final int FEATURE_NOT_SUPPORTED = -32193;
        public static final int SERVICE_UNAVAILABLE = -32194;
    }

    /**
     * Maps error code to human-readable category.
     */
    public static String getErrorCategory(int errorCode) {
        if (errorCode >= -32000 && errorCode >= -32768) {
            return "JSON-RPC Protocol Error";
        } else if (errorCode >= -32101 && errorCode <= -32110) {
            return "Authentication Error";
        } else if (errorCode >= -32111 && errorCode <= -32130) {
            return "External API Error";
        } else if (errorCode >= -32131 && errorCode <= -32150) {
            return "Validation Error";
        } else if (errorCode >= -32151 && errorCode <= -32170) {
            return "Tool Execution Error";
        } else if (errorCode >= -32171 && errorCode <= -32190) {
            return "Transport Error";
        } else if (errorCode >= -32191 && errorCode <= -32199) {
            return "System Error";
        }
        return "Unknown Error";
    }

    /**
     * Determines if an error code represents a retryable error.
     */
    public static boolean isRetryable(int errorCode) {
        switch (errorCode) {
            case Application.MONICA_API_UNREACHABLE:
            case Application.RATE_LIMIT_EXCEEDED:
            case Application.CIRCUIT_BREAKER_OPEN:
            case Application.TOOL_TIMEOUT:
            case Application.SERVICE_UNAVAILABLE:
                return true;
            default:
                return false;
        }
    }
}
```

---

### Phase 2: Update Exception Handlers

**File**: `src/main/java/com/monicahq/mcp/exception/GlobalExceptionHandler.java`

```java
@ExceptionHandler(WebClientResponseException.class)
public Mono<ResponseEntity<Map<String, Object>>> handleWebClientResponseException(
        WebClientResponseException ex, ServerWebExchange exchange) {

    int statusCode = ex.getStatusCode().value();
    int mcpErrorCode;
    String message;

    // Map HTTP status to appropriate MCP error code
    switch (statusCode) {
        case 401:
            mcpErrorCode = McpErrorCodes.Application.AUTHENTICATION_FAILED;
            message = "Authentication failed: Invalid or expired API token";
            break;
        case 403:
            mcpErrorCode = McpErrorCodes.Application.AUTHORIZATION_FAILED;
            message = "Insufficient permissions for this operation";
            break;
        case 404:
            mcpErrorCode = McpErrorCodes.Application.RESOURCE_NOT_FOUND;
            message = "Resource not found in Monica API";
            break;
        case 422:
            mcpErrorCode = McpErrorCodes.Application.VALIDATION_ERROR;
            message = "Validation error: Please check required fields";
            break;
        case 429:
            mcpErrorCode = McpErrorCodes.Application.RATE_LIMIT_EXCEEDED;
            message = "Rate limit exceeded - please retry after some time";
            break;
        default:
            mcpErrorCode = McpErrorCodes.Application.MONICA_API_ERROR;
            message = "Monica API error: " + ex.getStatusText();
    }

    Map<String, Object> errorData = Map.of(
        "details", ex.getMessage(),
        "monicaStatusCode", statusCode,
        "endpoint", ex.getRequest().getURI().getPath(),
        "retryable", McpErrorCodes.isRetryable(mcpErrorCode),
        "category", McpErrorCodes.getErrorCategory(mcpErrorCode)
    );

    Map<String, Object> errorResponse = createMcpErrorResponse(
        mcpErrorCode,
        message,
        errorData
    );

    return Mono.just(ResponseEntity.status(HttpStatus.BAD_GATEWAY).body(errorResponse));
}
```

---

### Phase 3: Enhanced Error Builder

**File**: `src/main/java/com/monicahq/mcp/exception/ErrorResponseBuilder.java`

```java
@Component
public class ErrorResponseBuilder {

    /**
     * Creates a standardized MCP error response.
     */
    public Map<String, Object> createError(
            Object id,
            int errorCode,
            String message,
            Map<String, Object> additionalData) {

        Map<String, Object> error = new HashMap<>();
        error.put("code", errorCode);
        error.put("message", message);

        Map<String, Object> data = new HashMap<>();
        data.put("category", McpErrorCodes.getErrorCategory(errorCode));
        data.put("retryable", McpErrorCodes.isRetryable(errorCode));
        data.put("timestamp", Instant.now().toString());
        data.put("correlationId", LoggingConfig.getCorrelationId());

        if (additionalData != null) {
            data.putAll(additionalData);
        }

        error.put("data", data);

        Map<String, Object> response = new HashMap<>();
        response.put("jsonrpc", "2.0");
        response.put("error", error);
        response.put("id", id);

        return response;
    }

    /**
     * Creates an error with suggested action for the user.
     */
    public Map<String, Object> createErrorWithSuggestion(
            Object id,
            int errorCode,
            String message,
            String suggestedAction) {

        Map<String, Object> data = Map.of("suggestedAction", suggestedAction);
        return createError(id, errorCode, message, data);
    }

    /**
     * Creates an authentication error with specific details.
     */
    public Map<String, Object> createAuthenticationError(Object id, String details) {
        Map<String, Object> data = Map.of(
            "details", details,
            "suggestedAction", "Verify your MONICA_API_TOKEN is valid and not expired"
        );

        return createError(
            id,
            McpErrorCodes.Application.AUTHENTICATION_FAILED,
            "Authentication failed",
            data
        );
    }

    /**
     * Creates a validation error with field-specific details.
     */
    public Map<String, Object> createValidationError(
            Object id,
            String fieldName,
            String validationMessage) {

        Map<String, Object> data = Map.of(
            "field", fieldName,
            "details", validationMessage,
            "suggestedAction", "Check the " + fieldName + " parameter and try again"
        );

        return createError(
            id,
            McpErrorCodes.Application.VALIDATION_ERROR,
            "Validation failed for " + fieldName,
            data
        );
    }
}
```

---

## Migration Strategy

### Step 1: Backward Compatibility (Optional)

If clients rely on old error codes, provide a migration period:

```java
/**
 * Legacy error codes - DEPRECATED, will be removed in v2.0.
 */
@Deprecated(since = "1.1", forRemoval = true)
public static final class Legacy {
    /** @deprecated Use Application.MONICA_API_ERROR instead */
    @Deprecated
    public static final int MONICA_API_ERROR_OLD = -32000;

    /** @deprecated Use Application.MONICA_API_UNREACHABLE instead */
    @Deprecated
    public static final int MONICA_CONNECTION_ERROR_OLD = -32001;

    // ... other legacy codes
}

/**
 * Maps legacy error codes to new standardized codes.
 */
public static int migrateErrorCode(int legacyCode) {
    switch (legacyCode) {
        case -32000: return Application.MONICA_API_ERROR;
        case -32001: return Application.MONICA_API_UNREACHABLE;
        // ... other mappings
        default: return legacyCode;
    }
}
```

### Step 2: Update All Exception Handlers

Replace old error codes with new constants:

```java
// BEFORE
throw new McpException(-32000, "Monica API error");

// AFTER
throw new McpException(
    McpErrorCodes.Application.MONICA_API_ERROR,
    "Monica API error"
);
```

### Step 3: Update Tests

Update test assertions to use new error codes:

```java
// BEFORE
assertThat(error.get("code")).isEqualTo(-32000);

// AFTER
assertThat(error.get("code")).isEqualTo(McpErrorCodes.Application.MONICA_API_ERROR);
```

### Step 4: Update Documentation

Document the new error code scheme in:
1. API documentation
2. README.md
3. Client libraries/SDKs

---

## Testing Strategy

### Unit Tests

Test error code classification:

```java
@Test
void shouldClassifyAuthenticationErrors() {
    String category = McpErrorCodes.getErrorCategory(-32101);
    assertThat(category).isEqualTo("Authentication Error");
}

@Test
void shouldIdentifyRetryableErrors() {
    boolean retryable = McpErrorCodes.isRetryable(
        McpErrorCodes.Application.RATE_LIMIT_EXCEEDED
    );
    assertThat(retryable).isTrue();
}
```

### Integration Tests

Test error responses end-to-end:

```java
@Test
void shouldReturnStandardizedErrorForAuthenticationFailure() {
    // Trigger authentication error
    Map<String, Object> response = makeRequestWithInvalidToken();

    assertThat(response.get("jsonrpc")).isEqualTo("2.0");
    assertThat(response).containsKey("error");

    Map<String, Object> error = (Map<String, Object>) response.get("error");
    assertThat(error.get("code")).isEqualTo(-32101);
    assertThat(error.get("message")).contains("Authentication failed");

    Map<String, Object> data = (Map<String, Object>) error.get("data");
    assertThat(data.get("category")).isEqualTo("Authentication Error");
    assertThat(data.get("retryable")).isEqualTo(false);
    assertThat(data.get("suggestedAction")).isNotNull();
}
```

---

## Benefits

### For Developers
✅ Clear error code organization
✅ No overlapping codes
✅ Easy to add new error codes
✅ Self-documenting via categories

### For Operations
✅ Better error categorization for monitoring
✅ Identify retryable vs. fatal errors
✅ Improved debugging with correlation IDs
✅ Suggested actions for common issues

### For API Consumers
✅ Consistent error format
✅ Actionable error messages
✅ Know which errors are retryable
✅ Better error handling in client code

---

## Appendix: Complete Error Code Reference

### JSON-RPC 2.0 Standard (-32768 to -32000)

| Code | Name | Description |
|------|------|-------------|
| -32700 | Parse error | Invalid JSON received |
| -32600 | Invalid Request | Invalid Request object |
| -32601 | Method not found | Method does not exist |
| -32602 | Invalid params | Invalid method parameters |
| -32603 | Internal error | Internal JSON-RPC error |

### Authentication (-32101 to -32110)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32101 | Authentication failed | No | Invalid or missing token |
| -32102 | Authorization failed | No | Insufficient permissions |
| -32103 | Token expired | No | Token has expired |

### External API (-32111 to -32130)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32111 | API unreachable | Yes | Network/timeout error |
| -32112 | API error | No | Monica API error response |
| -32113 | Rate limit exceeded | Yes | Too many requests |
| -32114 | Endpoint unavailable | No | Endpoint not supported |
| -32115 | Circuit breaker open | Yes | Too many failures |

### Validation (-32131 to -32150)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32131 | Missing parameter | No | Required param missing |
| -32132 | Invalid parameter | No | Invalid value/format |
| -32133 | Validation error | No | Business logic validation failed |
| -32134 | Already exists | No | Duplicate resource |
| -32135 | Not found | No | Resource doesn't exist |

### Tool Execution (-32151 to -32170)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32151 | Execution failed | No | Tool execution error |
| -32152 | Timeout | Yes | Operation timed out |
| -32153 | Tool not found | No | Unknown tool |

### WebSocket (-32171 to -32190)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32171 | WebSocket error | Yes | Connection error |
| -32172 | Max sessions | No | Too many connections |
| -32173 | Disconnected | Yes | Unexpected disconnect |

### System (-32191 to -32199)

| Code | Name | Retryable | Description |
|------|------|-----------|-------------|
| -32191 | Configuration error | No | Missing/invalid config |
| -32192 | Resource exhausted | Yes | Out of memory/disk |
| -32193 | Not supported | No | Feature unavailable |
| -32194 | Service unavailable | Yes | Temporary maintenance |

---

**Document Owner**: Architecture Team / Devil's Advocate
**Last Updated**: 2026-02-06
**Status**: PROPOSAL - Requires approval before implementation
**Estimated Effort**: 3-5 days development + testing
