package com.monicahq.mcp.exception;

import lombok.Getter;

/**
 * Exception thrown when the MonicaHQ API returns an error response.
 * Captures the HTTP status code and response body for detailed error reporting.
 */
@Getter
public class MonicaApiException extends RuntimeException {

    private final int statusCode;
    private final String responseBody;

    public MonicaApiException(int statusCode, String message, String responseBody) {
        super(message);
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }

    public MonicaApiException(int statusCode, String message, String responseBody, Throwable cause) {
        super(message, cause);
        this.statusCode = statusCode;
        this.responseBody = responseBody;
    }

    /**
     * Checks if this is a client error (4xx status code)
     */
    public boolean isClientError() {
        return statusCode >= 400 && statusCode < 500;
    }

    /**
     * Checks if this is a server error (5xx status code)
     */
    public boolean isServerError() {
        return statusCode >= 500 && statusCode < 600;
    }

    /**
     * Checks if this is a validation error (422 status code)
     */
    public boolean isValidationError() {
        return statusCode == 422;
    }

    /**
     * Checks if this is a not found error (404 status code)
     */
    public boolean isNotFound() {
        return statusCode == 404;
    }

    /**
     * Checks if this is an authentication error (401 status code)
     */
    public boolean isAuthenticationError() {
        return statusCode == 401;
    }

    /**
     * Checks if this is an authorization error (403 status code)
     */
    public boolean isAuthorizationError() {
        return statusCode == 403;
    }
}
