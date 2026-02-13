package com.monicahq.mcp.client;

import com.monicahq.mcp.exception.MonicaApiException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.ClientRequest;
import org.springframework.web.reactive.function.client.ClientResponse;
import org.springframework.web.reactive.function.client.ExchangeFilterFunction;
import reactor.core.publisher.Mono;

import java.util.regex.Pattern;

@Component
@RequiredArgsConstructor
@Slf4j
public class AuthInterceptor {

    @Value("${monica.api.token}")
    private String apiToken;

    private static final Pattern BEARER_TOKEN_PATTERN = Pattern.compile("^[A-Za-z0-9\\-._~+/=]+$");

    public ExchangeFilterFunction addAuthentication() {
        return ExchangeFilterFunction.ofRequestProcessor(this::addAuthHeader);
    }

    public ExchangeFilterFunction logRequests() {
        return ExchangeFilterFunction.ofRequestProcessor(request -> {
            log.info("MonicaHQ API Request: {} {}", request.method(), request.url());
            request.headers().forEach((name, values) -> {
                if (!name.equalsIgnoreCase("Authorization")) {
                    log.debug("  Header: {}: {}", name, values);
                }
            });
            return Mono.just(request);
        });
    }

    public ExchangeFilterFunction logResponses() {
        return ExchangeFilterFunction.ofResponseProcessor(response -> {
            log.info("MonicaHQ API Response: {} for {} {}",
                response.statusCode(),
                response.request().getMethod(),
                response.request().getURI());
            return Mono.just(response);
        });
    }

    public ExchangeFilterFunction handleErrors() {
        return ExchangeFilterFunction.ofResponseProcessor(this::handleErrorResponse);
    }

    private Mono<ClientRequest> addAuthHeader(ClientRequest request) {
        if (apiToken == null || apiToken.trim().isEmpty()) {
            log.error("MonicaHQ API token is not configured");
            return Mono.error(new IllegalStateException("MonicaHQ API token is required"));
        }

        if (!isValidBearerToken(apiToken)) {
            log.error("Invalid MonicaHQ API token format");
            return Mono.error(new IllegalArgumentException("Invalid API token format"));
        }

        // Add Bearer token to Authorization header
        ClientRequest authenticatedRequest = ClientRequest.from(request)
            .header(HttpHeaders.AUTHORIZATION, "Bearer " + apiToken)
            .header(HttpHeaders.USER_AGENT, "MonicaHQ-MCP-Server/1.0")
            .build();

        log.debug("Added authentication header for request to: {}", request.url());
        return Mono.just(authenticatedRequest);
    }

    private Mono<ClientResponse> handleErrorResponse(ClientResponse response) {
        if (response.statusCode().is4xxClientError() || response.statusCode().is5xxServerError()) {
            // Extract the response body for detailed error logging
            return response.bodyToMono(String.class)
                .defaultIfEmpty("")
                .flatMap(responseBody -> {
                    int statusCode = response.statusCode().value();

                    log.error("===== MonicaHQ API Error =====");
                    log.error("Request: {} {}", response.request().getMethod(), response.request().getURI());
                    log.error("Status Code: {}", statusCode);
                    log.error("Response Body: {}", responseBody);
                    log.error("==============================");

                    // Build detailed error message based on status code
                    String errorMessage = buildErrorMessage(statusCode, responseBody);

                    // Throw MonicaApiException with full details for upstream error handling
                    return Mono.error(new MonicaApiException(statusCode, errorMessage, responseBody));
                });
        }

        return Mono.just(response);
    }

    /**
     * Builds an appropriate error message based on the HTTP status code and response body.
     *
     * @param statusCode the HTTP status code
     * @param responseBody the raw error response from Monica API
     * @return formatted error message
     */
    private String buildErrorMessage(int statusCode, String responseBody) {
        switch (statusCode) {
            case 401:
                return "Authentication failed: Invalid or expired API token";
            case 403:
                return "Access forbidden: Insufficient permissions";
            case 404:
                return "Resource not found: The requested item does not exist in your MonicaHQ account";
            case 422:
                return enhanceValidationError(responseBody);
            case 429:
                return "Rate limit exceeded";
            default:
                if (statusCode >= 500) {
                    return "MonicaHQ API server error (" + statusCode + ")";
                } else {
                    return "Client error (" + statusCode + ")";
                }
        }
    }

    /**
     * Enhances validation error messages with helpful guidance.
     * Detects common validation errors and provides actionable suggestions.
     *
     * @param responseBody the raw error response from Monica API
     * @return enhanced error message with guidance
     */
    private String enhanceValidationError(String responseBody) {
        // Check for common validation errors and provide helpful guidance
        if (responseBody.contains("contact field type id is invalid")) {
            return "Invalid contactFieldTypeId. " +
                   "Use the contact_field_type_list tool to see available field types in your Monica instance.";
        }

        if (responseBody.contains("contact id") && responseBody.contains("invalid")) {
            return "Invalid contactId. " +
                   "Use the contact_list tool to find valid contact IDs in your Monica instance.";
        }

        if (responseBody.contains("contact id") && responseBody.contains("required")) {
            return "Missing required field: contactId. " +
                   "Use the contact_list tool to find a valid contact ID.";
        }

        // Default: return generic validation error message
        return "Validation error from Monica API";
    }

    private boolean isValidBearerToken(String token) {
        if (token == null || token.trim().isEmpty()) {
            log.debug("Token validation failed: token is null or empty");
            return false;
        }
        
        // Basic validation - Bearer tokens should not contain spaces or special characters
        // and should have reasonable length (between 20 and 2000 characters for JWT tokens)
        String trimmedToken = token.trim();
        boolean lengthValid = trimmedToken.length() >= 20 && trimmedToken.length() <= 2000;
        boolean patternValid = BEARER_TOKEN_PATTERN.matcher(trimmedToken).matches();
        
        log.debug("Token validation - length: {} (valid: {}), pattern match: {}",
                 trimmedToken.length(), lengthValid, patternValid);
        
        return lengthValid && patternValid;
    }

    public boolean validateToken() {
        return isValidBearerToken(apiToken);
    }
}
