package com.monicahq.mcp.client;

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
                    String errorType = response.statusCode().is4xxClientError() ? "Client error" : "Server error";

                    log.error("===== MonicaHQ API Error =====");
                    log.error("Request: {} {}", response.request().getMethod(), response.request().getURI());
                    log.error("Status Code: {}", response.statusCode().value());
                    log.error("Response Body: {}", responseBody);
                    log.error("==============================");

                    // Handle specific error codes with detailed messages
                    int statusCode = response.statusCode().value();
                    String errorMessage;

                    switch (statusCode) {
                        case 401:
                            errorMessage = "Authentication failed: Invalid or expired API token";
                            break;
                        case 403:
                            errorMessage = "Access forbidden: Insufficient permissions";
                            break;
                        case 404:
                            errorMessage = "Resource not found: The requested item does not exist in your MonicaHQ account";
                            break;
                        case 422:
                            errorMessage = "Validation error from Monica API: " + responseBody;
                            break;
                        case 429:
                            errorMessage = "Rate limit exceeded";
                            break;
                        default:
                            if (response.statusCode().is5xxServerError()) {
                                errorMessage = "MonicaHQ API server error (" + statusCode + "): " + responseBody;
                            } else {
                                errorMessage = errorType + " (" + statusCode + "): " + responseBody;
                            }
                    }

                    return Mono.error(new RuntimeException(errorMessage));
                });
        }

        return Mono.just(response);
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
