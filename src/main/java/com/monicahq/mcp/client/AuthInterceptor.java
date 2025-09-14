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

    private static final Pattern BEARER_TOKEN_PATTERN = Pattern.compile("^[A-Za-z0-9\\-._~+/]+=*$");

    public ExchangeFilterFunction addAuthentication() {
        return ExchangeFilterFunction.ofRequestProcessor(this::addAuthHeader);
    }

    public ExchangeFilterFunction logRequests() {
        return ExchangeFilterFunction.ofRequestProcessor(request -> {
            log.debug("Request: {} {}", request.method(), request.url());
            return Mono.just(request);
        });
    }

    public ExchangeFilterFunction logResponses() {
        return ExchangeFilterFunction.ofResponseProcessor(response -> {
            log.debug("Response: {} for {}", response.statusCode(), response.request().getURI());
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
        if (response.statusCode().is4xxClientError()) {
            log.warn("Client error response: {} for {}", response.statusCode(), response.request().getURI());
            
            if (response.statusCode().value() == 401) {
                log.error("Authentication failed - invalid or expired API token");
                return Mono.error(new RuntimeException("Authentication failed: Invalid or expired API token"));
            }
            
            if (response.statusCode().value() == 403) {
                log.error("Access forbidden - insufficient permissions");
                return Mono.error(new RuntimeException("Access forbidden: Insufficient permissions"));
            }
            
            if (response.statusCode().value() == 429) {
                log.warn("Rate limit exceeded for MonicaHQ API");
                return Mono.error(new RuntimeException("Rate limit exceeded"));
            }
        }
        
        if (response.statusCode().is5xxServerError()) {
            log.error("Server error response: {} for {}", response.statusCode(), response.request().getURI());
            return Mono.error(new RuntimeException("MonicaHQ API server error: " + response.statusCode()));
        }
        
        return Mono.just(response);
    }

    private boolean isValidBearerToken(String token) {
        if (token == null || token.trim().isEmpty()) {
            return false;
        }
        
        // Basic validation - Bearer tokens should not contain spaces or special characters
        // and should have reasonable length (between 20 and 500 characters)
        String trimmedToken = token.trim();
        return trimmedToken.length() >= 20 && 
               trimmedToken.length() <= 500 && 
               BEARER_TOKEN_PATTERN.matcher(trimmedToken).matches();
    }

    public boolean validateToken() {
        return isValidBearerToken(apiToken);
    }
}
