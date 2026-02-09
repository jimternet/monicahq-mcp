package com.monicahq.mcp.client;

import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.ParameterizedTypeReference;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Map;

/**
 * HTTP client for MonicaHQ API.
 *
 * <p>Error handling for 4xx/5xx responses is centralized in {@link AuthInterceptor#handleErrors()}
 * which is configured as a WebClient filter in ResilienceConfig. This eliminates the need for
 * duplicated onStatus handlers in each HTTP method.</p>
 */
@Component
@RequiredArgsConstructor
@Slf4j
public class MonicaHqClient {

    private static final ParameterizedTypeReference<Map<String, Object>> RESPONSE_TYPE =
            new ParameterizedTypeReference<>() {};

    private final WebClient webClient;

    @Value("${monica.api.timeout:30s}")
    private Duration timeout;

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> get(String endpoint, Map<String, String> queryParams) {
        log.debug("GET request to MonicaHQ API: {}", endpoint);

        WebClient.RequestHeadersSpec<?> request = webClient
            .get()
            .uri(uriBuilder -> {
                uriBuilder.path(endpoint);
                if (queryParams != null) {
                    queryParams.forEach(uriBuilder::queryParam);
                }
                return uriBuilder.build();
            })
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE);

        return executeRequest(request.retrieve(), "GET", endpoint);
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> post(String endpoint, Map<String, Object> requestBody) {
        log.info("POST request to MonicaHQ API: {}", endpoint);
        log.info("Request body: {}", requestBody);

        WebClient.ResponseSpec responseSpec = webClient
            .post()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .bodyValue(requestBody != null ? requestBody : Map.of())
            .retrieve();

        return executeRequest(responseSpec, "POST", endpoint);
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> put(String endpoint, Map<String, Object> requestBody) {
        log.info("PUT request to MonicaHQ API: {}", endpoint);
        log.info("Request body: {}", requestBody);

        WebClient.ResponseSpec responseSpec = webClient
            .put()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .bodyValue(requestBody != null ? requestBody : Map.of())
            .retrieve();

        return executeRequest(responseSpec, "PUT", endpoint);
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> delete(String endpoint) {
        log.debug("DELETE request to MonicaHQ API: {}", endpoint);

        WebClient.ResponseSpec responseSpec = webClient
            .delete()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .retrieve();

        return executeRequest(responseSpec, "DELETE", endpoint);
    }

    /**
     * Executes the HTTP request with centralized timeout, body conversion, and logging.
     *
     * <p>Error handling for 4xx/5xx responses is handled by AuthInterceptor.handleErrors()
     * at the WebClient filter level, so no onStatus handlers are needed here.</p>
     *
     * @param responseSpec the WebClient response specification
     * @param method the HTTP method name for logging
     * @param endpoint the API endpoint for logging
     * @return a Mono containing the response as a Map
     */
    private Mono<Map<String, Object>> executeRequest(
            WebClient.ResponseSpec responseSpec,
            String method,
            String endpoint) {
        return responseSpec
            .bodyToMono(RESPONSE_TYPE)
            .timeout(timeout)
            .doOnSuccess(response -> log.info("{} successful for endpoint: {}", method, endpoint))
            .doOnError(error -> {
                log.error("===== {} FAILED =====", method);
                log.error("Endpoint: {}", endpoint);
                log.error("Error Type: {}", error.getClass().getName());
                log.error("Error Message: {}", error.getMessage());
                if (error.getCause() != null) {
                    log.error("Root Cause: {}", error.getCause().getMessage());
                }
                log.error("========================");
            });
    }

    // Circuit breaker fallback
    @SuppressWarnings("unused")
    private Mono<Map<String, Object>> fallbackResponse(Exception ex) {
        log.error("Circuit breaker activated for MonicaHQ API call: {}", ex.getMessage());
        return Mono.error(new RuntimeException("MonicaHQ API is currently unavailable", ex));
    }

}
