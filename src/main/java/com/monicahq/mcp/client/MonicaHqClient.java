package com.monicahq.mcp.client;

import io.github.resilience4j.circuitbreaker.annotation.CircuitBreaker;
import io.github.resilience4j.retry.annotation.Retry;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Component;
import org.springframework.web.reactive.function.client.WebClient;
import org.springframework.web.reactive.function.client.WebClientResponseException;
import reactor.core.publisher.Mono;

import java.time.Duration;
import java.util.Map;

@Component
@RequiredArgsConstructor
@Slf4j
public class MonicaHqClient {

    private final WebClient webClient;
    
    @Value("${monica.api.url}")
    private String apiUrl;
    
    @Value("${monica.api.token}")
    private String apiToken;
    
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

        return request
            .retrieve()
            .onStatus(status -> status.is4xxClientError(), response -> {
                log.error("Client error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return response.bodyToMono(String.class)
                    .flatMap(body -> Mono.error(new RuntimeException("Client error: " + body)));
            })
            .onStatus(status -> status.is5xxServerError(), response -> {
                log.error("Server error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return Mono.error(new RuntimeException("Server error"));
            })
            .bodyToMono(new org.springframework.core.ParameterizedTypeReference<Map<String, Object>>() {})
            .timeout(timeout)
            .doOnSuccess(response -> log.debug("GET successful for endpoint: {}", endpoint))
            .doOnError(error -> log.error("GET failed for endpoint {}: {}", endpoint, error.getMessage()));
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> post(String endpoint, Map<String, Object> requestBody) {
        log.debug("POST request to MonicaHQ API: {} with body: {}", endpoint, requestBody);
        
        return webClient
            .post()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .bodyValue(requestBody != null ? requestBody : Map.of())
            .retrieve()
            .onStatus(status -> status.is4xxClientError(), response -> {
                return response.bodyToMono(String.class)
                    .flatMap(body -> {
                        log.error("Client error from MonicaHQ API on POST {}: Status {}, Request: {}, Response: {}", 
                            endpoint, response.statusCode(), requestBody, body);
                        String errorMessage = String.format("MonicaHQ API error (POST %s, status %d): %s", 
                            endpoint, response.statusCode().value(), body);
                        return Mono.error(new RuntimeException(errorMessage));
                    });
            })
            .onStatus(status -> status.is5xxServerError(), response -> {
                log.error("Server error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return Mono.error(new RuntimeException("MonicaHQ server error (status " + response.statusCode() + ")"));
            })
            .bodyToMono(new org.springframework.core.ParameterizedTypeReference<Map<String, Object>>() {})
            .timeout(timeout)
            .doOnSuccess(response -> log.debug("POST successful for endpoint: {}", endpoint))
            .doOnError(error -> log.error("POST failed for endpoint {} with request {}: {}", endpoint, requestBody, error.getMessage()));
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> put(String endpoint, Map<String, Object> requestBody) {
        log.debug("PUT request to MonicaHQ API: {} with body: {}", endpoint, requestBody);
        
        return webClient
            .put()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .header(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
            .bodyValue(requestBody != null ? requestBody : Map.of())
            .retrieve()
            .onStatus(status -> status.is4xxClientError(), response -> {
                return response.bodyToMono(String.class)
                    .flatMap(body -> {
                        log.error("Client error from MonicaHQ API on PUT {}: Status {}, Request: {}, Response: {}", 
                            endpoint, response.statusCode(), requestBody, body);
                        String errorMessage = String.format("MonicaHQ API error (PUT %s, status %d): %s", 
                            endpoint, response.statusCode().value(), body);
                        return Mono.error(new RuntimeException(errorMessage));
                    });
            })
            .onStatus(status -> status.is5xxServerError(), response -> {
                log.error("Server error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return Mono.error(new RuntimeException("MonicaHQ server error (status " + response.statusCode() + ")"));
            })
            .bodyToMono(new org.springframework.core.ParameterizedTypeReference<Map<String, Object>>() {})
            .timeout(timeout)
            .doOnSuccess(response -> log.debug("PUT successful for endpoint: {}", endpoint))
            .doOnError(error -> log.error("PUT failed for endpoint {} with request {}: {}", endpoint, requestBody, error.getMessage()));
    }

    @CircuitBreaker(name = "monicaApi", fallbackMethod = "fallbackResponse")
    @Retry(name = "monicaApi")
    public Mono<Map<String, Object>> delete(String endpoint) {
        log.debug("DELETE request to MonicaHQ API: {}", endpoint);
        
        return webClient
            .delete()
            .uri(endpoint)
            .header(HttpHeaders.ACCEPT, MediaType.APPLICATION_JSON_VALUE)
            .retrieve()
            .onStatus(status -> status.is4xxClientError(), response -> {
                log.error("Client error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return response.bodyToMono(String.class)
                    .flatMap(body -> Mono.error(new RuntimeException("Client error: " + body)));
            })
            .onStatus(status -> status.is5xxServerError(), response -> {
                log.error("Server error from MonicaHQ API: {} {}", response.statusCode(), endpoint);
                return Mono.error(new RuntimeException("Server error"));
            })
            .bodyToMono(new org.springframework.core.ParameterizedTypeReference<Map<String, Object>>() {})
            .timeout(timeout)
            .doOnSuccess(response -> log.debug("DELETE successful for endpoint: {}", endpoint))
            .doOnError(error -> log.error("DELETE failed for endpoint {}: {}", endpoint, error.getMessage()));
    }

    // Circuit breaker fallback
    @SuppressWarnings("unused")
    private Mono<Map<String, Object>> fallbackResponse(Exception ex) {
        log.error("Circuit breaker activated for MonicaHQ API call: {}", ex.getMessage());
        return Mono.error(new RuntimeException("MonicaHQ API is currently unavailable", ex));
    }

}
