package com.monicahq.mcp.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.client.AuthInterceptor;
import io.github.resilience4j.circuitbreaker.CircuitBreaker;
import io.github.resilience4j.circuitbreaker.CircuitBreakerConfig;
import io.github.resilience4j.retry.Retry;
import io.github.resilience4j.retry.RetryConfig;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.client.WebClient;

import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;

@Configuration
@RequiredArgsConstructor
@Slf4j
public class ResilienceConfig {

    @Value("${monica.api.timeout:30s}")
    private Duration timeout;
    
    @Value("${monica.api.url}")
    private String apiUrl;

    private final AuthInterceptor authInterceptor;

    @Bean
    public ObjectMapper objectMapper() {
        return new ObjectMapper();
    }

    @Bean
    public WebClient webClient() {
        validateApiUrl(apiUrl);

        return WebClient.builder()
            .baseUrl(apiUrl)
            .filter(authInterceptor.addAuthentication())
            .filter(authInterceptor.logRequests())
            .filter(authInterceptor.logResponses())
            .filter(authInterceptor.handleErrors())
            .codecs(configurer -> configurer.defaultCodecs().maxInMemorySize(1024 * 1024)) // 1MB
            .build();
    }

    private void validateApiUrl(String configuredApiUrl) {
        if (configuredApiUrl == null || configuredApiUrl.trim().isEmpty()) {
            throw new IllegalStateException(
                "Invalid MONICA_API_URL: value is empty. Expected an absolute URL like https://app.monicahq.com/api");
        }

        try {
            URI uri = new URI(configuredApiUrl.trim());
            if (uri.getScheme() == null || uri.getHost() == null) {
                throw new IllegalStateException(
                    "Invalid MONICA_API_URL: '" + configuredApiUrl + "'. Expected an absolute URL with scheme and host, for example https://app.monicahq.com/api or http://monica-app/api");
            }
        } catch (URISyntaxException e) {
            throw new IllegalStateException(
                "Invalid MONICA_API_URL: '" + configuredApiUrl + "'. Expected an absolute URL like https://app.monicahq.com/api",
                e);
        }
    }

    @Bean
    public CircuitBreaker monicaApiCircuitBreaker() {
        CircuitBreakerConfig config = CircuitBreakerConfig.custom()
            .slidingWindowSize(10)
            .minimumNumberOfCalls(5)
            .permittedNumberOfCallsInHalfOpenState(3)
            .waitDurationInOpenState(Duration.ofSeconds(10))
            .failureRateThreshold(50.0f)
            .slowCallRateThreshold(50.0f)
            .slowCallDurationThreshold(Duration.ofSeconds(5))
            .recordExceptions(RuntimeException.class, Exception.class)
            .build();

        CircuitBreaker circuitBreaker = CircuitBreaker.of("monicaApi", config);
        
        circuitBreaker.getEventPublisher()
            .onStateTransition(event -> log.info("MonicaHQ API Circuit Breaker state transition: {}", event))
            .onCallNotPermitted(event -> log.warn("MonicaHQ API call not permitted due to circuit breaker"))
            .onError(event -> log.error("MonicaHQ API error: {}", event.getThrowable().getMessage()));
        
        return circuitBreaker;
    }

    @Bean
    public Retry monicaApiRetry() {
        RetryConfig config = RetryConfig.custom()
            .maxAttempts(3)
            .waitDuration(Duration.ofSeconds(1))
            .retryExceptions(RuntimeException.class)
            .ignoreExceptions(IllegalArgumentException.class)
            .build();

        Retry retry = Retry.of("monicaApi", config);
        
        retry.getEventPublisher()
            .onRetry(event -> log.warn("MonicaHQ API retry attempt {} due to: {}", 
                event.getNumberOfRetryAttempts(), event.getLastThrowable().getMessage()))
            .onError(event -> log.error("MonicaHQ API retry failed after {} attempts", 
                event.getNumberOfRetryAttempts()));
        
        return retry;
    }
}
