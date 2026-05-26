package com.monicahq.mcp.config;

import com.monicahq.mcp.client.AuthInterceptor;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.ExchangeFilterFunction;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.core.publisher.Mono;

import java.time.Duration;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
@DisplayName("ResilienceConfig Unit Tests")
class ResilienceConfigTest {

    @Mock
    private AuthInterceptor authInterceptor;

    private ResilienceConfig resilienceConfig;

    @BeforeEach
    void setUp() {
        resilienceConfig = new ResilienceConfig(authInterceptor);
        ReflectionTestUtils.setField(resilienceConfig, "timeout", Duration.ofSeconds(30));
    }

    @Test
    @DisplayName("Should create WebClient for valid MONICA_API_URL")
    void shouldCreateWebClientForValidApiUrl() {
        ReflectionTestUtils.setField(resilienceConfig, "apiUrl", "http://monica-app/api");
        stubAuthFilters();

        WebClient webClient = resilienceConfig.webClient();

        assertNotNull(webClient);
    }

    @Test
    @DisplayName("Should fail fast when MONICA_API_URL host is invalid")
    void shouldFailFastWhenApiUrlHostIsInvalid() {
        ReflectionTestUtils.setField(resilienceConfig, "apiUrl", "http://monica_app/api");

        IllegalStateException exception = assertThrows(IllegalStateException.class, resilienceConfig::webClient);

        assertTrue(exception.getMessage().contains("Invalid MONICA_API_URL"));
    }

    private void stubAuthFilters() {
        ExchangeFilterFunction requestPassThrough = ExchangeFilterFunction.ofRequestProcessor(Mono::just);
        ExchangeFilterFunction responsePassThrough = ExchangeFilterFunction.ofResponseProcessor(Mono::just);
        when(authInterceptor.addAuthentication()).thenReturn(requestPassThrough);
        when(authInterceptor.logRequests()).thenReturn(requestPassThrough);
        when(authInterceptor.logResponses()).thenReturn(responsePassThrough);
        when(authInterceptor.handleErrors()).thenReturn(responsePassThrough);
    }
}
