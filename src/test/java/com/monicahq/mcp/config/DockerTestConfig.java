package com.monicahq.mcp.config;

import com.monicahq.mcp.client.AuthInterceptor;
import com.monicahq.mcp.client.MonicaHqClient;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.web.reactive.function.client.WebClient;

@Configuration
@Profile("docker-test")
public class DockerTestConfig {

    @Bean
    @Primary
    public WebClient dockerWebClient(
            @Value("${monica.api.url}") String apiUrl,
            AuthInterceptor authInterceptor) {
        return WebClient.builder()
            .baseUrl(apiUrl)
            .filter(authInterceptor.addAuthentication())
            .filter(authInterceptor.logRequests())
            .filter(authInterceptor.logResponses())
            .filter(authInterceptor.handleErrors())
            .codecs(configurer -> configurer.defaultCodecs().maxInMemorySize(1024 * 1024))
            .build();
    }

    @Bean
    @Primary
    public MonicaHqClient realMonicaHqClient(WebClient dockerWebClient) {
        // Return real MonicaHqClient that makes actual API calls
        // This replaces the TestMonicaHqClient stub used in unit tests
        return new MonicaHqClient(dockerWebClient);
    }
}
