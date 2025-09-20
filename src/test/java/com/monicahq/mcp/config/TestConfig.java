package com.monicahq.mcp.config;

import com.monicahq.mcp.client.MonicaHqClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;
import org.springframework.web.reactive.function.client.WebClient;

@Configuration
@Profile("test")
public class TestConfig {

    @Bean
    @Primary
    public WebClient testWebClient() {
        return WebClient.builder().build();
    }

    @Bean
    @Primary
    public MonicaHqClient testMonicaHqClient(WebClient webClient) {
        // Use stubbed client instead of real HTTP client
        return new TestMonicaHqClient(webClient);
    }
}