package com.monicahq.mcp.config;

import com.monicahq.mcp.client.MonicaHqClient;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Primary;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("test")
public class TestConfig {

    @Bean
    @Primary
    public MonicaHqClient testMonicaHqClient() {
        // Use stubbed client instead of real HTTP client
        return new TestMonicaHqClient();
    }
}