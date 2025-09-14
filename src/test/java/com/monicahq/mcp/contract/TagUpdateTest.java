package com.monicahq.mcp.contract;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.web.reactive.server.WebTestClient;

import java.util.Map;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class TagUpdateTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldUpdateTagViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "tag_update", "arguments", Map.of("id", 11111, "name", "Very Important", "nameSlug", "very-important")), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.data.id").isEqualTo(11111);
    }

    @Test
    void shouldValidateUpdateRequiresId() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "tag_update", "arguments", Map.of("name", "No ID")), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest().expectBody().jsonPath("$.error.code").isEqualTo(-32602);
    }
}
