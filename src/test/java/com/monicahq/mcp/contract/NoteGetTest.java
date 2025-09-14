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
public class NoteGetTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldGetNoteByIdViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_get", "arguments", Map.of("id", 67890)), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.data.id").isEqualTo(67890);
    }

    @Test
    void shouldHandleNoteNotFound() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_get", "arguments", Map.of("id", 99999)), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.error.code").isEqualTo(-32000);
    }
}