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
public class NoteDeleteTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldDeleteNoteViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_delete", "arguments", Map.of("id", 67890)), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.content[0].text").value(text -> text.toString().contains("deleted"));
    }

    @Test
    void shouldValidateDeleteRequiresId() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_delete", "arguments", Map.of()), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest();
    }
}