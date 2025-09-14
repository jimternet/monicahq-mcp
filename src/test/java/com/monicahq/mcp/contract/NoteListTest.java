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
public class NoteListTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldListNotesViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_list", "arguments", Map.of("page", 1, "limit", 10)), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.data").isArray().jsonPath("$.result.meta.page").isEqualTo(1);
    }

    @Test
    void shouldListNotesForContact() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "note_list", "arguments", Map.of("contactId", 12345, "page", 1, "limit", 10)), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.data").isArray();
    }
}