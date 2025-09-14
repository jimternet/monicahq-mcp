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
public class ContactTagRemoveTest {

    @Autowired
    private WebTestClient webTestClient;

    @Test
    void shouldRemoveTagFromContactViaMcpProtocol() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contacttag_remove", "arguments", Map.of("contactId", 12345, "tagId", 11111)), "id", 1);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.result.content[0].text").value(text -> text.toString().contains("tag removed"));
    }

    @Test
    void shouldValidateRemoveRequiresContactId() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contacttag_remove", "arguments", Map.of("tagId", 11111)), "id", 2);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest().expectBody().jsonPath("$.error.code").isEqualTo(-32602);
    }

    @Test
    void shouldValidateRemoveRequiresTagId() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contacttag_remove", "arguments", Map.of("contactId", 12345)), "id", 3);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isBadRequest().expectBody().jsonPath("$.error.code").isEqualTo(-32602);
    }

    @Test
    void shouldHandleNonExistentTagAssignment() {
        Map<String, Object> mcpRequest = Map.of("jsonrpc", "2.0", "method", "tools/call", "params", Map.of("name", "contacttag_remove", "arguments", Map.of("contactId", 12345, "tagId", 99999)), "id", 4);
        webTestClient.post().uri("/mcp").bodyValue(mcpRequest).exchange().expectStatus().isOk().expectBody().jsonPath("$.error.code").isEqualTo(-32000);
    }
}
