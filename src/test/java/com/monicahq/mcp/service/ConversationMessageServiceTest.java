package com.monicahq.mcp.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.client.AuthInterceptor;
import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import okhttp3.mockwebserver.MockResponse;
import okhttp3.mockwebserver.MockWebServer;
import okhttp3.mockwebserver.RecordedRequest;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.test.util.ReflectionTestUtils;
import org.springframework.web.reactive.function.client.WebClient;
import reactor.test.StepVerifier;

import java.io.IOException;
import java.time.Duration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for ConversationMessageService using MockWebServer.
 *
 * <p>Tests the ConversationMessageService which manages conversation messages in MonicaHQ.
 * This service includes a critical WORKAROUND for the listConversationMessages() method:
 * instead of using GET /conversations/{id}/messages (which returns HTTP 405), it extracts
 * messages from the GET /conversations/{id} response. These tests verify:
 * <ul>
 *   <li>Create message with correct API mapping</li>
 *   <li>Get message by ID</li>
 *   <li>Update message</li>
 *   <li>Delete message</li>
 *   <li>List messages via conversation extraction workaround</li>
 *   <li>Null/empty message handling in extraction</li>
 *   <li>Validation error handling</li>
 * </ul>
 *
 * @see ConversationMessageService#listConversationMessages(Map)
 */
@DisplayName("ConversationMessageService Unit Tests")
class ConversationMessageServiceTest {

    private MockWebServer mockWebServer;
    private MonicaHqClient monicaClient;
    private ConversationMessageService conversationMessageService;
    private ContentFormatter contentFormatter;
    private AuthInterceptor authInterceptor;

    @BeforeEach
    void setUp() throws IOException {
        mockWebServer = new MockWebServer();
        mockWebServer.start();

        // Create a mock AuthInterceptor that adds authentication and handles errors
        authInterceptor = new AuthInterceptor();
        // Set a valid token (20+ characters as required by isValidBearerToken)
        ReflectionTestUtils.setField(authInterceptor, "apiToken",
            "valid-test-token-that-is-long-enough-for-validation");

        // Create WebClient with error handling filter
        WebClient webClient = WebClient.builder()
            .baseUrl(mockWebServer.url("/").toString())
            .filter(authInterceptor.addAuthentication())
            .filter(authInterceptor.handleErrors())
            .build();

        monicaClient = new MonicaHqClient(webClient);
        // Set default timeout
        ReflectionTestUtils.setField(monicaClient, "timeout", Duration.ofSeconds(5));

        // Create real ContentFormatter with ObjectMapper
        ObjectMapper objectMapper = new ObjectMapper();
        contentFormatter = new ContentFormatter(objectMapper);

        // Create ConversationMessageService with real dependencies
        conversationMessageService = new ConversationMessageService(monicaClient, contentFormatter);
    }

    @AfterEach
    void tearDown() throws IOException {
        mockWebServer.shutdown();
    }

    @Nested
    @DisplayName("Create Conversation Message")
    class CreateConversationMessage {

        @Test
        @DisplayName("Creates message successfully with required fields")
        void createMessageSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Hello, how are you?",
                        "written_at": "2024-01-15T10:00:00Z",
                        "written_by_me": true,
                        "created_at": "2024-01-15T10:00:00Z",
                        "updated_at": "2024-01-15T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("content", "Hello, how are you?");
            arguments.put("writtenAt", "2024-01-15T10:00:00Z");
            arguments.put("writtenByMe", true);

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("Hello, how are you?", data.get("content"));
                    assertEquals(true, data.get("writtenByMe"));
                })
                .verifyComplete();

            // Verify request was made correctly
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("/conversations/456/messages", request.getPath());

            String requestBody = request.getBody().readUtf8();
            assertTrue(requestBody.contains("\"written_at\""));
            assertTrue(requestBody.contains("\"written_by_me\""));
            assertTrue(requestBody.contains("\"content\""));
        }

        @Test
        @DisplayName("Creates message without optional content")
        void createMessageWithoutContent() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 124,
                        "conversation_id": 456,
                        "content": null,
                        "written_at": "2024-01-15T10:00:00Z",
                        "written_by_me": false
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("writtenAt", "2024-01-15T10:00:00Z");
            arguments.put("writtenByMe", false);

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("/conversations/456/messages", request.getPath());
        }

        @Test
        @DisplayName("Fails when conversationId is missing")
        void createMessageWithoutConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("writtenAt", "2024-01-15T10:00:00Z");
            arguments.put("writtenByMe", true);

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("conversationId is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when writtenAt is missing")
        void createMessageWithoutWrittenAtFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("writtenByMe", true);

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("writtenAt is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when writtenByMe is missing")
        void createMessageWithoutWrittenByMeFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("writtenAt", "2024-01-15T10:00:00Z");

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("writtenByMe is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when arguments are null")
        void createMessageWithNullArgumentsFails() {
            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(null))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("cannot be empty"))
                .verify();
        }

        @Test
        @DisplayName("Fails when arguments are empty")
        void createMessageWithEmptyArgumentsFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("cannot be empty"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Get Conversation Message")
    class GetConversationMessage {

        @Test
        @DisplayName("Gets message by ID successfully")
        void getMessageSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Hello, how are you?",
                        "written_at": "2024-01-15T10:00:00Z",
                        "written_by_me": true,
                        "created_at": "2024-01-15T10:00:00Z",
                        "updated_at": "2024-01-15T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("Hello, how are you?", data.get("content"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("GET", request.getMethod());
            assertEquals("/conversations/456/messages/123", request.getPath());
        }

        @Test
        @DisplayName("Gets message with string IDs")
        void getMessageWithStringIds() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Test message"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", "456"); // String ID
            arguments.put("id", "123"); // String ID

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("/conversations/456/messages/123", request.getPath());
        }

        @Test
        @DisplayName("Fails when conversationId is missing")
        void getMessageWithoutConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Conversation ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when message ID is missing")
        void getMessageWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Message ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when conversationId is invalid")
        void getMessageWithInvalidConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", "invalid");
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Invalid conversation ID format"))
                .verify();
        }

        @Test
        @DisplayName("Fails when message ID is invalid")
        void getMessageWithInvalidIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", "invalid");

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Invalid message ID format"))
                .verify();
        }

        @Test
        @DisplayName("Handles 404 not found error")
        void getMessageNotFound() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(404)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Message not found\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 99999);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Resource not found"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Update Conversation Message")
    class UpdateConversationMessage {

        @Test
        @DisplayName("Updates message successfully")
        void updateMessageSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Updated content",
                        "written_at": "2024-01-16T10:00:00Z",
                        "written_by_me": true,
                        "updated_at": "2024-01-16T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);
            arguments.put("content", "Updated content");
            arguments.put("writtenAt", "2024-01-16T10:00:00Z");
            arguments.put("writtenByMe", true);

            // When & Then
            StepVerifier.create(conversationMessageService.updateConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("Updated content", data.get("content"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("PUT", request.getMethod());
            assertEquals("/conversations/456/messages/123", request.getPath());

            String requestBody = request.getBody().readUtf8();
            assertTrue(requestBody.contains("\"written_at\""));
            assertTrue(requestBody.contains("\"written_by_me\""));
            assertTrue(requestBody.contains("\"content\""));
            // ID and conversationId should be removed from the request body
            assertFalse(requestBody.contains("\"id\""));
            assertFalse(requestBody.contains("\"conversationId\""));
        }

        @Test
        @DisplayName("Fails when conversationId is missing for update")
        void updateMessageWithoutConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);
            arguments.put("content", "New content");

            // When & Then
            StepVerifier.create(conversationMessageService.updateConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Conversation ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when ID is missing for update")
        void updateMessageWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("content", "New content");

            // When & Then
            StepVerifier.create(conversationMessageService.updateConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Message ID is required"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Delete Conversation Message")
    class DeleteConversationMessage {

        @Test
        @DisplayName("Deletes message successfully")
        void deleteMessageSuccessfully() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"deleted\": true}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.deleteConversationMessage(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
                    assertEquals("text", content.get(0).get("type"));
                    // The delete response contains a formatted success message
                    String text = content.get(0).get("text").toString();
                    assertTrue(text.contains("Delete") || text.contains("Success") || text.contains("123"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("DELETE", request.getMethod());
            assertEquals("/conversations/456/messages/123", request.getPath());
        }

        @Test
        @DisplayName("Fails when conversationId is missing for delete")
        void deleteMessageWithoutConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.deleteConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Conversation ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Fails when ID is missing for delete")
        void deleteMessageWithoutIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.deleteConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Message ID is required"))
                .verify();
        }
    }

    @Nested
    @DisplayName("List Conversation Messages (Workaround)")
    class ListConversationMessages {

        @Test
        @DisplayName("Extracts messages from conversation GET response")
        void listMessagesExtractsFromConversation() throws InterruptedException {
            // Given - Response from GET /conversations/{id} with embedded messages
            String responseBody = """
                {
                    "data": {
                        "id": 456,
                        "contact_id": 789,
                        "happened_at": "2024-01-15T10:00:00Z",
                        "messages": [
                            {
                                "id": 1,
                                "content": "Hello",
                                "written_at": "2024-01-15T10:00:00Z",
                                "written_by_me": true
                            },
                            {
                                "id": 2,
                                "content": "Hi there!",
                                "written_at": "2024-01-15T10:01:00Z",
                                "written_by_me": false
                            }
                        ]
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("content"));
                    assertTrue(response.containsKey("meta"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertEquals(2, data.size());

                    // Verify first message is mapped correctly
                    Map<String, Object> firstMessage = data.get(0);
                    assertEquals(1, firstMessage.get("id"));
                    assertEquals("Hello", firstMessage.get("content"));
                    assertEquals(true, firstMessage.get("writtenByMe"));

                    // Verify meta contains source info
                    @SuppressWarnings("unchecked")
                    Map<String, Object> meta = (Map<String, Object>) response.get("meta");
                    assertEquals(2, meta.get("total"));
                    assertEquals("extracted_from_conversation", meta.get("source"));
                })
                .verifyComplete();

            // Verify the workaround uses GET /conversations/{id}, NOT /conversations/{id}/messages
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("GET", request.getMethod());
            assertEquals("/conversations/456", request.getPath());
            assertFalse(request.getPath().contains("/messages"));
        }

        @Test
        @DisplayName("Handles conversation with no messages")
        void listMessagesHandlesNoMessages() throws InterruptedException {
            // Given - Conversation with empty messages array
            String responseBody = """
                {
                    "data": {
                        "id": 456,
                        "contact_id": 789,
                        "messages": []
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    assertNotNull(response);

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertTrue(data.isEmpty());

                    @SuppressWarnings("unchecked")
                    Map<String, Object> meta = (Map<String, Object>) response.get("meta");
                    assertEquals(0, meta.get("total"));
                })
                .verifyComplete();
        }

        @Test
        @DisplayName("Handles conversation with null messages field")
        void listMessagesHandlesNullMessages() throws InterruptedException {
            // Given - Conversation with no messages field
            String responseBody = """
                {
                    "data": {
                        "id": 456,
                        "contact_id": 789,
                        "happened_at": "2024-01-15T10:00:00Z"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    assertNotNull(response);

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertTrue(data.isEmpty());

                    @SuppressWarnings("unchecked")
                    Map<String, Object> meta = (Map<String, Object>) response.get("meta");
                    assertEquals(0, meta.get("total"));
                })
                .verifyComplete();
        }

        @Test
        @DisplayName("Handles conversation with null data")
        void listMessagesHandlesNullData() throws InterruptedException {
            // Given - Response with null data
            String responseBody = """
                {
                    "data": null
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then - Should handle gracefully with empty list
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    assertNotNull(response);

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    assertTrue(data.isEmpty());
                })
                .verifyComplete();
        }

        @Test
        @DisplayName("Correctly maps snake_case to camelCase")
        void listMessagesMapsCasing() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 456,
                        "messages": [
                            {
                                "id": 1,
                                "written_at": "2024-01-15T10:00:00Z",
                                "written_by_me": true,
                                "created_at": "2024-01-14T10:00:00Z",
                                "updated_at": "2024-01-15T10:00:00Z"
                            }
                        ]
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> data = (List<Map<String, Object>>) response.get("data");
                    Map<String, Object> message = data.get(0);

                    // Verify camelCase field names
                    assertEquals("2024-01-15T10:00:00Z", message.get("writtenAt"));
                    assertEquals(true, message.get("writtenByMe"));
                    assertEquals("2024-01-14T10:00:00Z", message.get("createdAt"));
                    assertEquals("2024-01-15T10:00:00Z", message.get("updatedAt"));

                    // Snake_case should not be present
                    assertFalse(message.containsKey("written_at"));
                    assertFalse(message.containsKey("written_by_me"));
                })
                .verifyComplete();
        }

        @Test
        @DisplayName("Fails when conversationId is missing")
        void listMessagesWithoutConversationIdFails() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof IllegalArgumentException &&
                    throwable.getMessage().contains("Conversation ID is required"))
                .verify();
        }

        @Test
        @DisplayName("Response includes content field for Claude Desktop")
        void listMessagesIncludesContentField() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 456,
                        "messages": [
                            {
                                "id": 1,
                                "content": "Test message"
                            }
                        ]
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);

            // When & Then
            StepVerifier.create(conversationMessageService.listConversationMessages(arguments))
                .assertNext(response -> {
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
                    assertEquals(1, content.size());
                    assertEquals("text", content.get(0).get("type"));

                    String text = (String) content.get(0).get("text");
                    assertNotNull(text);
                    // Should contain escaped JSON
                    assertTrue(text.contains("Test message") || text.contains("1"));
                })
                .verifyComplete();
        }
    }

    @Nested
    @DisplayName("API Error Handling")
    class ApiErrorHandling {

        @Test
        @DisplayName("Handles 401 authentication error")
        void handles401Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(401)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Invalid token\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Authentication failed"))
                .verify();
        }

        @Test
        @DisplayName("Handles 422 validation error")
        void handles422Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(422)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"errors\": {\"written_at\": [\"required\"]}}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("writtenAt", "invalid-date");
            arguments.put("writtenByMe", true);

            // When & Then
            StepVerifier.create(conversationMessageService.createConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Validation error"))
                .verify();
        }

        @Test
        @DisplayName("Handles 500 server error")
        void handles500Error() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"error\": \"Internal server error\"}"));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Request Headers")
    class RequestHeaders {

        @Test
        @DisplayName("Includes correct headers in requests")
        void includesCorrectHeaders() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Hello"
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When
            conversationMessageService.getConversationMessage(arguments).block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertEquals("application/json", request.getHeader(HttpHeaders.CONTENT_TYPE));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
            assertEquals("MonicaHQ-MCP-Server/1.0", request.getHeader(HttpHeaders.USER_AGENT));
        }
    }

    @Nested
    @DisplayName("Content Formatting")
    class ContentFormattingTests {

        @Test
        @DisplayName("Response includes formatted content field")
        void responseIncludesFormattedContent() throws InterruptedException {
            // Given
            String responseBody = """
                {
                    "data": {
                        "id": 123,
                        "conversation_id": 456,
                        "content": "Hello, World!",
                        "written_at": "2024-01-15T10:00:00Z",
                        "written_by_me": true
                    }
                }
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> arguments = new HashMap<>();
            arguments.put("conversationId", 456);
            arguments.put("id", 123);

            // When & Then
            StepVerifier.create(conversationMessageService.getConversationMessage(arguments))
                .assertNext(response -> {
                    assertTrue(response.containsKey("content"));

                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> content = (List<Map<String, Object>>) response.get("content");
                    assertEquals(1, content.size());
                    assertEquals("text", content.get(0).get("type"));

                    String text = (String) content.get(0).get("text");
                    assertNotNull(text);
                    // Content should be escaped JSON containing the message data
                    assertTrue(text.contains("Hello") || text.contains("123"));
                })
                .verifyComplete();
        }
    }
}
