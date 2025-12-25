package com.monicahq.mcp.client;

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
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for MonicaHqClient.
 *
 * <p>Tests the refactored client which uses centralized error handling through
 * AuthInterceptor.handleErrors() at the WebClient filter level. These tests verify:
 * <ul>
 *   <li>Successful requests for all HTTP methods (GET, POST, PUT, DELETE)</li>
 *   <li>4xx error handling (401, 403, 404, 422, 429)</li>
 *   <li>5xx error handling (500, 502, 503)</li>
 *   <li>Timeout scenarios</li>
 * </ul>
 */
@DisplayName("MonicaHqClient Unit Tests")
class MonicaHqClientTest {

    private MockWebServer mockWebServer;
    private MonicaHqClient client;
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

        client = new MonicaHqClient(webClient);
        // Set default timeout
        ReflectionTestUtils.setField(client, "timeout", Duration.ofSeconds(5));
    }

    @AfterEach
    void tearDown() throws IOException {
        mockWebServer.shutdown();
    }

    @Nested
    @DisplayName("Successful Requests")
    class SuccessfulRequests {

        @Test
        @DisplayName("GET request returns response successfully")
        void getRequestReturnsSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {"data": {"id": 123, "first_name": "John", "last_name": "Doe"}}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            // When & Then
            StepVerifier.create(client.get("/contacts/123", null))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(123, data.get("id"));
                    assertEquals("John", data.get("first_name"));
                })
                .verifyComplete();

            // Verify request was made correctly
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("GET", request.getMethod());
            assertTrue(request.getPath().contains("/contacts/123"));
            assertTrue(request.getHeader("Authorization").startsWith("Bearer "));
        }

        @Test
        @DisplayName("GET request with query parameters")
        void getRequestWithQueryParams() throws InterruptedException {
            // Given
            String responseBody = """
                {"data": [{"id": 1}, {"id": 2}], "meta": {"total": 2}}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, String> queryParams = new HashMap<>();
            queryParams.put("page", "1");
            queryParams.put("limit", "10");

            // When & Then
            StepVerifier.create(client.get("/contacts", queryParams))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    assertTrue(response.containsKey("meta"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertTrue(request.getPath().contains("page=1"));
            assertTrue(request.getPath().contains("limit=10"));
        }

        @Test
        @DisplayName("POST request creates resource successfully")
        void postRequestCreatesSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {"data": {"id": 456, "first_name": "Jane", "last_name": "Smith"}}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("first_name", "Jane");
            requestBody.put("last_name", "Smith");
            requestBody.put("gender_id", 1);

            // When & Then
            StepVerifier.create(client.post("/contacts", requestBody))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals(456, data.get("id"));
                    assertEquals("Jane", data.get("first_name"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("/contacts", request.getPath());
            assertTrue(request.getBody().readUtf8().contains("Jane"));
        }

        @Test
        @DisplayName("POST request with null body sends empty object")
        void postRequestWithNullBody() throws InterruptedException {
            // Given
            String responseBody = """
                {"data": {"status": "ok"}}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            // When & Then
            StepVerifier.create(client.post("/endpoint", null))
                .assertNext(response -> assertNotNull(response))
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("POST", request.getMethod());
            assertEquals("{}", request.getBody().readUtf8());
        }

        @Test
        @DisplayName("PUT request updates resource successfully")
        void putRequestUpdatesSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {"data": {"id": 123, "first_name": "Updated", "last_name": "Name"}}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            Map<String, Object> requestBody = new HashMap<>();
            requestBody.put("first_name", "Updated");
            requestBody.put("last_name", "Name");

            // When & Then
            StepVerifier.create(client.put("/contacts/123", requestBody))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                    @SuppressWarnings("unchecked")
                    Map<String, Object> data = (Map<String, Object>) response.get("data");
                    assertEquals("Updated", data.get("first_name"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("PUT", request.getMethod());
            assertEquals("/contacts/123", request.getPath());
        }

        @Test
        @DisplayName("DELETE request deletes resource successfully")
        void deleteRequestDeletesSuccessfully() throws InterruptedException {
            // Given
            String responseBody = """
                {"deleted": true}
                """;
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody(responseBody));

            // When & Then
            StepVerifier.create(client.delete("/contacts/123"))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertEquals(true, response.get("deleted"));
                })
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("DELETE", request.getMethod());
            assertEquals("/contacts/123", request.getPath());
        }

        @Test
        @DisplayName("DELETE request returns 204 No Content successfully")
        void deleteRequestReturns204() throws InterruptedException {
            // Given - 204 No Content with empty body
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(204)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE));

            // When & Then - 204 with empty body returns empty mono
            StepVerifier.create(client.delete("/contacts/123"))
                .verifyComplete();

            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("DELETE", request.getMethod());
        }
    }

    @Nested
    @DisplayName("Client Error Handling (4xx)")
    class ClientErrorHandling {

        @Test
        @DisplayName("401 Unauthorized returns authentication error")
        void unauthorizedReturnsAuthError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(401)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Invalid token"}
                    """));

            // When & Then
            StepVerifier.create(client.get("/contacts", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Authentication failed"))
                .verify();
        }

        @Test
        @DisplayName("403 Forbidden returns access denied error")
        void forbiddenReturnsAccessDenied() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(403)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Access denied"}
                    """));

            // When & Then
            StepVerifier.create(client.get("/admin/settings", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Access forbidden"))
                .verify();
        }

        @Test
        @DisplayName("404 Not Found returns resource not found error")
        void notFoundReturnsResourceNotFound() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(404)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Contact not found"}
                    """));

            // When & Then
            StepVerifier.create(client.get("/contacts/99999", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Resource not found"))
                .verify();
        }

        @Test
        @DisplayName("422 Unprocessable Entity returns validation error")
        void unprocessableEntityReturnsValidationError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(422)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"errors": {"first_name": ["required"]}}
                    """));

            // When & Then
            StepVerifier.create(client.post("/contacts", Map.of()))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Validation error"))
                .verify();
        }

        @Test
        @DisplayName("429 Too Many Requests returns rate limit error")
        void tooManyRequestsReturnsRateLimitError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(429)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setHeader("Retry-After", "60")
                .setBody("""
                    {"error": "Rate limit exceeded"}
                    """));

            // When & Then
            StepVerifier.create(client.get("/contacts", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("Rate limit exceeded"))
                .verify();
        }

    }

    @Nested
    @DisplayName("Server Error Handling (5xx)")
    class ServerErrorHandling {

        @Test
        @DisplayName("500 Internal Server Error returns server error")
        void internalServerErrorReturnsServerError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(500)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Internal server error"}
                    """));

            // When & Then
            StepVerifier.create(client.get("/contacts", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }

        @Test
        @DisplayName("502 Bad Gateway returns server error")
        void badGatewayReturnsServerError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(502)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Bad gateway"}
                    """));

            // When & Then
            StepVerifier.create(client.post("/contacts", Map.of("first_name", "Test")))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }

        @Test
        @DisplayName("503 Service Unavailable returns server error")
        void serviceUnavailableReturnsServerError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(503)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Service unavailable"}
                    """));

            // When & Then
            StepVerifier.create(client.put("/contacts/123", Map.of("first_name", "Updated")))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }

        @Test
        @DisplayName("504 Gateway Timeout returns server error")
        void gatewayTimeoutReturnsServerError() {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(504)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"error": "Gateway timeout"}
                    """));

            // When & Then
            StepVerifier.create(client.delete("/contacts/123"))
                .expectErrorMatches(throwable ->
                    throwable instanceof RuntimeException &&
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Timeout Handling")
    class TimeoutHandling {

        @Test
        @DisplayName("Request times out when server takes too long")
        void requestTimesOutWhenServerIsSlow() {
            // Given - Set a short timeout for testing
            ReflectionTestUtils.setField(client, "timeout", Duration.ofMillis(100));

            // Server delays response beyond timeout
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"data": {"id": 123}}
                    """)
                .setBodyDelay(500, TimeUnit.MILLISECONDS));

            // When & Then
            StepVerifier.create(client.get("/contacts/123", null))
                .expectErrorMatches(throwable ->
                    throwable instanceof java.util.concurrent.TimeoutException ||
                    throwable.getCause() instanceof java.util.concurrent.TimeoutException)
                .verify(Duration.ofSeconds(2));
        }

        @Test
        @DisplayName("Request completes when server responds within timeout")
        void requestCompletesWithinTimeout() {
            // Given - Set timeout that allows response
            ReflectionTestUtils.setField(client, "timeout", Duration.ofSeconds(2));

            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"data": {"id": 123}}
                    """)
                .setBodyDelay(100, TimeUnit.MILLISECONDS));

            // When & Then
            StepVerifier.create(client.get("/contacts/123", null))
                .assertNext(response -> {
                    assertNotNull(response);
                    assertTrue(response.containsKey("data"));
                })
                .verifyComplete();
        }

        @Test
        @DisplayName("POST request times out correctly")
        void postRequestTimesOut() {
            // Given
            ReflectionTestUtils.setField(client, "timeout", Duration.ofMillis(100));

            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(201)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("""
                    {"data": {"id": 456}}
                    """)
                .setBodyDelay(500, TimeUnit.MILLISECONDS));

            // When & Then
            StepVerifier.create(client.post("/contacts", Map.of("first_name", "Test")))
                .expectErrorMatches(throwable ->
                    throwable instanceof java.util.concurrent.TimeoutException ||
                    throwable.getCause() instanceof java.util.concurrent.TimeoutException)
                .verify(Duration.ofSeconds(2));
        }
    }

    @Nested
    @DisplayName("Error Handling for All HTTP Methods")
    class ErrorHandlingAllMethods {

        @Test
        @DisplayName("GET handles 404 error correctly")
        void getHandles404() {
            mockWebServer.enqueue(new MockResponse().setResponseCode(404));

            StepVerifier.create(client.get("/contacts/99999", null))
                .expectErrorMatches(throwable ->
                    throwable.getMessage().contains("Resource not found"))
                .verify();
        }

        @Test
        @DisplayName("POST handles 401 error correctly")
        void postHandles401() {
            mockWebServer.enqueue(new MockResponse().setResponseCode(401));

            StepVerifier.create(client.post("/contacts", Map.of("first_name", "Test")))
                .expectErrorMatches(throwable ->
                    throwable.getMessage().contains("Authentication failed"))
                .verify();
        }

        @Test
        @DisplayName("PUT handles 403 error correctly")
        void putHandles403() {
            mockWebServer.enqueue(new MockResponse().setResponseCode(403));

            StepVerifier.create(client.put("/contacts/123", Map.of("first_name", "Updated")))
                .expectErrorMatches(throwable ->
                    throwable.getMessage().contains("Access forbidden"))
                .verify();
        }

        @Test
        @DisplayName("DELETE handles 500 error correctly")
        void deleteHandles500() {
            mockWebServer.enqueue(new MockResponse().setResponseCode(500));

            StepVerifier.create(client.delete("/contacts/123"))
                .expectErrorMatches(throwable ->
                    throwable.getMessage().contains("MonicaHQ API server error"))
                .verify();
        }
    }

    @Nested
    @DisplayName("Request Headers")
    class RequestHeaders {

        @Test
        @DisplayName("GET request includes correct headers")
        void getRequestIncludesCorrectHeaders() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"data\": {}}"));

            // When
            client.get("/contacts", null).block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertEquals("application/json", request.getHeader(HttpHeaders.CONTENT_TYPE));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
            assertEquals("MonicaHQ-MCP-Server/1.0", request.getHeader(HttpHeaders.USER_AGENT));
        }

        @Test
        @DisplayName("POST request includes correct headers")
        void postRequestIncludesCorrectHeaders() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"data\": {}}"));

            // When
            client.post("/contacts", Map.of("first_name", "Test")).block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertEquals("application/json", request.getHeader(HttpHeaders.CONTENT_TYPE));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
        }

        @Test
        @DisplayName("PUT request includes correct headers")
        void putRequestIncludesCorrectHeaders() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"data\": {}}"));

            // When
            client.put("/contacts/123", Map.of("first_name", "Updated")).block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertEquals("application/json", request.getHeader(HttpHeaders.CONTENT_TYPE));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
        }

        @Test
        @DisplayName("DELETE request includes correct headers")
        void deleteRequestIncludesCorrectHeaders() throws InterruptedException {
            // Given
            mockWebServer.enqueue(new MockResponse()
                .setResponseCode(200)
                .setHeader(HttpHeaders.CONTENT_TYPE, MediaType.APPLICATION_JSON_VALUE)
                .setBody("{\"deleted\": true}"));

            // When
            client.delete("/contacts/123").block();

            // Then
            RecordedRequest request = mockWebServer.takeRequest();
            assertEquals("application/json", request.getHeader(HttpHeaders.ACCEPT));
            assertTrue(request.getHeader(HttpHeaders.AUTHORIZATION).startsWith("Bearer "));
        }
    }
}
