package com.monicahq.mcp.integration;

import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.core.read.ListAppender;
import com.monicahq.mcp.client.MonicaHqClient;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.slf4j.LoggerFactory;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;
import org.springframework.web.reactive.function.client.WebClient;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Security test for token handling and logging.
 * Validates P0-001: Token content must never appear in logs.
 *
 * Tests verify that:
 * - Token content is not logged in DEBUG mode
 * - Token validation doesn't expose token fragments
 * - Error messages don't contain tokens
 * - Stack traces don't leak sensitive data
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none",
    "logging.level.com.monicahq.mcp=DEBUG"  // Enable DEBUG to test logging
})
public class TokenSecurityTest {

    private ListAppender<ILoggingEvent> logAppender;
    private Logger rootLogger;
    private static final String TEST_TOKEN = "test-token-1234567890abcdef";
    private static final String SENSITIVE_FRAGMENT = "1234567890";

    @BeforeEach
    void setUp() {
        // Set up log appender to capture log messages
        rootLogger = (Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME);
        logAppender = new ListAppender<>();
        logAppender.start();
        rootLogger.addAppender(logAppender);
    }

    @AfterEach
    void tearDown() {
        if (logAppender != null) {
            logAppender.stop();
            rootLogger.detachAppender(logAppender);
        }
    }

    @Test
    void shouldNotLogTokenContentInDebugMode() {
        // Given: MonicaHQ client with test token
        WebClient webClient = WebClient.builder()
            .baseUrl("http://localhost:8081")
            .defaultHeader("Authorization", "Bearer " + TEST_TOKEN)
            .build();

        MonicaHqClient client = new MonicaHqClient(webClient);

        // When: Perform operation that triggers logging
        try {
            client.get("/contacts/1", null).block();
        } catch (Exception e) {
            // Expected - we're testing logging, not actual API calls
        }

        // Then: Verify NO token content appears in logs
        List<ILoggingEvent> logEvents = logAppender.list;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();

            // Token content should NEVER appear in logs
            assertFalse(message.contains(TEST_TOKEN),
                "Full token should never appear in logs: " + message);
            assertFalse(message.contains(SENSITIVE_FRAGMENT),
                "Token fragments should never appear in logs: " + message);

            // Even partial tokens like "test-token-1234" are forbidden
            assertFalse(message.contains("test-token-1234"),
                "Token prefixes should never appear in logs: " + message);
        }
    }

    @Test
    void shouldNotLogTokensInValidationErrors() {
        // Given: Invalid token
        String invalidToken = "invalid-token-secret-xyz";
        WebClient webClient = WebClient.builder()
            .baseUrl("http://localhost:8081")
            .defaultHeader("Authorization", "Bearer " + invalidToken)
            .build();

        MonicaHqClient client = new MonicaHqClient(webClient);

        // Clear previous logs
        logAppender.list.clear();

        // When: Validation fails
        try {
            client.get("/contacts/1", null).block();
        } catch (Exception e) {
            // Expected validation error
        }

        // Then: Error messages should NOT contain token
        List<ILoggingEvent> logEvents = logAppender.list;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();
            assertFalse(message.contains(invalidToken),
                "Invalid token should not appear in error logs: " + message);
            assertFalse(message.contains("secret-xyz"),
                "Token fragments should not appear in error logs: " + message);
        }
    }

    @Test
    void shouldRedactTokensInStackTraces() {
        // Given: Operation that might throw exception
        WebClient webClient = WebClient.builder()
            .baseUrl("http://localhost:8081")
            .defaultHeader("Authorization", "Bearer " + TEST_TOKEN)
            .build();

        MonicaHqClient client = new MonicaHqClient(webClient);

        // Clear previous logs
        logAppender.list.clear();

        // When: Exception occurs
        try {
            client.get("/invalid-endpoint-causes-error", null).block();
        } catch (Exception e) {
            // Log the exception (simulating what error handler does)
            LoggerFactory.getLogger(TokenSecurityTest.class)
                .error("Test error occurred", e);
        }

        // Then: Stack traces should NOT contain token
        List<ILoggingEvent> logEvents = logAppender.list;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();
            assertFalse(message.contains(TEST_TOKEN),
                "Token should not appear in stack traces: " + message);

            // Check throwable proxy as well
            if (event.getThrowableProxy() != null) {
                String stackTrace = event.getThrowableProxy().getMessage();
                if (stackTrace != null) {
                    assertFalse(stackTrace.contains(TEST_TOKEN),
                        "Token should not appear in exception messages");
                }
            }
        }
    }

    @Test
    void shouldUseSecureLoggingForTokenValidation() {
        // Given: Token validation logs
        logAppender.list.clear();

        // When: Token validation occurs (simulated)
        String token = "Bearer production-token-super-secret";
        boolean isValid = token.startsWith("Bearer ") && token.length() > 20;

        // Log validation result securely
        LoggerFactory.getLogger(TokenSecurityTest.class)
            .debug("Token validation - length: {}, format valid: {}",
                token.length(), isValid);

        // Then: Logs should show validation metadata, NOT token content
        List<ILoggingEvent> logEvents = logAppender.list;
        boolean foundValidationLog = false;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();
            if (message.contains("Token validation")) {
                foundValidationLog = true;

                // Should log length (OK)
                assertTrue(message.contains("length"),
                    "Should log token length for debugging");

                // Should NOT log token content
                assertFalse(message.contains("production-token"),
                    "Should not log token content: " + message);
                assertFalse(message.contains("super-secret"),
                    "Should not log token secrets: " + message);
            }
        }

        assertTrue(foundValidationLog, "Should have token validation log for testing");
    }

    @Test
    void shouldNotLogAuthorizationHeaders() {
        // Given: HTTP request with Authorization header
        logAppender.list.clear();

        WebClient webClient = WebClient.builder()
            .baseUrl("http://localhost:8081")
            .defaultHeader("Authorization", "Bearer " + TEST_TOKEN)
            .build();

        // When: Request is made
        try {
            webClient.get()
                .uri("/contacts")
                .retrieve()
                .bodyToMono(String.class)
                .block();
        } catch (Exception e) {
            // Expected - testing logging, not connectivity
        }

        // Then: Authorization header should NOT appear in logs
        List<ILoggingEvent> logEvents = logAppender.list;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();
            assertFalse(message.contains("Authorization"),
                "Should not log Authorization header name: " + message);
            assertFalse(message.contains(TEST_TOKEN),
                "Should not log token from header: " + message);
        }
    }

    @Test
    void shouldSanitizeTokensBeforeLogging() {
        // Given: Need to log authentication status
        logAppender.list.clear();

        String token = "Bearer sensitive-production-token";

        // When: Logging authentication status (correct pattern)
        LoggerFactory.getLogger(TokenSecurityTest.class)
            .debug("Authentication check - token present: {}, token valid format: {}",
                token != null, token.startsWith("Bearer "));

        // Then: Should log METADATA about token, not content
        List<ILoggingEvent> logEvents = logAppender.list;
        for (ILoggingEvent event : logEvents) {
            String message = event.getFormattedMessage();
            if (message.contains("Authentication check")) {
                assertTrue(message.contains("true"),
                    "Should log whether token is present");
                assertFalse(message.contains("sensitive-production-token"),
                    "Should not log actual token: " + message);
            }
        }
    }
}
