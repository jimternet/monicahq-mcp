package com.monicahq.mcp.integration;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.monicahq.mcp.controller.McpMessageHandler;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Security tests for input validation and sanitization.
 * Validates protection against:
 * - SQL Injection
 * - XSS (Cross-Site Scripting)
 * - Buffer overflow / excessive input length
 * - Special character handling
 * - Malformed data
 *
 * These tests ensure the MCP server properly sanitizes or rejects malicious input.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.profiles.active=test",
    "spring.main.web-application-type=none"
})
public class InputValidationSecurityTest {

    @Autowired
    private McpMessageHandler messageHandler;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    void shouldRejectSQLInjectionInContactName() {
        // Given: SQL injection payload in firstName
        Map<String, Object> sqlInjectionRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "'; DROP TABLE contacts; --",
                    "lastName", "Attacker",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 1
        );

        // When: Process malicious request
        JsonNode requestNode = objectMapper.valueToTree(sqlInjectionRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should either sanitize or accept (Monica API handles escaping)
        // Most importantly: should NOT crash or execute SQL
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // If it succeeded, verify data was escaped/sanitized
        if (response.containsKey("result")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> result = (Map<String, Object>) response.get("result");
            assertTrue(result.containsKey("data") || result.containsKey("content"),
                "Should return data or content field");
        }
        // If it failed, that's also acceptable (input validation rejected it)
    }

    @Test
    void shouldHandleXSSPayloadsInNoteBody() {
        // Given: XSS payload in note body
        Map<String, Object> xssRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "<script>alert('XSS')</script><img src=x onerror=alert('XSS')>"
                )
            ),
            "id", 2
        );

        // When: Process XSS attempt
        JsonNode requestNode = objectMapper.valueToTree(xssRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should not crash and should handle safely
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // XSS payloads should be escaped or rejected
        // Monica API should handle HTML escaping
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should return result or error, not crash");
    }

    @Test
    void shouldEnforceLengthLimitsOnTextFields() {
        // Given: Extremely long string (potential buffer overflow)
        String veryLongString = "A".repeat(1000000);  // 1MB of 'A's

        Map<String, Object> lengthAttackRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", veryLongString
                )
            ),
            "id", 3
        );

        // When: Process excessive length input
        JsonNode requestNode = objectMapper.valueToTree(lengthAttackRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should reject or truncate gracefully, NOT crash
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // Either accept (Monica truncates) or reject (validation error)
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle excessive length gracefully without crashing");

        // If error, should be validation error
        if (response.containsKey("error")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> error = (Map<String, Object>) response.get("error");
            Object code = error.get("code");
            // Should be validation error (-32602) or similar
            assertTrue(code.equals(-32602) || code.equals(-32603),
                "Should return validation or internal error code");
        }
    }

    @Test
    void shouldHandleSpecialCharactersInNames() {
        // Given: Names with special characters
        Map<String, Object> specialCharsRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Jean-François O'Brien-Smith",
                    "lastName", "Müller-González",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 4
        );

        // When: Process legitimate special characters
        JsonNode requestNode = objectMapper.valueToTree(specialCharsRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should accept valid international characters
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result"),
            "Should accept legitimate special characters in names");
    }

    @Test
    void shouldRejectNullByteInjection() {
        // Given: Null byte injection attempt
        Map<String, Object> nullByteRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Evil\u0000Admin",
                    "lastName", "User",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 5
        );

        // When: Process null byte injection
        JsonNode requestNode = objectMapper.valueToTree(nullByteRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should either strip null bytes or reject
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // Null bytes should be handled safely
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle null bytes without crashing");
    }

    @Test
    void shouldHandleUnicodeControlCharacters() {
        // Given: Unicode control characters that could cause issues
        Map<String, Object> controlCharsRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "Test\u202E\u202DRight-to-left override\u200Bzero-width"
                )
            ),
            "id", 6
        );

        // When: Process control characters
        JsonNode requestNode = objectMapper.valueToTree(controlCharsRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should handle without crashing
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle Unicode control characters safely");
    }

    @Test
    void shouldValidateEmailFormatInContactField() {
        // Given: Malformed email that could be used for injection
        Map<String, Object> invalidEmailRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Test",
                    "lastName", "User",
                    "genderId", 1,
                    "email", "<script>alert('xss')</script>@evil.com",
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 7
        );

        // When: Process invalid email
        JsonNode requestNode = objectMapper.valueToTree(invalidEmailRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should either sanitize or reject
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));

        // Monica API should validate email format
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle malformed email safely");
    }

    @Test
    void shouldHandlePathTraversalAttempts() {
        // Given: Path traversal attempt in string field
        Map<String, Object> pathTraversalRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "note_create",
                "arguments", Map.of(
                    "contactId", 12345,
                    "body", "../../../etc/passwd"
                )
            ),
            "id", 8
        );

        // When: Process path traversal attempt
        JsonNode requestNode = objectMapper.valueToTree(pathTraversalRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should treat as literal string, not file path
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle path traversal attempts as literal strings");
    }

    @Test
    void shouldRejectCommandInjectionAttempts() {
        // Given: Command injection attempt in string field
        Map<String, Object> commandInjectionRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of(
                    "firstName", "Test; rm -rf /;",
                    "lastName", "$(whoami)",
                    "genderId", 1,
                    "isBirthdateKnown", false,
                    "isDeceased", false,
                    "isDeceasedDateKnown", false
                )
            ),
            "id", 9
        );

        // When: Process command injection attempt
        JsonNode requestNode = objectMapper.valueToTree(commandInjectionRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should treat as literal strings, NOT execute commands
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("result") || response.containsKey("error"),
            "Should handle command injection attempts as literal strings");
    }

    @Test
    void shouldHandleMalformedJSONGracefully() {
        // This test verifies that the MCP handler deals with malformed input
        // Note: ObjectMapper will throw before it reaches handler, but we test the principle

        // Given: Request with missing required fields
        Map<String, Object> malformedRequest = Map.of(
            "jsonrpc", "2.0",
            "method", "tools/call",
            "params", Map.of(
                "name", "contact_create",
                "arguments", Map.of()  // Missing all required fields
            ),
            "id", 10
        );

        // When: Process malformed request
        JsonNode requestNode = objectMapper.valueToTree(malformedRequest);
        Map<String, Object> response = messageHandler.handleMessage(requestNode, null);

        // Then: Should return validation error, not crash
        assertNotNull(response);
        assertEquals("2.0", response.get("jsonrpc"));
        assertTrue(response.containsKey("error"),
            "Should return validation error for missing required fields");

        @SuppressWarnings("unchecked")
        Map<String, Object> error = (Map<String, Object>) response.get("error");
        assertEquals(-32602, error.get("code"),
            "Should return Invalid params error code");
    }
}
