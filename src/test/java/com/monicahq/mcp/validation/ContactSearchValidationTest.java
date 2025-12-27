package com.monicahq.mcp.validation;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.ContactService;
import com.monicahq.mcp.service.config.ContactFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Validation tests for contact search operation data integrity and error handling.
 */
@ExtendWith(MockitoExtension.class)
class ContactSearchValidationTest {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ContactService contactService;

    @BeforeEach
    void setUp() {
        ContactFieldMappingConfig config = new ContactFieldMappingConfig();
        contactService = new ContactService(monicaClient, contentFormatter, config);
    }

    @Test
    void contactSearch_ValidArguments_ShouldReturnStructuredResponse() {
        // Given
        Map<String, Object> arguments = Map.of(
            "query", "John Doe",
            "limit", 25
        );
        
        Map<String, Object> mockResponse = Map.of(
            "data", List.of(
                Map.of("id", 1L, "first_name", "John", "last_name", "Doe", "email", "john.doe@example.com")
            ),
            "meta", Map.of("total", 1, "current_page", 1, "per_page", 25)
        );
        
        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.searchContacts(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));
        
        // Validate data structure
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());
        assertEquals(1L, data.get(0).get("id"));
        assertEquals("John", data.get(0).get("firstName"));
        
        // Verify query parameters were properly formatted
        verify(monicaClient).get(eq("/contacts"), argThat(params -> {
            return "John Doe".equals(params.get("query")) && 
                   "25".equals(params.get("limit"));
        }));
    }

    @Test
    void contactSearch_InvalidLimit_ShouldClampToValidRange() {
        // Given
        Map<String, Object> arguments = Map.of(
            "query", "test",
            "limit", 200  // Above maximum
        );
        
        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(Map.of("data", List.of())));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        contactService.searchContacts(arguments).block();

        // Then - Should clamp to maximum limit
        verify(monicaClient).get(eq("/contacts"), argThat(params -> 
            "100".equals(params.get("limit"))  // Clamped to max
        ));
    }

    @Test
    void contactSearch_MissingQuery_ShouldHandleGracefully() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 10);  // No query
        
        Map<String, Object> emptyResponse = Map.of(
            "data", List.of(),
            "meta", Map.of("total", 0, "current_page", 1, "per_page", 10)
        );
        
        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.searchContacts(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void contactSearch_ResponseStructureValidation() {
        // Given
        Map<String, Object> arguments = Map.of("query", "validation");
        Map<String, Object> mockResponse = Map.of(
            "data", List.of(Map.of("id", 1L, "first_name", "Test")),
            "meta", Map.of("total", 1)
        );
        
        when(monicaClient.get(anyString(), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.searchContacts(arguments).block();

        // Then - Validate MCP response structure
        assertNotNull(result, "Contact search should return non-null result");
        assertTrue(result.containsKey("data"), "Should contain 'data' key");
        assertTrue(result.containsKey("content"), "Should contain 'content' key");
        
        // Validate content is properly formatted for MCP
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertNotNull(content, "Content should not be null");
        assertFalse(content.isEmpty(), "Content should not be empty");
        
        // Validate content structure for MCP protocol
        Map<String, Object> firstContent = content.get(0);
        assertTrue(firstContent.containsKey("type"), "Content should have 'type'");
        assertEquals("text", firstContent.get("type"), "Content should be text type");
        assertTrue(firstContent.containsKey("text"), "Content should have 'text'");
    }
}