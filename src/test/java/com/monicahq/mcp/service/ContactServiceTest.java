package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for ContactService focusing on new API gap fix operations.
 */
@ExtendWith(MockitoExtension.class)
class ContactServiceTest {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private ContactService contactService;

    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockApiResponse = Map.of(
            "data", List.of(
                Map.of(
                    "id", 1L,
                    "first_name", "John",
                    "last_name", "Doe",
                    "email", "john.doe@example.com"
                )
            ),
            "meta", Map.of(
                "total", 1,
                "current_page", 1,
                "per_page", 10
            )
        );
    }

    @Test
    void searchContacts_ShouldReturnFormattedResults() {
        // Given
        Map<String, Object> arguments = Map.of(
            "query", "John",
            "limit", 10
        );
        
        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.searchContacts(arguments).block();
        
        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).get(eq("/contacts"), argThat(params -> 
            params.containsKey("query") && "John".equals(params.get("query"))
        ));
    }

    @Test
    void updateContactCareer_ShouldCallCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 1L,
            "jobTitle", "Software Engineer",
            "company", "TechCorp",
            "startDate", "2023-01-15"
        );
        
        Map<String, Object> careerResponse = Map.of(
            "data", Map.of(
                "id", 1L,
                "job_title", "Software Engineer",
                "company", "TechCorp"
            )
        );
        
        when(monicaClient.put(eq("/contacts/1/work"), any())).thenReturn(Mono.just(careerResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted single JSON content");

        // When
        Map<String, Object> result = contactService.updateContactCareer(arguments).block();
        
        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/contacts/1/work"), argThat(data -> 
            "Software Engineer".equals(data.get("job_title")) &&
            "TechCorp".equals(data.get("company"))
        ));
    }

    @Test
    void updateContactCareer_WithoutContactId_ShouldThrowException() {
        // Given
        Map<String, Object> arguments = Map.of(
            "jobTitle", "Software Engineer",
            "company", "TechCorp"
        );

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            contactService.updateContactCareer(arguments).block();
        });

        verifyNoInteractions(monicaClient);
    }

    @Test
    void getContactAuditLogs_ShouldReturnFormattedLogs() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 1L,
            "limit", 20
        );
        
        Map<String, Object> auditResponse = Map.of(
            "data", List.of(
                Map.of(
                    "id", 1L,
                    "action", "create",
                    "description", "Contact created",
                    "created_at", "2023-01-15T10:00:00Z"
                )
            )
        );
        
        when(monicaClient.get(eq("/contacts/1/logs"), any())).thenReturn(Mono.just(auditResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.getContactAuditLogs(arguments).block();
        
        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        verify(monicaClient).get(eq("/contacts/1/logs"), argThat(params -> 
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void getContactAuditLogs_WithoutContactId_ShouldThrowException() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 20);

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            contactService.getContactAuditLogs(arguments).block();
        });

        verifyNoInteractions(monicaClient);
    }

    @Test
    void searchContacts_WithInvalidLimit_ShouldClampToValidRange() {
        // Given
        Map<String, Object> arguments = Map.of(
            "query", "test",
            "limit", 150 // Above maximum
        );
        
        when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted JSON content");

        // When
        Map<String, Object> result = contactService.searchContacts(arguments).block();
        
        // Then
        assertNotNull(result);

        verify(monicaClient).get(eq("/contacts"), argThat(params -> 
            "100".equals(params.get("limit")) // Clamped to maximum
        ));
    }
}