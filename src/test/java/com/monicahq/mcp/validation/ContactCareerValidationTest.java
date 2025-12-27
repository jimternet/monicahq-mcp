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

import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Validation tests for contact career update operation.
 */
@ExtendWith(MockitoExtension.class)
class ContactCareerValidationTest {

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
    void contactCareerUpdate_ValidData_ShouldFormatCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "id", 123L,
            "jobTitle", "Senior Developer",
            "company", "Tech Corp",
            "startDate", "2023-01-15",
            "endDate", "2024-12-31"
        );
        
        Map<String, Object> mockResponse = Map.of(
            "data", Map.of(
                "id", 1L,
                "job_title", "Senior Developer",
                "company", "Tech Corp",
                "start_date", "2023-01-15"
            )
        );
        
        when(monicaClient.put(eq("/contacts/123/work"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted single JSON content");

        // When
        Map<String, Object> result = contactService.updateContactCareer(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        
        // Verify request body formatting
        verify(monicaClient).put(eq("/contacts/123/work"), argThat(data -> {
            return "Senior Developer".equals(data.get("job_title")) &&
                   "Tech Corp".equals(data.get("company")) &&
                   "2023-01-15".equals(data.get("start_date")) &&
                   "2024-12-31".equals(data.get("end_date"));
        }));
    }

    @Test
    void contactCareerUpdate_MissingContactId_ShouldThrowException() {
        // Given
        Map<String, Object> arguments = Map.of(
            "jobTitle", "Developer",
            "company", "TechCorp"
        );

        // When & Then
        assertThrows(IllegalArgumentException.class, () -> {
            contactService.updateContactCareer(arguments).block();
        });

        verifyNoInteractions(monicaClient);
    }

    @Test
    void contactCareerUpdate_MinimalData_ShouldWork() {
        // Given - Only required fields
        Map<String, Object> arguments = Map.of(
            "id", 456L,
            "jobTitle", "Designer"
        );
        
        Map<String, Object> mockResponse = Map.of(
            "data", Map.of("id", 1L, "job_title", "Designer")
        );
        
        when(monicaClient.put(eq("/contacts/456/work"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted single JSON content");

        // When
        Map<String, Object> result = contactService.updateContactCareer(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).put(eq("/contacts/456/work"), argThat(data -> 
            "Designer".equals(data.get("job_title"))
        ));
    }
}