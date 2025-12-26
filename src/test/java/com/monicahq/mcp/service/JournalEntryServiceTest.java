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

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for JournalEntryService covering CRUD operations, validation,
 * date handling, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class JournalEntryServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private JournalEntryService journalEntryService;

    private Map<String, Object> mockJournalEntryData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockJournalEntryData = journalEntryBuilder()
            .id(1L)
            .title("My Journal Entry")
            .post("This is the content of my journal entry")
            .date("2024-01-15")
            .build();

        mockApiResponse = createSingleEntityResponse(mockJournalEntryData);
    }

    // ========================================================================================
    // CREATE JOURNAL ENTRY TESTS
    // ========================================================================================

    @Test
    void createJournalEntry_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "My Journal Entry");
        arguments.put("date", "2024-01-15");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted journal entry JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "My Journal Entry".equals(data.get("title")) &&
            "2024-01-15".equals(data.get("date"))
        ));
    }

    @Test
    void createJournalEntry_MissingTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("date", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_NullTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", null);
        arguments.put("date", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_EmptyTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "");
        arguments.put("date", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_WhitespaceOnlyTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "   ");
        arguments.put("date", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_MissingDate_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "My Journal Entry");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("date is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_NullDate_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "My Journal Entry");
        arguments.put("date", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("date is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(arguments).block();
        });
        assertEquals("Journal entry creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.createJournalEntry(null).block();
        });
        assertEquals("Journal entry creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createJournalEntry_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "My Journal Entry");
        arguments.put("date", "2024-01-15");
        arguments.put("post", "This is the journal content");
        arguments.put("journalEntry", "Additional entry notes");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.createJournalEntry(arguments).block();

        // Then
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "My Journal Entry".equals(data.get("title")) &&
            "2024-01-15".equals(data.get("date")) &&
            "This is the journal content".equals(data.get("post")) &&
            "Additional entry notes".equals(data.get("journal_entry"))
        ));
    }

    @Test
    void createJournalEntry_MapsJournalEntryField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "My Journal Entry");
        arguments.put("date", "2024-01-15");
        arguments.put("journalEntry", "My reflections");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.createJournalEntry(arguments).block();

        // Then - verify journalEntry is mapped to journal_entry
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "My reflections".equals(data.get("journal_entry")) &&
            !data.containsKey("journalEntry")
        ));
    }

    @Test
    void createJournalEntry_DateFormat_Preserved() {
        // Given - test various date formats
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Date Format Test");
        arguments.put("date", "2024-12-31");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        journalEntryService.createJournalEntry(arguments).block();

        // Then
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "2024-12-31".equals(data.get("date"))
        ));
    }

    // ========================================================================================
    // GET JOURNAL ENTRY TESTS
    // ========================================================================================

    @Test
    void getJournalEntry_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/entries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/entries/1"), any());
    }

    @Test
    void getJournalEntry_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            journalEntryBuilder().id(42L).title("String ID Test").build()
        );

        when(monicaClient.get(eq("/entries/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/entries/42"), any());
    }

    @Test
    void getJournalEntry_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            journalEntryBuilder().id(123L).title("Integer ID Test").build()
        );

        when(monicaClient.get(eq("/entries/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/entries/123"), any());
    }

    @Test
    void getJournalEntry_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Some Title");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.getJournalEntry(arguments).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getJournalEntry_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.getJournalEntry(null).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getJournalEntry_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.getJournalEntry(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid journal entry ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getJournalEntry_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("title", "My Entry");
        apiData.put("post", "Entry content");
        apiData.put("date", "2024-01-15");
        apiData.put("journal_entry", "Journal notes");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/entries/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals("Journal notes", data.get("journalEntry"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        // These fields should pass through unchanged
        assertEquals("My Entry", data.get("title"));
        assertEquals("Entry content", data.get("post"));
        assertEquals("2024-01-15", data.get("date"));
    }

    @Test
    void getJournalEntry_DirectResponse_HandlesCorrectly() {
        // Given - response without data wrapper
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directApiResponse = new HashMap<>();
        directApiResponse.put("id", 1L);
        directApiResponse.put("title", "Direct Response Entry");
        directApiResponse.put("date", "2024-01-15");

        when(monicaClient.get(eq("/entries/1"), any())).thenReturn(Mono.just(directApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Direct Response Entry", data.get("title"));
    }

    // ========================================================================================
    // UPDATE JOURNAL ENTRY TESTS
    // ========================================================================================

    @Test
    void updateJournalEntry_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "Updated Title");

        when(monicaClient.put(eq("/entries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        Map<String, Object> result = journalEntryService.updateJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/entries/1"), argThat(data ->
            "Updated Title".equals(data.get("title"))
        ));
    }

    @Test
    void updateJournalEntry_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("title", "Updated Title");

        when(monicaClient.put(eq("/entries/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/entries/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateJournalEntry_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Updated Title");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.updateJournalEntry(arguments).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateJournalEntry_MapsJournalEntryField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("journalEntry", "Updated reflections");

        when(monicaClient.put(eq("/entries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then - verify journalEntry is mapped to journal_entry
        verify(monicaClient).put(eq("/entries/1"), argThat(data ->
            "Updated reflections".equals(data.get("journal_entry")) &&
            !data.containsKey("journalEntry")
        ));
    }

    @Test
    void updateJournalEntry_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("title", "Updated Title");

        when(monicaClient.put(eq("/entries/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then
        verify(monicaClient).put(eq("/entries/42"), any());
    }

    @Test
    void updateJournalEntry_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("title", "Updated Title");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.updateJournalEntry(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid journal entry ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateJournalEntry_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("title", "Updated Title");

        when(monicaClient.put(eq("/entries/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted journal entry JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then
        verify(monicaClient).put(eq("/entries/99"), any());
    }

    @Test
    void updateJournalEntry_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.updateJournalEntry(null).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateJournalEntry_WithDateField_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("date", "2024-06-20");

        when(monicaClient.put(eq("/entries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then
        verify(monicaClient).put(eq("/entries/1"), argThat(data ->
            "2024-06-20".equals(data.get("date"))
        ));
    }

    @Test
    void updateJournalEntry_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "Updated Title");
        arguments.put("post", "Updated content");
        arguments.put("date", "2024-06-20");
        arguments.put("journalEntry", "Updated notes");

        when(monicaClient.put(eq("/entries/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        journalEntryService.updateJournalEntry(arguments).block();

        // Then
        verify(monicaClient).put(eq("/entries/1"), argThat(data ->
            "Updated Title".equals(data.get("title")) &&
            "Updated content".equals(data.get("post")) &&
            "2024-06-20".equals(data.get("date")) &&
            "Updated notes".equals(data.get("journal_entry")) &&
            !data.containsKey("id") &&
            !data.containsKey("journalEntry")
        ));
    }

    // ========================================================================================
    // DELETE JOURNAL ENTRY TESTS
    // ========================================================================================

    @Test
    void deleteJournalEntry_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/entries/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Journal Entry"), eq(1L), eq(true), anyString()
        )).thenReturn("Journal entry deleted successfully");

        // When
        Map<String, Object> result = journalEntryService.deleteJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/entries/1"));
    }

    @Test
    void deleteJournalEntry_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/entries/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Journal Entry"), eq(99L), eq(true), anyString()
        )).thenReturn("Journal entry deleted successfully");

        // When
        Map<String, Object> result = journalEntryService.deleteJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/entries/99"));
    }

    @Test
    void deleteJournalEntry_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/entries/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Journal Entry"), eq(55L), eq(true), anyString()
        )).thenReturn("Journal entry deleted successfully");

        // When
        Map<String, Object> result = journalEntryService.deleteJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/entries/55"));
    }

    @Test
    void deleteJournalEntry_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Some Title");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.deleteJournalEntry(arguments).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteJournalEntry_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.deleteJournalEntry(null).block();
        });
        assertEquals("Journal entry ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteJournalEntry_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            journalEntryService.deleteJournalEntry(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid journal entry ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST JOURNAL ENTRIES TESTS
    // ========================================================================================

    @Test
    void listJournalEntries_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build(),
            journalEntryBuilder().id(2L).title("Entry 2").build()
        );
        Map<String, Object> listResponse = createListResponse(entries, 2, 20, 50);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries, 1, 10, 100);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(100, meta.get("total"));
        assertEquals(10, meta.get("last_page"));
    }

    @Test
    void listJournalEntries_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listJournalEntries_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listJournalEntries_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> entryWithFields = new HashMap<>();
        entryWithFields.put("id", 1L);
        entryWithFields.put("title", "Test Entry");
        entryWithFields.put("post", "Entry content");
        entryWithFields.put("date", "2024-01-15");
        entryWithFields.put("journal_entry", "Journal notes");
        entryWithFields.put("created_at", "2024-01-15T10:00:00Z");
        entryWithFields.put("updated_at", "2024-01-15T11:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(entryWithFields));

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping
        assertEquals("Journal notes", data.get(0).get("journalEntry"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get(0).get("updatedAt"));
        assertEquals("Test Entry", data.get(0).get("title"));
        assertEquals("Entry content", data.get(0).get("post"));
        assertEquals("2024-01-15", data.get(0).get("date"));
    }

    @Test
    void listJournalEntries_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> listResponse = createListResponse(entries, 3, 10, 30);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        journalEntryService.listJournalEntries(arguments).block();

        // Then
        verify(monicaClient).get(eq("/entries"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listJournalEntries_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> entries = List.of(
            journalEntryBuilder().id(1L).title("Entry 1").build()
        );
        Map<String, Object> responseWithoutMeta = new HashMap<>();
        responseWithoutMeta.put("data", entries);
        // No meta field

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(responseWithoutMeta));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listJournalEntries_MultipleEntries_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> entry1 = new HashMap<>();
        entry1.put("id", 1L);
        entry1.put("title", "Entry 1");
        entry1.put("date", "2024-01-15");
        entry1.put("journal_entry", "Notes 1");

        Map<String, Object> entry2 = new HashMap<>();
        entry2.put("id", 2L);
        entry2.put("title", "Entry 2");
        entry2.put("date", "2024-01-16");
        entry2.put("journal_entry", "Notes 2");

        Map<String, Object> entry3 = new HashMap<>();
        entry3.put("id", 3L);
        entry3.put("title", "Entry 3");
        entry3.put("date", "2024-01-17");
        entry3.put("journal_entry", "Notes 3");

        Map<String, Object> listResponse = createListResponse(List.of(entry1, entry2, entry3), 1, 10, 3);

        when(monicaClient.get(eq("/entries"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = journalEntryService.listJournalEntries(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Entry 1", data.get(0).get("title"));
        assertEquals("2024-01-15", data.get(0).get("date"));
        assertEquals("Notes 1", data.get(0).get("journalEntry"));

        assertEquals("Entry 2", data.get(1).get("title"));
        assertEquals("2024-01-16", data.get(1).get("date"));
        assertEquals("Notes 2", data.get(1).get("journalEntry"));

        assertEquals("Entry 3", data.get(2).get("title"));
        assertEquals("2024-01-17", data.get(2).get("date"));
        assertEquals("Notes 3", data.get(2).get("journalEntry"));
    }

    // ========================================================================================
    // DATE HANDLING TESTS
    // ========================================================================================

    @Test
    void journalEntry_DateFormat_StandardFormat_Works() {
        // Given - Standard date format
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Date Test");
        arguments.put("date", "2024-06-15");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "2024-06-15".equals(data.get("date"))
        ));
    }

    @Test
    void journalEntry_DateFormat_EndOfYear_Works() {
        // Given - End of year date
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Year End Entry");
        arguments.put("date", "2024-12-31");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "2024-12-31".equals(data.get("date"))
        ));
    }

    @Test
    void journalEntry_DateFormat_StartOfYear_Works() {
        // Given - Start of year date
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Year Start Entry");
        arguments.put("date", "2024-01-01");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "2024-01-01".equals(data.get("date"))
        ));
    }

    @Test
    void journalEntry_DateFormat_LeapYear_Works() {
        // Given - Leap year February 29
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Leap Year Entry");
        arguments.put("date", "2024-02-29");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "2024-02-29".equals(data.get("date"))
        ));
    }

    // ========================================================================================
    // EDGE CASE TESTS
    // ========================================================================================

    @Test
    void createJournalEntry_LongTitle_Succeeds() {
        // Given - Long but valid title
        String longTitle = "A".repeat(255);
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", longTitle);
        arguments.put("date", "2024-01-15");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            longTitle.equals(data.get("title"))
        ));
    }

    @Test
    void createJournalEntry_SpecialCharactersInTitle_Succeeds() {
        // Given - Title with special characters
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Test Entry with <special> & \"characters\" '`~!@#$%^&*()");
        arguments.put("date", "2024-01-15");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), any());
    }

    @Test
    void createJournalEntry_UnicodeTitle_Succeeds() {
        // Given - Title with unicode characters
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Test Entry with unicode: \u4E2D\u6587 \uD83D\uDE00 \u00E9\u00E8\u00EA");
        arguments.put("date", "2024-01-15");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), any());
    }

    @Test
    void getJournalEntry_LargeId_Succeeds() {
        // Given - Large numeric ID
        Map<String, Object> arguments = Map.of("id", Long.MAX_VALUE);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            journalEntryBuilder().id(Long.MAX_VALUE).build()
        );

        when(monicaClient.get(eq("/entries/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.getJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/entries/" + Long.MAX_VALUE), any());
    }

    @Test
    void createJournalEntry_EmptyPost_Succeeds() {
        // Given - Empty post content is allowed
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Entry with empty post");
        arguments.put("date", "2024-01-15");
        arguments.put("post", "");

        when(monicaClient.post(eq("/entries"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = journalEntryService.createJournalEntry(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/entries"), argThat(data ->
            "".equals(data.get("post"))
        ));
    }
}
