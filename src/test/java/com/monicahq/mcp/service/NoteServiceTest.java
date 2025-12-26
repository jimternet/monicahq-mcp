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
 * Unit tests for NoteService covering CRUD operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class NoteServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private NoteService noteService;

    private Map<String, Object> mockNoteData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockNoteData = noteBuilder()
            .id(1L)
            .body("This is a test note about our meeting")
            .contactId(10L)
            .favorited(false)
            .build();

        mockApiResponse = createSingleEntityResponse(mockNoteData);
    }

    // ========================================================================================
    // CREATE NOTE TESTS
    // ========================================================================================

    @Test
    void createNote_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("body", "This is a test note about our meeting");

        when(monicaClient.post(eq("/notes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.createNote(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted note JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/notes"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "This is a test note about our meeting".equals(data.get("body"))
        ));
    }

    @Test
    void createNote_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("body", "Test note");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("body", "Test note");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_MissingBody_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(arguments).block();
        });
        assertEquals("body is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_NullBody_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("body", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(arguments).block();
        });
        assertEquals("body is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_EmptyBody_Succeeds() {
        // Given - empty body is allowed per NoteService implementation
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("body", "");

        when(monicaClient.post(eq("/notes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.createNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/notes"), any());
    }

    @Test
    void createNote_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(arguments).block();
        });
        assertEquals("Note creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.createNote(null).block();
        });
        assertEquals("Note creation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createNote_StringContactId_Succeeds() {
        // Given - String contactId that is parseable to a number
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("body", "Test note");

        when(monicaClient.post(eq("/notes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.createNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/notes"), any());
    }

    @Test
    void createNote_WithIsFavorited_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("body", "Test note");
        arguments.put("isFavorited", true);

        when(monicaClient.post(eq("/notes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.createNote(arguments).block();

        // Then - verify isFavorited is mapped to is_favorited
        verify(monicaClient).post(eq("/notes"), argThat(data ->
            Boolean.TRUE.equals(data.get("is_favorited")) &&
            !data.containsKey("isFavorited")
        ));
    }

    @Test
    void createNote_MapsContactIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("body", "Test note");

        when(monicaClient.post(eq("/notes"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.createNote(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).post(eq("/notes"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            !data.containsKey("contactId")
        ));
    }

    // ========================================================================================
    // GET NOTE TESTS
    // ========================================================================================

    @Test
    void getNote_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/notes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.getNote(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/notes/1"), any());
    }

    @Test
    void getNote_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            noteBuilder().id(42L).body("Test Note").build()
        );

        when(monicaClient.get(eq("/notes/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.getNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/notes/42"), any());
    }

    @Test
    void getNote_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            noteBuilder().id(123L).body("Test Note").build()
        );

        when(monicaClient.get(eq("/notes/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.getNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/notes/123"), any());
    }

    @Test
    void getNote_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("body", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.getNote(arguments).block();
        });
        assertEquals("Note ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getNote_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.getNote(null).block();
        });
        assertEquals("Note ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getNote_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.getNote(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid note ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getNote_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("body", "Test Note");
        apiData.put("contact_id", 5L);
        apiData.put("is_favorited", true);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/notes/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = noteService.getNote(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get("contactId"));
        assertEquals(true, data.get("isFavorited"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
    }

    // ========================================================================================
    // UPDATE NOTE TESTS
    // ========================================================================================

    @Test
    void updateNote_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("body", "Updated note content");
        arguments.put("isFavorited", true);

        when(monicaClient.put(eq("/notes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        Map<String, Object> result = noteService.updateNote(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/notes/1"), argThat(data ->
            "Updated note content".equals(data.get("body"))
        ));
    }

    @Test
    void updateNote_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("body", "Updated note");

        when(monicaClient.put(eq("/notes/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.updateNote(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/notes/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateNote_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("body", "Updated note");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.updateNote(arguments).block();
        });
        assertEquals("Note ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateNote_WithIsFavorited_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("isFavorited", true);

        when(monicaClient.put(eq("/notes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.updateNote(arguments).block();

        // Then - verify isFavorited is mapped to is_favorited
        verify(monicaClient).put(eq("/notes/1"), argThat(data ->
            Boolean.TRUE.equals(data.get("is_favorited")) &&
            !data.containsKey("isFavorited")
        ));
    }

    @Test
    void updateNote_WithContactId_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 20L);

        when(monicaClient.put(eq("/notes/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.updateNote(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/notes/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateNote_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("body", "Updated note");

        when(monicaClient.put(eq("/notes/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted note JSON");

        // When
        noteService.updateNote(arguments).block();

        // Then
        verify(monicaClient).put(eq("/notes/42"), any());
    }

    @Test
    void updateNote_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("body", "Updated note");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.updateNote(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid note ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DELETE NOTE TESTS
    // ========================================================================================

    @Test
    void deleteNote_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/notes/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Note"), eq(1L), eq(true), anyString()
        )).thenReturn("Note deleted successfully");

        // When
        Map<String, Object> result = noteService.deleteNote(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/notes/1"));
    }

    @Test
    void deleteNote_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/notes/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Note"), eq(99L), eq(true), anyString()
        )).thenReturn("Note deleted successfully");

        // When
        Map<String, Object> result = noteService.deleteNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/notes/99"));
    }

    @Test
    void deleteNote_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/notes/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Note"), eq(55L), eq(true), anyString()
        )).thenReturn("Note deleted successfully");

        // When
        Map<String, Object> result = noteService.deleteNote(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/notes/55"));
    }

    @Test
    void deleteNote_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("body", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.deleteNote(arguments).block();
        });
        assertEquals("Note ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteNote_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.deleteNote(null).block();
        });
        assertEquals("Note ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteNote_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            noteService.deleteNote(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid note ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST NOTES TESTS
    // ========================================================================================

    @Test
    void listNotes_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build(),
            noteBuilder().id(2L).body("Note 2").build()
        );
        Map<String, Object> listResponse = createListResponse(notes, 2, 20, 50);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = noteService.listNotes(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_WithContactFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note for contact").contactId(5L).build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify contactId is mapped to "contact_id" query param
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "5".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listNotes_WithFavoritedFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("favorited", true);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Favorited note").favorited(true).build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify favorited filter is mapped to is_favorited
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "true".equals(params.get("is_favorited"))
        ));
    }

    @Test
    void listNotes_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes, 1, 10, 100);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = noteService.listNotes(arguments).block();

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
    void listNotes_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = noteService.listNotes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listNotes_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listNotes_MapsContactIdFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> noteWithContactId = new HashMap<>();
        noteWithContactId.put("id", 1L);
        noteWithContactId.put("body", "Note with contact");
        noteWithContactId.put("contact_id", 10L);
        noteWithContactId.put("is_favorited", true);
        noteWithContactId.put("created_at", "2024-01-15T10:00:00Z");
        noteWithContactId.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(noteWithContactId));

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = noteService.listNotes(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify contact_id is mapped to contactId
        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals(true, data.get(0).get("isFavorited"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
    }

    @Test
    void listNotes_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").build()
        );
        Map<String, Object> listResponse = createListResponse(notes, 3, 10, 30);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listNotes_WithIntegerContactId_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 42);

        List<Map<String, Object>> notes = List.of(
            noteBuilder().id(1L).body("Note 1").contactId(42L).build()
        );
        Map<String, Object> listResponse = createListResponse(notes);

        when(monicaClient.get(eq("/notes"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        noteService.listNotes(arguments).block();

        // Then
        verify(monicaClient).get(eq("/notes"), argThat(params ->
            "42".equals(params.get("contact_id"))
        ));
    }
}
