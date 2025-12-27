package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.CallFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
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
 * Unit tests for CallService covering call logging, content validation,
 * duration handling, and CRUD operations.
 */
@ExtendWith(MockitoExtension.class)
class CallServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private CallService callService;

    private Map<String, Object> mockCallData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        CallFieldMappingConfig fieldMappingConfig = new CallFieldMappingConfig();
        callService = new CallService(monicaClient, contentFormatter, fieldMappingConfig);

        mockCallData = callBuilder()
            .id(1L)
            .content("Discussed project updates and timeline")
            .contactId(10L)
            .calledAt("2024-01-15T14:30:00Z")
            .build();

        mockApiResponse = createSingleEntityResponse(mockCallData);
    }

    // ========================================================================================
    // CREATE CALL TESTS
    // ========================================================================================

    @Test
    void createCall_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("content", "Discussed project updates and timeline");

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.createCall(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted call JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/calls"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "2024-01-15T14:30:00Z".equals(data.get("called_at")) &&
            "Discussed project updates and timeline".equals(data.get("content"))
        ));
    }

    @Test
    void createCall_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("content", "Test call");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_MissingCalledAt_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("calledAt is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_NullCalledAt_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("calledAt is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_EmptyCalledAt_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("calledAt is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("Call arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(null).block();
        });
        assertEquals("Call arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_StringContactId_Succeeds() {
        // Given - String contactId that is parseable to a number
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("calledAt", "2024-01-15T14:30:00Z");

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.createCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/calls"), any());
    }

    @Test
    void createCall_WithDurationInMinutes_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("durationInMinutes", 30);

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.createCall(arguments).block();

        // Then - verify durationInMinutes is mapped to duration
        verify(monicaClient).post(eq("/calls"), argThat(data ->
            Integer.valueOf(30).equals(data.get("duration")) &&
            !data.containsKey("durationInMinutes")
        ));
    }

    @Test
    void createCall_MapsContactIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.createCall(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).post(eq("/calls"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void createCall_MapsCalledAtField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.createCall(arguments).block();

        // Then - verify calledAt is mapped to called_at
        verify(monicaClient).post(eq("/calls"), argThat(data ->
            "2024-01-15T14:30:00Z".equals(data.get("called_at")) &&
            !data.containsKey("calledAt")
        ));
    }

    // ========================================================================================
    // CREATE CALL DURATION VALIDATION TESTS
    // ========================================================================================

    @Test
    void createCall_NegativeDuration_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("durationInMinutes", -5);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("Duration cannot be negative", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_DurationExceeds24Hours_ThrowsException() {
        // Given - 1441 minutes is more than 24 hours (1440 minutes)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("durationInMinutes", 1441);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.createCall(arguments).block();
        });
        assertEquals("Duration cannot exceed 24 hours (1440 minutes)", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCall_DurationExactly24Hours_Succeeds() {
        // Given - 1440 minutes is exactly 24 hours
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("durationInMinutes", 1440);

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.createCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/calls"), argThat(data ->
            Integer.valueOf(1440).equals(data.get("duration"))
        ));
    }

    @Test
    void createCall_ZeroDuration_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("calledAt", "2024-01-15T14:30:00Z");
        arguments.put("durationInMinutes", 0);

        when(monicaClient.post(eq("/calls"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.createCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/calls"), any());
    }

    // ========================================================================================
    // GET CALL TESTS
    // ========================================================================================

    @Test
    void getCall_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/calls/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.getCall(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/calls/1"), any());
    }

    @Test
    void getCall_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            callBuilder().id(42L).content("Test Call").build()
        );

        when(monicaClient.get(eq("/calls/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.getCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/calls/42"), any());
    }

    @Test
    void getCall_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            callBuilder().id(123L).content("Test Call").build()
        );

        when(monicaClient.get(eq("/calls/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.getCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/calls/123"), any());
    }

    @Test
    void getCall_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("content", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.getCall(arguments).block();
        });
        assertEquals("Call ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCall_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.getCall(null).block();
        });
        assertEquals("Call ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCall_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.getCall(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid call ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCall_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("content", "Test Call");
        apiData.put("contact_id", 5L);
        apiData.put("called_at", "2024-01-15T14:30:00Z");
        apiData.put("duration", 45);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/calls/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = callService.getCall(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get("contactId"));
        assertEquals("2024-01-15T14:30:00Z", data.get("calledAt"));
        assertEquals(45, data.get("durationInMinutes"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
    }

    // ========================================================================================
    // UPDATE CALL TESTS
    // ========================================================================================

    @Test
    void updateCall_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("content", "Updated call notes");
        arguments.put("durationInMinutes", 60);

        when(monicaClient.put(eq("/calls/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        Map<String, Object> result = callService.updateCall(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/calls/1"), argThat(data ->
            "Updated call notes".equals(data.get("content"))
        ));
    }

    @Test
    void updateCall_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("content", "Updated call");

        when(monicaClient.put(eq("/calls/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.updateCall(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/calls/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateCall_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("content", "Updated call");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.updateCall(arguments).block();
        });
        assertEquals("Call ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCall_WithDurationInMinutes_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("durationInMinutes", 90);

        when(monicaClient.put(eq("/calls/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.updateCall(arguments).block();

        // Then - verify durationInMinutes is mapped to duration
        verify(monicaClient).put(eq("/calls/1"), argThat(data ->
            Integer.valueOf(90).equals(data.get("duration")) &&
            !data.containsKey("durationInMinutes")
        ));
    }

    @Test
    void updateCall_WithContactId_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 20L);

        when(monicaClient.put(eq("/calls/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.updateCall(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/calls/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateCall_WithCalledAt_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("calledAt", "2024-02-15T10:00:00Z");

        when(monicaClient.put(eq("/calls/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.updateCall(arguments).block();

        // Then - verify calledAt is mapped to called_at
        verify(monicaClient).put(eq("/calls/1"), argThat(data ->
            "2024-02-15T10:00:00Z".equals(data.get("called_at")) &&
            !data.containsKey("calledAt")
        ));
    }

    @Test
    void updateCall_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("content", "Updated call");

        when(monicaClient.put(eq("/calls/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted call JSON");

        // When
        callService.updateCall(arguments).block();

        // Then
        verify(monicaClient).put(eq("/calls/42"), any());
    }

    @Test
    void updateCall_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("content", "Updated call");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.updateCall(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid call ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DELETE CALL TESTS
    // ========================================================================================

    @Test
    void deleteCall_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/calls/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Call"), eq(1L), eq(true), anyString()
        )).thenReturn("Call deleted successfully");

        // When
        Map<String, Object> result = callService.deleteCall(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/calls/1"));
    }

    @Test
    void deleteCall_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/calls/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Call"), eq(99L), eq(true), anyString()
        )).thenReturn("Call deleted successfully");

        // When
        Map<String, Object> result = callService.deleteCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/calls/99"));
    }

    @Test
    void deleteCall_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/calls/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Call"), eq(55L), eq(true), anyString()
        )).thenReturn("Call deleted successfully");

        // When
        Map<String, Object> result = callService.deleteCall(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/calls/55"));
    }

    @Test
    void deleteCall_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("content", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.deleteCall(arguments).block();
        });
        assertEquals("Call ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCall_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.deleteCall(null).block();
        });
        assertEquals("Call ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCall_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            callService.deleteCall(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid call ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST CALLS TESTS
    // ========================================================================================

    @Test
    void listCalls_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build(),
            callBuilder().id(2L).content("Call 2").build()
        );
        Map<String, Object> listResponse = createListResponse(calls, 2, 20, 50);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = callService.listCalls(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_WithContactFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call for contact").contactId(5L).build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify contactId is mapped to "contact_id" query param
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "5".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listCalls_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls, 1, 10, 100);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = callService.listCalls(arguments).block();

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
    void listCalls_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = callService.listCalls(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listCalls_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listCalls_MapsContactIdFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> callWithContactId = new HashMap<>();
        callWithContactId.put("id", 1L);
        callWithContactId.put("content", "Call with contact");
        callWithContactId.put("contact_id", 10L);
        callWithContactId.put("called_at", "2024-01-15T14:30:00Z");
        callWithContactId.put("duration", 30);
        callWithContactId.put("created_at", "2024-01-15T10:00:00Z");
        callWithContactId.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(callWithContactId));

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = callService.listCalls(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify contact_id is mapped to contactId
        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals("2024-01-15T14:30:00Z", data.get(0).get("calledAt"));
        assertEquals(30, data.get(0).get("durationInMinutes"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
    }

    @Test
    void listCalls_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls, 3, 10, 30);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listCalls_WithIntegerContactId_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 42);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").contactId(42L).build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            "42".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listCalls_NullContactId_ExcludesFromQueryParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);

        List<Map<String, Object>> calls = List.of(
            callBuilder().id(1L).content("Call 1").build()
        );
        Map<String, Object> listResponse = createListResponse(calls);

        when(monicaClient.get(eq("/calls"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        callService.listCalls(arguments).block();

        // Then - verify contactId is not included in query params
        verify(monicaClient).get(eq("/calls"), argThat(params ->
            !params.containsKey("contact_id")
        ));
    }
}
