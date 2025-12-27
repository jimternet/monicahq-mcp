package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ConversationFieldMappingConfig;
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
 * Unit tests for ConversationService covering CRUD operations, validation,
 * message handling, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class ConversationServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ConversationService conversationService;

    private Map<String, Object> mockConversationData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        // Create service with real FieldMappingConfig
        ConversationFieldMappingConfig fieldMappingConfig = new ConversationFieldMappingConfig();
        conversationService = new ConversationService(monicaClient, contentFormatter, fieldMappingConfig);

        mockConversationData = conversationBuilder()
            .id(1L)
            .contactId(10L)
            .happenedAt("2024-01-15")
            .build();

        mockApiResponse = createSingleEntityResponse(mockConversationData);
    }

    // ========================================================================================
    // CREATE CONVERSATION TESTS
    // ========================================================================================

    @Test
    void createConversation_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.createConversation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted conversation JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/conversations"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "2024-01-15".equals(data.get("happened_at"))
        ));
    }

    @Test
    void createConversation_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("happenedAt", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("happenedAt", "2024-01-15");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_MissingHappenedAt_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(arguments).block();
        });
        assertEquals("happenedAt is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_NullHappenedAt_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("happenedAt", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(arguments).block();
        });
        assertEquals("happenedAt is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(arguments).block();
        });
        assertEquals("Conversation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.createConversation(null).block();
        });
        assertEquals("Conversation arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createConversation_StringContactId_Succeeds() {
        // Given - String contactId that is parseable
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.createConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/conversations"), any());
    }

    @Test
    void createConversation_MapsContactIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.createConversation(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).post(eq("/conversations"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void createConversation_MapsHappenedAtField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.createConversation(arguments).block();

        // Then - verify happenedAt is mapped to happened_at
        verify(monicaClient).post(eq("/conversations"), argThat(data ->
            "2024-01-15".equals(data.get("happened_at")) &&
            !data.containsKey("happenedAt")
        ));
    }

    @Test
    void createConversation_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("happenedAt", "2024-01-15");
        arguments.put("description", "Meeting notes");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.createConversation(arguments).block();

        // Then
        verify(monicaClient).post(eq("/conversations"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "2024-01-15".equals(data.get("happened_at")) &&
            "Meeting notes".equals(data.get("description"))
        ));
    }

    @Test
    void createConversation_IntegerContactId_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10);
        arguments.put("happenedAt", "2024-01-15");

        when(monicaClient.post(eq("/conversations"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.createConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/conversations"), any());
    }

    // ========================================================================================
    // GET CONVERSATION TESTS
    // ========================================================================================

    @Test
    void getConversation_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/conversations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.getConversation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/conversations/1"), any());
    }

    @Test
    void getConversation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            conversationBuilder().id(42L).contactId(10L).build()
        );

        when(monicaClient.get(eq("/conversations/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.getConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/conversations/42"), any());
    }

    @Test
    void getConversation_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            conversationBuilder().id(123L).contactId(10L).build()
        );

        when(monicaClient.get(eq("/conversations/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.getConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/conversations/123"), any());
    }

    @Test
    void getConversation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.getConversation(arguments).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getConversation_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.getConversation(null).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getConversation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.getConversation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid conversation ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getConversation_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("contact_id", 10L);
        apiData.put("happened_at", "2024-01-15");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/conversations/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = conversationService.getConversation(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(10L, data.get("contactId"));
        assertEquals("2024-01-15", data.get("happenedAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
    }

    @Test
    void getConversation_DirectResponse_HandlesCorrectly() {
        // Given - response without data wrapper
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directApiResponse = new HashMap<>();
        directApiResponse.put("id", 1L);
        directApiResponse.put("contact_id", 10L);
        directApiResponse.put("happened_at", "2024-01-15");

        when(monicaClient.get(eq("/conversations/1"), any())).thenReturn(Mono.just(directApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = conversationService.getConversation(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals(10L, data.get("contactId"));
    }

    // ========================================================================================
    // UPDATE CONVERSATION TESTS
    // ========================================================================================

    @Test
    void updateConversation_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("happenedAt", "2024-02-20");

        when(monicaClient.put(eq("/conversations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        Map<String, Object> result = conversationService.updateConversation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/conversations/1"), argThat(data ->
            "2024-02-20".equals(data.get("happened_at"))
        ));
    }

    @Test
    void updateConversation_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("happenedAt", "2024-02-20");

        when(monicaClient.put(eq("/conversations/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.updateConversation(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/conversations/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateConversation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("happenedAt", "2024-02-20");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.updateConversation(arguments).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateConversation_MapsHappenedAtField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("happenedAt", "2024-02-25");

        when(monicaClient.put(eq("/conversations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.updateConversation(arguments).block();

        // Then - verify happenedAt is mapped to happened_at
        verify(monicaClient).put(eq("/conversations/1"), argThat(data ->
            "2024-02-25".equals(data.get("happened_at")) &&
            !data.containsKey("happenedAt")
        ));
    }

    @Test
    void updateConversation_MapsContactIdField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 20L);

        when(monicaClient.put(eq("/conversations/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.updateConversation(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/conversations/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateConversation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("happenedAt", "2024-02-20");

        when(monicaClient.put(eq("/conversations/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.updateConversation(arguments).block();

        // Then
        verify(monicaClient).put(eq("/conversations/42"), any());
    }

    @Test
    void updateConversation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("happenedAt", "2024-02-20");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.updateConversation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid conversation ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateConversation_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("happenedAt", "2024-02-20");

        when(monicaClient.put(eq("/conversations/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted conversation JSON");

        // When
        conversationService.updateConversation(arguments).block();

        // Then
        verify(monicaClient).put(eq("/conversations/99"), any());
    }

    @Test
    void updateConversation_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.updateConversation(null).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // DELETE CONVERSATION TESTS
    // ========================================================================================

    @Test
    void deleteConversation_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/conversations/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Conversation"), eq(1L), eq(true), anyString()
        )).thenReturn("Conversation deleted successfully");

        // When
        Map<String, Object> result = conversationService.deleteConversation(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/conversations/1"));
    }

    @Test
    void deleteConversation_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/conversations/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Conversation"), eq(99L), eq(true), anyString()
        )).thenReturn("Conversation deleted successfully");

        // When
        Map<String, Object> result = conversationService.deleteConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/conversations/99"));
    }

    @Test
    void deleteConversation_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/conversations/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Conversation"), eq(55L), eq(true), anyString()
        )).thenReturn("Conversation deleted successfully");

        // When
        Map<String, Object> result = conversationService.deleteConversation(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/conversations/55"));
    }

    @Test
    void deleteConversation_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.deleteConversation(arguments).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteConversation_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.deleteConversation(null).block();
        });
        assertEquals("Conversation ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteConversation_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            conversationService.deleteConversation(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid conversation ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST CONVERSATIONS TESTS
    // ========================================================================================

    @Test
    void listConversations_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build(),
            conversationBuilder().id(2L).contactId(20L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations, 2, 20, 50);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_WithContactFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(5L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - verify contactId is mapped to "contact_id" query param
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "5".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listConversations_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations, 1, 10, 100);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

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
    void listConversations_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listConversations_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listConversations_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> conversationWithFields = new HashMap<>();
        conversationWithFields.put("id", 1L);
        conversationWithFields.put("contact_id", 10L);
        conversationWithFields.put("happened_at", "2024-01-15");
        conversationWithFields.put("created_at", "2024-01-15T10:00:00Z");
        conversationWithFields.put("updated_at", "2024-01-15T11:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(conversationWithFields));

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify contact_id is mapped to contactId
        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals("2024-01-15", data.get(0).get("happenedAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get(0).get("updatedAt"));
    }

    @Test
    void listConversations_WithStringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations, 3, 10, 30);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listConversations_WithIntegerContactId_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 42);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(42L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            "42".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listConversations_WithNullContactId_DoesNotIncludeParam() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> listResponse = createListResponse(conversations);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        conversationService.listConversations(arguments).block();

        // Then - null contactId should not be included in query params
        verify(monicaClient).get(eq("/conversations"), argThat(params ->
            !params.containsKey("contact_id")
        ));
    }

    @Test
    void listConversations_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> conversations = List.of(
            conversationBuilder().id(1L).contactId(10L).build()
        );
        Map<String, Object> responseWithoutMeta = new HashMap<>();
        responseWithoutMeta.put("data", conversations);
        // No meta field

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(responseWithoutMeta));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listConversations_MultipleConversations_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> conv1 = new HashMap<>();
        conv1.put("id", 1L);
        conv1.put("contact_id", 10L);
        conv1.put("happened_at", "2024-01-15");

        Map<String, Object> conv2 = new HashMap<>();
        conv2.put("id", 2L);
        conv2.put("contact_id", 20L);
        conv2.put("happened_at", "2024-01-16");

        Map<String, Object> conv3 = new HashMap<>();
        conv3.put("id", 3L);
        conv3.put("contact_id", 30L);
        conv3.put("happened_at", "2024-01-17");

        Map<String, Object> listResponse = createListResponse(List.of(conv1, conv2, conv3), 1, 10, 3);

        when(monicaClient.get(eq("/conversations"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = conversationService.listConversations(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals("2024-01-15", data.get(0).get("happenedAt"));

        assertEquals(20L, data.get(1).get("contactId"));
        assertEquals("2024-01-16", data.get(1).get("happenedAt"));

        assertEquals(30L, data.get(2).get("contactId"));
        assertEquals("2024-01-17", data.get(2).get("happenedAt"));
    }
}
