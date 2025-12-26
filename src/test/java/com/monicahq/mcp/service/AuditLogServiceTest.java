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
 * Unit tests for AuditLogService covering audit log retrieval,
 * listing, searching, field mapping, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class AuditLogServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    @InjectMocks
    private AuditLogService auditLogService;

    private Map<String, Object> mockAuditLogData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        mockAuditLogData = createAuditLogData(1L, "created", "Contact");
        mockApiResponse = createSingleEntityResponse(mockAuditLogData);
    }

    // Helper method to create audit log data with snake_case field names (API format)
    private Map<String, Object> createAuditLogData(Long id, String action, String auditableType) {
        Map<String, Object> data = new HashMap<>();
        data.put("id", id);
        data.put("action", action);
        data.put("auditable_type", auditableType);
        data.put("auditable_id", 100L);
        data.put("user_id", 1L);
        data.put("user_name", "Test User");
        data.put("ip_address", "192.168.1.1");
        data.put("user_agent", "Mozilla/5.0");
        data.put("old_values", Map.of("field", "old"));
        data.put("new_values", Map.of("field", "new"));
        data.put("created_at", "2024-01-15T10:00:00Z");
        return data;
    }

    // ========================================================================================
    // GET AUDIT LOG TESTS
    // ========================================================================================

    @Test
    void getAuditLog_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/auditlogs/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted audit log JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted audit log JSON", content.get(0).get("text"));

        verify(monicaClient).get(eq("/auditlogs/1"), any());
    }

    @Test
    void getAuditLog_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createAuditLogData(42L, "updated", "Task")
        );

        when(monicaClient.get(eq("/auditlogs/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted audit log JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs/42"), any());
    }

    @Test
    void getAuditLog_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createAuditLogData(123L, "deleted", "Note")
        );

        when(monicaClient.get(eq("/auditlogs/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted audit log JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs/123"), any());
    }

    @Test
    void getAuditLog_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("action", "created");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            auditLogService.getAuditLog(arguments).block();
        });
        assertTrue(exception.getMessage().contains("id is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAuditLog_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            auditLogService.getAuditLog(arguments).block();
        });
        assertTrue(exception.getMessage().contains("id is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAuditLog_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            auditLogService.getAuditLog(arguments).block();
        });
        assertTrue(exception.getMessage().contains("id is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAuditLog_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            auditLogService.getAuditLog(arguments).block();
        });
        assertTrue(exception.getMessage().contains("id must be a valid number"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAuditLog_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("action", "created");
        apiData.put("auditable_type", "Contact");
        apiData.put("auditable_id", 500L);
        apiData.put("user_id", 10L);
        apiData.put("user_name", "Admin User");
        apiData.put("ip_address", "10.0.0.1");
        apiData.put("user_agent", "Chrome/120.0");
        apiData.put("old_values", Map.of("name", "Old Name"));
        apiData.put("new_values", Map.of("name", "New Name"));
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/auditlogs/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals("Contact", data.get("auditableType"));
        assertEquals(500L, data.get("auditableId"));
        assertEquals(10L, data.get("userId"));
        assertEquals("Admin User", data.get("userName"));
        assertEquals("10.0.0.1", data.get("ipAddress"));
        assertEquals("Chrome/120.0", data.get("userAgent"));
        assertNotNull(data.get("oldValues"));
        assertNotNull(data.get("newValues"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("created", data.get("action"));
    }

    @Test
    void getAuditLog_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("action", "updated");
        directResponse.put("auditable_type", "Task");
        directResponse.put("user_id", 5L);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/auditlogs/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("updated", data.get("action"));
        assertEquals("Task", data.get("auditableType"));
        assertEquals(5L, data.get("userId"));
    }

    @Test
    void getAuditLog_LargeId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 999999999L);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createAuditLogData(999999999L, "created", "Contact")
        );

        when(monicaClient.get(eq("/auditlogs/999999999"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted audit log JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs/999999999"), any());
    }

    @Test
    void getAuditLog_FormatterCalledWithRawData() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/auditlogs/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        auditLogService.getAuditLog(arguments).block();

        // Then
        verify(contentFormatter).formatAsEscapedJson(any());
    }

    // ========================================================================================
    // LIST AUDIT LOGS TESTS
    // ========================================================================================

    @Test
    void listAuditLogs_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact"),
            createAuditLogData(2L, "updated", "Task")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/auditlogs"), any());
    }

    @Test
    void listAuditLogs_WithPagination_PassesCorrectParameters() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 50
        );

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 2, 50, 100);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.listAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "2".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listAuditLogs_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.listAuditLogs(arguments).block();

        // Then - verify default pagination values (page=1, limit=25)
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "1".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listAuditLogs_StringPagination_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", "3",
            "limit", "30"
        );

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 3, 30, 100);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.listAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "3".equals(params.get("page")) &&
            "30".equals(params.get("limit"))
        ));
    }

    @Test
    void listAuditLogs_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 1, 10, 100);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

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
    void listAuditLogs_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 25, 0);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listAuditLogs_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> auditLogWithSnakeCase = new HashMap<>();
        auditLogWithSnakeCase.put("id", 1L);
        auditLogWithSnakeCase.put("action", "created");
        auditLogWithSnakeCase.put("auditable_type", "Contact");
        auditLogWithSnakeCase.put("auditable_id", 100L);
        auditLogWithSnakeCase.put("user_id", 5L);
        auditLogWithSnakeCase.put("user_name", "Test User");
        auditLogWithSnakeCase.put("ip_address", "192.168.1.1");
        auditLogWithSnakeCase.put("user_agent", "Mozilla/5.0");
        auditLogWithSnakeCase.put("old_values", Map.of("status", "pending"));
        auditLogWithSnakeCase.put("new_values", Map.of("status", "completed"));
        auditLogWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(auditLogWithSnakeCase));

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals("Contact", data.get(0).get("auditableType"));
        assertEquals(100L, data.get(0).get("auditableId"));
        assertEquals(5L, data.get(0).get("userId"));
        assertEquals("Test User", data.get(0).get("userName"));
        assertEquals("192.168.1.1", data.get(0).get("ipAddress"));
        assertEquals("Mozilla/5.0", data.get(0).get("userAgent"));
        assertNotNull(data.get(0).get("oldValues"));
        assertNotNull(data.get(0).get("newValues"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        // These should remain unchanged
        assertEquals(1L, data.get(0).get("id"));
        assertEquals("created", data.get(0).get("action"));
    }

    @Test
    void listAuditLogs_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listAuditLogs_MultipleRecords_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact"),
            createAuditLogData(2L, "updated", "Task"),
            createAuditLogData(3L, "deleted", "Note")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 1, 10, 3);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("created", data.get(0).get("action"));
        assertEquals("updated", data.get(1).get("action"));
        assertEquals("deleted", data.get(2).get("action"));
    }

    @Test
    void listAuditLogs_ContentFieldHasCorrectFormat() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list JSON", content.get(0).get("text"));
    }

    @Test
    void listAuditLogs_FormatterCalledWithListData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.listAuditLogs(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(any());
    }

    @Test
    void listAuditLogs_NullArgs_HandlesGracefully() {
        // Given
        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When - null args should use default pagination
        Map<String, Object> result = auditLogService.listAuditLogs(null).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "1".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }

    // ========================================================================================
    // SEARCH AUDIT LOGS TESTS
    // ========================================================================================

    @Test
    void searchAuditLogs_WithActionFilter_PassesCorrectParameters() {
        // Given
        Map<String, Object> arguments = Map.of("action", "created");

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "created".equals(params.get("action"))
        ));
    }

    @Test
    void searchAuditLogs_WithAuditableTypeFilter_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = Map.of("auditableType", "Contact");

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then - verify auditableType mapped to auditable_type query param
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "Contact".equals(params.get("auditable_type")) &&
            !params.containsKey("auditableType")
        ));
    }

    @Test
    void searchAuditLogs_WithUserIdFilter_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = Map.of("userId", 5L);

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then - verify userId mapped to user_id query param
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "5".equals(params.get("user_id")) &&
            !params.containsKey("userId")
        ));
    }

    @Test
    void searchAuditLogs_WithAllFilters_PassesAllParameters() {
        // Given
        Map<String, Object> arguments = Map.of(
            "action", "updated",
            "auditableType", "Task",
            "userId", 10L,
            "page", 2,
            "limit", 30
        );

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "updated", "Task")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 2, 30, 50);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "updated".equals(params.get("action")) &&
            "Task".equals(params.get("auditable_type")) &&
            "10".equals(params.get("user_id")) &&
            "2".equals(params.get("page")) &&
            "30".equals(params.get("limit"))
        ));
    }

    @Test
    void searchAuditLogs_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of("action", "created");

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact"),
            createAuditLogData(2L, "created", "Task")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());
    }

    @Test
    void searchAuditLogs_WithDefaultPagination_PassesDefaults() {
        // Given
        Map<String, Object> arguments = Map.of("action", "deleted");

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "deleted", "Note")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then - verify default pagination is applied along with filter
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "deleted".equals(params.get("action")) &&
            "1".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void searchAuditLogs_StringUserId_PassedCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("userId", "15");

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "15".equals(params.get("user_id"))
        ));
    }

    @Test
    void searchAuditLogs_IntegerUserId_PassedCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("userId", 20);

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        auditLogService.searchAuditLogs(arguments).block();

        // Then
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "20".equals(params.get("user_id"))
        ));
    }

    @Test
    void searchAuditLogs_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = Map.of("action", "nonexistent");

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 25, 0);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void searchAuditLogs_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of(
            "action", "created",
            "page", 1,
            "limit", 10
        );

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs, 1, 10, 50);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) result.get("meta");
        assertEquals(1, meta.get("current_page"));
        assertEquals(10, meta.get("per_page"));
        assertEquals(50, meta.get("total"));
    }

    @Test
    void searchAuditLogs_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("action", "created");

        Map<String, Object> auditLogWithSnakeCase = new HashMap<>();
        auditLogWithSnakeCase.put("id", 1L);
        auditLogWithSnakeCase.put("action", "created");
        auditLogWithSnakeCase.put("auditable_type", "Contact");
        auditLogWithSnakeCase.put("auditable_id", 100L);
        auditLogWithSnakeCase.put("user_id", 5L);
        auditLogWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(auditLogWithSnakeCase));

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping
        assertEquals("Contact", data.get(0).get("auditableType"));
        assertEquals(100L, data.get(0).get("auditableId"));
        assertEquals(5L, data.get(0).get("userId"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
    }

    @Test
    void searchAuditLogs_NullArgs_HandlesGracefully() {
        // Given
        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When - null args should just use default pagination with no filters
        Map<String, Object> result = auditLogService.searchAuditLogs(null).block();

        // Then
        assertNotNull(result);
    }

    // ========================================================================================
    // EDGE CASES
    // ========================================================================================

    @Test
    void getAuditLog_WithZeroId_ParsesCorrectly() {
        // Given - Edge case: id = 0
        Map<String, Object> arguments = Map.of("id", 0);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createAuditLogData(0L, "created", "Contact")
        );

        when(monicaClient.get(eq("/auditlogs/0"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted audit log JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs/0"), any());
    }

    @Test
    void searchAuditLogs_MultipleSameTypeFilters_PassesAll() {
        // Given - test with various auditable types
        String[] auditableTypes = {"Contact", "Task", "Note", "Activity", "Reminder"};

        for (String type : auditableTypes) {
            Map<String, Object> arguments = Map.of("auditableType", type);

            List<Map<String, Object>> auditLogs = List.of(
                createAuditLogData(1L, "created", type)
            );
            Map<String, Object> listResponse = createListResponse(auditLogs);

            when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

            // When
            Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

            // Then
            assertNotNull(result);
        }
    }

    @Test
    void searchAuditLogs_VariousActions_PassesCorrectly() {
        // Test various action types
        String[] actions = {"created", "updated", "deleted", "viewed", "exported"};

        for (String action : actions) {
            Map<String, Object> arguments = Map.of("action", action);

            List<Map<String, Object>> auditLogs = List.of(
                createAuditLogData(1L, action, "Contact")
            );
            Map<String, Object> listResponse = createListResponse(auditLogs);

            when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
            when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

            // When
            Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient, atLeastOnce()).get(eq("/auditlogs"), argThat(params ->
                action.equals(params.get("action"))
            ));
        }
    }

    @Test
    void getAuditLog_OldValuesAndNewValues_MappedCorrectly() {
        // Given - test complex old_values and new_values
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("action", "updated");
        apiData.put("old_values", Map.of(
            "first_name", "John",
            "last_name", "Doe",
            "email", "old@example.com"
        ));
        apiData.put("new_values", Map.of(
            "first_name", "Jane",
            "last_name", "Smith",
            "email", "new@example.com"
        ));
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/auditlogs/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = auditLogService.getAuditLog(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        @SuppressWarnings("unchecked")
        Map<String, Object> oldValues = (Map<String, Object>) data.get("oldValues");
        @SuppressWarnings("unchecked")
        Map<String, Object> newValues = (Map<String, Object>) data.get("newValues");

        assertEquals("John", oldValues.get("first_name"));
        assertEquals("Jane", newValues.get("first_name"));
        assertEquals("old@example.com", oldValues.get("email"));
        assertEquals("new@example.com", newValues.get("email"));
    }

    @Test
    void listAuditLogs_WithEmptyArgs_UsesDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.listAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "1".equals(params.get("page")) &&
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void searchAuditLogs_WithEmptyArgs_UsesDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> auditLogs = List.of(
            createAuditLogData(1L, "created", "Contact")
        );
        Map<String, Object> listResponse = createListResponse(auditLogs);

        when(monicaClient.get(eq("/auditlogs"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = auditLogService.searchAuditLogs(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/auditlogs"), argThat(params ->
            "1".equals(params.get("page")) &&
            "25".equals(params.get("limit")) &&
            !params.containsKey("action") &&
            !params.containsKey("auditable_type") &&
            !params.containsKey("user_id")
        ));
    }
}
