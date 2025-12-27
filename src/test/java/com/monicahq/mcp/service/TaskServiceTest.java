package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.TaskFieldMappingConfig;
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
 * Unit tests for TaskService covering CRUD operations, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class TaskServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private TaskService taskService;

    private Map<String, Object> mockTaskData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        // Create TaskService with real TaskFieldMappingConfig (no dependencies to mock)
        TaskFieldMappingConfig fieldMappingConfig = new TaskFieldMappingConfig();
        taskService = new TaskService(monicaClient, contentFormatter, fieldMappingConfig);

        mockTaskData = taskBuilder()
            .id(1L)
            .title("Complete project documentation")
            .description("Write comprehensive docs for the API")
            .contactId(10L)
            .completed(false)
            .build();

        mockApiResponse = createSingleEntityResponse(mockTaskData);
    }

    // ========================================================================================
    // CREATE TASK TESTS
    // ========================================================================================

    @Test
    void createTask_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Complete project documentation");
        arguments.put("description", "Write comprehensive docs for the API");

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.createTask(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted task JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/tasks"), argThat(data ->
            data.get("contact_id").equals(10L) &&
            "Complete project documentation".equals(data.get("title"))
        ));
    }

    @Test
    void createTask_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("title", "Test task");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("title", "Test task");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_MissingTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_NullTitle_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(arguments).block();
        });
        assertEquals("title is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_EmptyTitle_Succeeds() {
        // Given - empty title is allowed per AbstractCrudService implementation (only null check)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "   ");

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.createTask(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/tasks"), any());
    }

    @Test
    void createTask_SetsCompletedDefaultFalse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test task");
        // Note: completed is NOT provided

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.createTask(arguments).block();

        // Then - verify completed defaults to false
        verify(monicaClient).post(eq("/tasks"), argThat(data ->
            Boolean.FALSE.equals(data.get("completed"))
        ));
    }

    @Test
    void createTask_WithCompletedTrue_RespectsProvidedValue() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test task");
        arguments.put("completed", true);

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.createTask(arguments).block();

        // Then - verify provided completed value is respected
        verify(monicaClient).post(eq("/tasks"), argThat(data ->
            Boolean.TRUE.equals(data.get("completed"))
        ));
    }

    @Test
    void createTask_EmptyArguments_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(arguments).block();
        });
        assertEquals("Task arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.createTask(null).block();
        });
        assertEquals("Task arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createTask_StringContactId_Succeeds() {
        // Given - String contactId that is parseable to a number
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "10");
        arguments.put("title", "Test task");

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.createTask(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/tasks"), any());
    }

    @Test
    void createTask_MapsDueDateField_Correctly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 10L);
        arguments.put("title", "Test task");
        arguments.put("dueDate", "2024-02-15");

        when(monicaClient.post(eq("/tasks"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.createTask(arguments).block();

        // Then - verify dueDate is mapped to due_date
        verify(monicaClient).post(eq("/tasks"), argThat(data ->
            "2024-02-15".equals(data.get("due_date"))
        ));
    }

    // ========================================================================================
    // GET TASK TESTS
    // ========================================================================================

    @Test
    void getTask_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/tasks/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.getTask(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/tasks/1"), any());
    }

    @Test
    void getTask_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            taskBuilder().id(42L).title("Test Task").build()
        );

        when(monicaClient.get(eq("/tasks/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.getTask(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/tasks/42"), any());
    }

    @Test
    void getTask_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            taskBuilder().id(123L).title("Test Task").build()
        );

        when(monicaClient.get(eq("/tasks/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.getTask(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/tasks/123"), any());
    }

    @Test
    void getTask_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.getTask(arguments).block();
        });
        assertEquals("Task ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTask_NullArguments_ThrowsException() {
        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.getTask(null).block();
        });
        assertEquals("Task ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTask_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.getTask(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid task ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getTask_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("title", "Test Task");
        apiData.put("contact_id", 5L);
        apiData.put("completed_at", "2024-01-15T10:00:00Z");
        apiData.put("due_date", "2024-01-20");
        apiData.put("created_at", "2024-01-14T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/tasks/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = taskService.getTask(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5L, data.get("contactId"));
        assertEquals("2024-01-15T10:00:00Z", data.get("completedAt"));
        assertEquals("2024-01-20", data.get("dueDate"));
        assertEquals("2024-01-14T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
    }

    // ========================================================================================
    // UPDATE TASK TESTS
    // ========================================================================================

    @Test
    void updateTask_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("title", "Updated task title");
        arguments.put("description", "Updated description");

        when(monicaClient.put(eq("/tasks/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        Map<String, Object> result = taskService.updateTask(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/tasks/1"), argThat(data ->
            "Updated task title".equals(data.get("title")) &&
            "Updated description".equals(data.get("description"))
        ));
    }

    @Test
    void updateTask_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("title", "Updated task");

        when(monicaClient.put(eq("/tasks/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.updateTask(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/tasks/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateTask_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Updated task");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.updateTask(arguments).block();
        });
        assertEquals("Task ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateTask_WithDueDate_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("dueDate", "2024-02-01");

        when(monicaClient.put(eq("/tasks/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.updateTask(arguments).block();

        // Then - verify dueDate is mapped to due_date
        verify(monicaClient).put(eq("/tasks/1"), argThat(data ->
            "2024-02-01".equals(data.get("due_date")) &&
            !data.containsKey("dueDate")
        ));
    }

    @Test
    void updateTask_WithCompletedAt_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("completedAt", "2024-01-20T15:30:00Z");
        arguments.put("completed", true);

        when(monicaClient.put(eq("/tasks/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.updateTask(arguments).block();

        // Then - verify completedAt is mapped to completed_at
        verify(monicaClient).put(eq("/tasks/1"), argThat(data ->
            "2024-01-20T15:30:00Z".equals(data.get("completed_at")) &&
            Boolean.TRUE.equals(data.get("completed")) &&
            !data.containsKey("completedAt")
        ));
    }

    @Test
    void updateTask_WithContactId_MapsFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 20L);

        when(monicaClient.put(eq("/tasks/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted task JSON");

        // When
        taskService.updateTask(arguments).block();

        // Then - verify contactId is mapped to contact_id
        verify(monicaClient).put(eq("/tasks/1"), argThat(data ->
            Long.valueOf(20L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    // ========================================================================================
    // DELETE TASK TESTS
    // ========================================================================================

    @Test
    void deleteTask_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/tasks/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Task"), eq(1L), eq(true), anyString()
        )).thenReturn("Task deleted successfully");

        // When
        Map<String, Object> result = taskService.deleteTask(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/tasks/1"));
    }

    @Test
    void deleteTask_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/tasks/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Task"), eq(99L), eq(true), anyString()
        )).thenReturn("Task deleted successfully");

        // When
        Map<String, Object> result = taskService.deleteTask(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/tasks/99"));
    }

    @Test
    void deleteTask_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("title", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.deleteTask(arguments).block();
        });
        assertEquals("Task ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteTask_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            taskService.deleteTask(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid task ID format"));
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST TASKS TESTS
    // ========================================================================================

    @Test
    void listTasks_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build(),
            taskBuilder().id(2L).title("Task 2").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks, 2, 20, 50);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = taskService.listTasks(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_WithContactFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 5L);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task for contact").contactId(5L).build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify contactId is mapped to "contact_id" query param
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "5".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listTasks_WithCompletedFilter_IncludesQueryParam() {
        // Given
        Map<String, Object> arguments = Map.of("completed", true);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Completed task").completed(true).build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify completed filter is included
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "true".equals(params.get("completed"))
        ));
    }

    @Test
    void listTasks_LimitAboveMaximum_ClampsTo100() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify limit is clamped to 100
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_LimitBelowMinimum_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_NegativeLimit_ClampsTo1() {
        // Given
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then - verify limit is clamped to 1
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks, 1, 10, 100);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = taskService.listTasks(arguments).block();

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
    void listTasks_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = taskService.listTasks(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listTasks_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> tasks = List.of(
            taskBuilder().id(1L).title("Task 1").build()
        );
        Map<String, Object> listResponse = createListResponse(tasks);

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        taskService.listTasks(arguments).block();

        // Then
        verify(monicaClient).get(eq("/tasks"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listTasks_MapsContactIdFieldCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> taskWithContactId = new HashMap<>();
        taskWithContactId.put("id", 1L);
        taskWithContactId.put("title", "Task with contact");
        taskWithContactId.put("contact_id", 10L);
        taskWithContactId.put("due_date", "2024-02-01");
        taskWithContactId.put("created_at", "2024-01-15T10:00:00Z");
        taskWithContactId.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(taskWithContactId));

        when(monicaClient.get(eq("/tasks"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = taskService.listTasks(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify contact_id is mapped to contactId
        assertEquals(10L, data.get(0).get("contactId"));
        assertEquals("2024-02-01", data.get(0).get("dueDate"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
    }
}
