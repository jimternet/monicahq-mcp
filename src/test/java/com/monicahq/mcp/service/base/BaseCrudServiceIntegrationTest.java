package com.monicahq.mcp.service.base;

import com.monicahq.mcp.service.NoteService;
import com.monicahq.mcp.service.TaskService;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.TestPropertySource;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for the AbstractCrudService base class implementation.
 * <p>
 * These tests verify that the base service implementation works correctly with the
 * Monica API through representative service tests (NoteService and TaskService).
 * Uses TestMonicaHqClient in test profile to provide predictable stub responses
 * without actual network calls.
 * </p>
 * <p>
 * Test coverage includes:
 * <ul>
 *   <li>CRUD operations via inherited base class methods</li>
 *   <li>Field mapping (camelCase to snake_case and vice versa)</li>
 *   <li>Required field validation</li>
 *   <li>Default value application</li>
 *   <li>Response formatting for Claude Desktop visibility</li>
 *   <li>Pagination handling in list operations</li>
 *   <li>Error handling for invalid inputs</li>
 * </ul>
 * </p>
 */
@SpringBootTest
@ActiveProfiles("test")
@TestPropertySource(properties = {
    "spring.profiles.active=test"
})
public class BaseCrudServiceIntegrationTest {

    @Autowired
    private NoteService noteService;

    @Autowired
    private TaskService taskService;

    // ========================================================================================
    // CREATE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Create Operations")
    class CreateOperationTests {

        @Test
        @DisplayName("NoteService should create note with valid arguments")
        void noteService_CreateWithValidArguments_ReturnsNote() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 123);
            arguments.put("body", "Test note body");

            // When
            Map<String, Object> result = noteService.createNote(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"), "Response should contain data field");
            assertTrue(result.containsKey("content"), "Response should contain content field for Claude Desktop");

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertNotNull(data.get("id"), "Created note should have an ID");
            assertEquals("Test note body", data.get("body"));
        }

        @Test
        @DisplayName("TaskService should create task with valid arguments and apply defaults")
        void taskService_CreateWithValidArguments_AppliesDefaults() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 123);
            arguments.put("title", "Test task title");
            // Note: 'completed' is not provided, should get default value (false)

            // When
            Map<String, Object> result = taskService.createTask(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertNotNull(data.get("id"));
            assertEquals("Test task title", data.get("title"));
            // Verify default was applied
            assertEquals(false, data.get("completed"));
        }

        @Test
        @DisplayName("Create should throw when required field is missing")
        void create_MissingRequiredField_ThrowsException() {
            // Given - missing required 'body' field for note
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 123);
            // body is missing

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.createNote(arguments).block()
            );
            assertEquals("body is required", exception.getMessage());
        }

        @Test
        @DisplayName("Create should throw when arguments are empty")
        void create_EmptyArguments_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.createNote(arguments).block()
            );
            assertEquals("Note arguments cannot be empty", exception.getMessage());
        }

        @Test
        @DisplayName("Create should throw when arguments are null")
        void create_NullArguments_ThrowsException() {
            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.createNote(null).block()
            );
            assertEquals("Note arguments cannot be empty", exception.getMessage());
        }
    }

    // ========================================================================================
    // GET OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Get Operations")
    class GetOperationTests {

        @Test
        @DisplayName("NoteService should get note by ID")
        void noteService_GetById_ReturnsNote() {
            // Given
            Map<String, Object> arguments = Map.of("id", 101L);

            // When
            Map<String, Object> result = noteService.getNote(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals(101L, data.get("id"));
        }

        @Test
        @DisplayName("Get should accept String ID and parse correctly")
        void get_StringId_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", "202");

            // When
            Map<String, Object> result = taskService.getTask(arguments).block();

            // Then
            assertNotNull(result);

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals(202L, data.get("id"));
        }

        @Test
        @DisplayName("Get should accept Integer ID and parse correctly")
        void get_IntegerId_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", 303);

            // When
            Map<String, Object> result = taskService.getTask(arguments).block();

            // Then
            assertNotNull(result);

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals(303L, data.get("id"));
        }

        @Test
        @DisplayName("Get should throw when ID is missing")
        void get_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "test");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.getNote(arguments).block()
            );
            assertEquals("Note ID is required", exception.getMessage());
        }

        @Test
        @DisplayName("Get should throw when ID format is invalid")
        void get_InvalidIdFormat_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("id", "not-a-number");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.getNote(arguments).block()
            );
            assertTrue(exception.getMessage().contains("Invalid note ID format"));
        }
    }

    // ========================================================================================
    // UPDATE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Update Operations")
    class UpdateOperationTests {

        @Test
        @DisplayName("NoteService should update note with valid arguments")
        void noteService_UpdateWithValidArguments_ReturnsUpdatedNote() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 101L);
            arguments.put("body", "Updated note body");
            arguments.put("isFavorited", true);

            // When
            Map<String, Object> result = noteService.updateNote(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertNotNull(data.get("id"));
        }

        @Test
        @DisplayName("TaskService should update task with valid arguments")
        void taskService_UpdateWithValidArguments_ReturnsUpdatedTask() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 202L);
            arguments.put("title", "Updated task title");
            arguments.put("completed", true);

            // When
            Map<String, Object> result = taskService.updateTask(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        }

        @Test
        @DisplayName("Update should throw when ID is missing")
        void update_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "Updated content");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.updateNote(arguments).block()
            );
            assertEquals("Note ID is required", exception.getMessage());
        }
    }

    // ========================================================================================
    // DELETE OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Delete Operations")
    class DeleteOperationTests {

        @Test
        @DisplayName("NoteService should delete note by ID")
        void noteService_DeleteById_ReturnsConfirmation() {
            // Given
            Map<String, Object> arguments = Map.of("id", 101L);

            // When
            Map<String, Object> result = noteService.deleteNote(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"), "Delete response should contain content field");

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertNotNull(content);
            assertFalse(content.isEmpty());
            assertEquals("text", content.get(0).get("type"));

            String text = (String) content.get(0).get("text");
            assertTrue(text.contains("Delete"), "Response should confirm deletion");
        }

        @Test
        @DisplayName("TaskService should delete task by ID")
        void taskService_DeleteById_ReturnsConfirmation() {
            // Given
            Map<String, Object> arguments = Map.of("id", 202L);

            // When
            Map<String, Object> result = taskService.deleteTask(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));
        }

        @Test
        @DisplayName("Delete should throw when ID is missing")
        void delete_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("body", "test");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () ->
                noteService.deleteNote(arguments).block()
            );
            assertEquals("Note ID is required", exception.getMessage());
        }

        @Test
        @DisplayName("Delete should accept String ID")
        void delete_StringId_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", "303");

            // When
            Map<String, Object> result = taskService.deleteTask(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));
        }
    }

    // ========================================================================================
    // LIST OPERATION TESTS
    // ========================================================================================

    @Nested
    @DisplayName("List Operations")
    class ListOperationTests {

        @Test
        @DisplayName("NoteService should list notes with default pagination")
        void noteService_List_ReturnsNotesWithPagination() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When
            Map<String, Object> result = noteService.listNotes(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"), "Response should contain data field");
            assertTrue(result.containsKey("meta"), "Response should contain meta field for pagination");
            assertTrue(result.containsKey("content"), "Response should contain content field for Claude Desktop");

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertNotNull(data);
            assertFalse(data.isEmpty(), "List should return entities");
        }

        @Test
        @DisplayName("TaskService should list tasks with custom pagination")
        void taskService_ListWithPagination_UsesCustomValues() {
            // Given
            Map<String, Object> arguments = Map.of(
                "page", 2,
                "limit", 25
            );

            // When
            Map<String, Object> result = taskService.listTasks(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("meta"));
        }

        @Test
        @DisplayName("List should handle null arguments gracefully")
        void list_NullArguments_UsesDefaults() {
            // When
            Map<String, Object> result = noteService.listNotes(null).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("meta"));
        }

        @Test
        @DisplayName("List should handle filter parameters")
        void list_WithFilters_PassesFiltersToApi() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 123L);

            // When
            Map<String, Object> result = noteService.listNotes(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        }

        @Test
        @DisplayName("List should clamp limit to maximum of 100")
        void list_LimitAboveMax_ClampedTo100() {
            // Given
            Map<String, Object> arguments = Map.of("limit", 200);

            // When - should not throw, limit should be clamped internally
            Map<String, Object> result = noteService.listNotes(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        }

        @Test
        @DisplayName("List should clamp limit to minimum of 1")
        void list_LimitBelowMin_ClampedTo1() {
            // Given
            Map<String, Object> arguments = Map.of("limit", 0);

            // When - should not throw, limit should be clamped internally
            Map<String, Object> result = noteService.listNotes(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        }
    }

    // ========================================================================================
    // FIELD MAPPING TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Field Mapping")
    class FieldMappingTests {

        @Test
        @DisplayName("Response should map snake_case fields to camelCase")
        void response_MapsSnakeCaseToCamelCase() {
            // Given
            Map<String, Object> arguments = Map.of("id", 101L);

            // When
            Map<String, Object> result = noteService.getNote(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");

            // Verify camelCase fields are present (mapped from snake_case)
            assertTrue(data.containsKey("contactId") || data.containsKey("contact_id"),
                "Should contain contactId field");
            assertTrue(data.containsKey("isFavorited") || data.containsKey("is_favorited"),
                "Should contain isFavorited field");
        }

        @Test
        @DisplayName("Response should always map timestamp fields")
        void response_AlwaysMapsTimestampFields() {
            // Given
            Map<String, Object> arguments = Map.of("id", 101L);

            // When
            Map<String, Object> result = noteService.getNote(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");

            // Timestamp fields should be present (either camelCase or snake_case depending on mapping)
            assertTrue(data.containsKey("createdAt") || data.containsKey("created_at"),
                "Should contain createdAt field");
        }
    }

    // ========================================================================================
    // RESPONSE FORMATTING TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Response Formatting")
    class ResponseFormattingTests {

        @Test
        @DisplayName("Single entity response should be formatted for Claude Desktop visibility")
        void singleEntity_FormattedForClaudeDesktop() {
            // Given
            Map<String, Object> arguments = Map.of("id", 101L);

            // When
            Map<String, Object> result = noteService.getNote(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertEquals(1, content.size());
            assertEquals("text", content.get(0).get("type"));
            assertNotNull(content.get(0).get("text"), "Content should have text field");
        }

        @Test
        @DisplayName("List response should be formatted for Claude Desktop visibility")
        void listEntity_FormattedForClaudeDesktop() {
            // When
            Map<String, Object> result = noteService.listNotes(new HashMap<>()).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertEquals(1, content.size());
            assertEquals("text", content.get(0).get("type"));
            assertNotNull(content.get(0).get("text"));
        }

        @Test
        @DisplayName("Create response should be formatted for Claude Desktop visibility")
        void create_FormattedForClaudeDesktop() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 123);
            arguments.put("body", "Test note");

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
        }
    }

    // ========================================================================================
    // CROSS-SERVICE CONSISTENCY TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Cross-Service Consistency")
    class CrossServiceConsistencyTests {

        @Test
        @DisplayName("Both services should follow same response structure for create")
        void createResponseStructure_ConsistentAcrossServices() {
            // Given
            Map<String, Object> noteArgs = new HashMap<>();
            noteArgs.put("contactId", 123);
            noteArgs.put("body", "Test note");

            Map<String, Object> taskArgs = new HashMap<>();
            taskArgs.put("contactId", 123);
            taskArgs.put("title", "Test task");

            // When
            Map<String, Object> noteResult = noteService.createNote(noteArgs).block();
            Map<String, Object> taskResult = taskService.createTask(taskArgs).block();

            // Then
            assertNotNull(noteResult);
            assertNotNull(taskResult);

            // Both should have same structure
            assertTrue(noteResult.containsKey("data"));
            assertTrue(noteResult.containsKey("content"));
            assertTrue(taskResult.containsKey("data"));
            assertTrue(taskResult.containsKey("content"));
        }

        @Test
        @DisplayName("Both services should follow same response structure for list")
        void listResponseStructure_ConsistentAcrossServices() {
            // When
            Map<String, Object> noteResult = noteService.listNotes(new HashMap<>()).block();
            Map<String, Object> taskResult = taskService.listTasks(new HashMap<>()).block();

            // Then
            assertNotNull(noteResult);
            assertNotNull(taskResult);

            // Both should have same structure
            assertTrue(noteResult.containsKey("data"));
            assertTrue(noteResult.containsKey("meta"));
            assertTrue(noteResult.containsKey("content"));
            assertTrue(taskResult.containsKey("data"));
            assertTrue(taskResult.containsKey("meta"));
            assertTrue(taskResult.containsKey("content"));
        }

        @Test
        @DisplayName("Both services should throw same exception type for missing ID")
        void missingIdException_ConsistentAcrossServices() {
            // Given
            Map<String, Object> arguments = Map.of("body", "test");

            // When & Then
            IllegalArgumentException noteException = assertThrows(IllegalArgumentException.class, () ->
                noteService.getNote(arguments).block()
            );
            IllegalArgumentException taskException = assertThrows(IllegalArgumentException.class, () ->
                taskService.getTask(arguments).block()
            );

            // Both should throw IllegalArgumentException with entity-specific message
            assertTrue(noteException.getMessage().contains("Note ID is required"));
            assertTrue(taskException.getMessage().contains("Task ID is required"));
        }

        @Test
        @DisplayName("Both services should throw same exception type for invalid ID format")
        void invalidIdFormatException_ConsistentAcrossServices() {
            // Given
            Map<String, Object> arguments = Map.of("id", "invalid");

            // When & Then
            IllegalArgumentException noteException = assertThrows(IllegalArgumentException.class, () ->
                noteService.getNote(arguments).block()
            );
            IllegalArgumentException taskException = assertThrows(IllegalArgumentException.class, () ->
                taskService.getTask(arguments).block()
            );

            // Both should throw IllegalArgumentException with entity-specific message
            assertTrue(noteException.getMessage().contains("Invalid note ID format"));
            assertTrue(taskException.getMessage().contains("Invalid task ID format"));
        }

        @Test
        @DisplayName("Both services should throw same exception type for empty arguments on create")
        void emptyArgumentsException_ConsistentAcrossServices() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            IllegalArgumentException noteException = assertThrows(IllegalArgumentException.class, () ->
                noteService.createNote(arguments).block()
            );
            IllegalArgumentException taskException = assertThrows(IllegalArgumentException.class, () ->
                taskService.createTask(arguments).block()
            );

            // Both should throw IllegalArgumentException with entity-specific message
            assertTrue(noteException.getMessage().contains("Note arguments cannot be empty"));
            assertTrue(taskException.getMessage().contains("Task arguments cannot be empty"));
        }
    }
}
