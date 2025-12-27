package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.GroupFieldMappingConfig;
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
 * Unit tests for GroupService covering CRUD operations,
 * field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class GroupServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private GroupService groupService;

    private Map<String, Object> mockGroupData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        GroupFieldMappingConfig config = new GroupFieldMappingConfig();
        groupService = new GroupService(monicaClient, contentFormatter, config);

        mockGroupData = groupBuilder()
            .id(1L)
            .name("Family")
            .description("Close family members")
            .accountId(1)
            .build();

        mockApiResponse = createSingleEntityResponse(mockGroupData);
    }

    // ========================================================================================
    // CREATE GROUP TESTS
    // ========================================================================================

    @Test
    void createGroup_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "New Group");
        arguments.put("description", "A new group");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted group JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "New Group".equals(data.get("name")) &&
            "A new group".equals(data.get("description"))
        ));
    }

    @Test
    void createGroup_OnlyNameProvided_SucceedsWithoutDescription() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Simple Group");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));

        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "Simple Group".equals(data.get("name")) &&
            !data.containsKey("description")
        ));
    }

    @Test
    void createGroup_MissingName_ThrowsException() {
        // Given - has description but no name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("description", "A group without a name");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.createGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGroup_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.createGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGroup_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.createGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGroup_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.createGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGroup_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.createGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("cannot be empty"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createGroup_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Complete Group");
        arguments.put("description", "Complete group description");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.createGroup(arguments).block();

        // Then
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "Complete Group".equals(data.get("name")) &&
            "Complete group description".equals(data.get("description"))
        ));
    }

    @Test
    void createGroup_FormatterCalledWithRawData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Test Group");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        groupService.createGroup(arguments).block();

        // Then
        verify(contentFormatter).formatAsEscapedJson(any());
    }

    // ========================================================================================
    // GET GROUP TESTS
    // ========================================================================================

    @Test
    void getGroup_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/groups/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/groups/1"), any());
    }

    @Test
    void getGroup_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            groupBuilder().id(42L).name("Test Group").build()
        );

        when(monicaClient.get(eq("/groups/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/groups/42"), any());
    }

    @Test
    void getGroup_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            groupBuilder().id(123L).name("Test Group").build()
        );

        when(monicaClient.get(eq("/groups/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/groups/123"), any());
    }

    @Test
    void getGroup_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.getGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Group ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGroup_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.getGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Group ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGroup_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.getGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid group ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getGroup_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "API Group");
        apiData.put("description", "Group from API");
        apiData.put("account_id", 5);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/groups/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(5, data.get("accountId"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("API Group", data.get("name"));
        assertEquals("Group from API", data.get("description"));
    }

    @Test
    void getGroup_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("name", "Direct Group");
        directResponse.put("account_id", 3);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/groups/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("Direct Group", data.get("name"));
        assertEquals(3, data.get("accountId"));
    }

    @Test
    void getGroup_LargeId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 999999999L);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            groupBuilder().id(999999999L).name("Large ID Group").build()
        );

        when(monicaClient.get(eq("/groups/999999999"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/groups/999999999"), any());
    }

    // ========================================================================================
    // UPDATE GROUP TESTS
    // ========================================================================================

    @Test
    void updateGroup_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Group Name");

        when(monicaClient.put(eq("/groups/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.updateGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/groups/1"), argThat(data ->
            "Updated Group Name".equals(data.get("name"))
        ));
    }

    @Test
    void updateGroup_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("name", "Updated Group");

        when(monicaClient.put(eq("/groups/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.updateGroup(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/groups/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateGroup_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Updated Group");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Group ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_MissingName_ThrowsException() {
        // Given - update requires name
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("description", "Updated description only");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_WithDescription_IncludesDescriptionField() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Group");
        arguments.put("description", "New description");

        when(monicaClient.put(eq("/groups/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.updateGroup(arguments).block();

        // Then
        verify(monicaClient).put(eq("/groups/1"), argThat(data ->
            "Updated Group".equals(data.get("name")) &&
            "New description".equals(data.get("description"))
        ));
    }

    @Test
    void updateGroup_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Group");

        when(monicaClient.put(eq("/groups/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.updateGroup(arguments).block();

        // Then
        verify(monicaClient).put(eq("/groups/42"), any());
    }

    @Test
    void updateGroup_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("name", "Updated Group");

        when(monicaClient.put(eq("/groups/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.updateGroup(arguments).block();

        // Then
        verify(monicaClient).put(eq("/groups/99"), any());
    }

    @Test
    void updateGroup_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Group");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid group ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_EmptyName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_WhitespaceName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "   ");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_NullName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.updateGroup(arguments).block();
        });
        assertEquals("name is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateGroup_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Complete Update Group");
        arguments.put("description", "Complete description update");

        when(monicaClient.put(eq("/groups/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        groupService.updateGroup(arguments).block();

        // Then
        verify(monicaClient).put(eq("/groups/1"), argThat(data ->
            "Complete Update Group".equals(data.get("name")) &&
            "Complete description update".equals(data.get("description"))
        ));
    }

    // ========================================================================================
    // DELETE GROUP TESTS
    // ========================================================================================

    @Test
    void deleteGroup_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/groups/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Group"), eq(1L), eq(true), anyString()
        )).thenReturn("Group with ID 1 has been deleted successfully");

        // When
        Map<String, Object> result = groupService.deleteGroup(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/groups/1"));
    }

    @Test
    void deleteGroup_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/groups/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Group"), eq(99L), eq(true), anyString()
        )).thenReturn("Group with ID 99 has been deleted successfully");

        // When
        Map<String, Object> result = groupService.deleteGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/groups/99"));
    }

    @Test
    void deleteGroup_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/groups/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Group"), eq(55L), eq(true), anyString()
        )).thenReturn("Group with ID 55 has been deleted successfully");

        // When
        Map<String, Object> result = groupService.deleteGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/groups/55"));
    }

    @Test
    void deleteGroup_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Test");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.deleteGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Group ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteGroup_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.deleteGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Group ID is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteGroup_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            groupService.deleteGroup(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Invalid group ID format"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteGroup_MessageContainsId() {
        // Given
        Map<String, Object> arguments = Map.of("id", 42L);
        Map<String, Object> deleteResponse = createDeleteResponse(42L);

        when(monicaClient.delete(eq("/groups/42"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Group"), eq(42L), eq(true), anyString()
        )).thenReturn("Group with ID 42 has been deleted successfully");

        // When
        Map<String, Object> result = groupService.deleteGroup(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertTrue(content.get(0).get("text").toString().contains("42"));
    }

    // ========================================================================================
    // LIST GROUPS TESTS
    // ========================================================================================

    @Test
    void listGroups_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group A").build(),
            groupBuilder().id(2L).name("Group B").build()
        );
        Map<String, Object> listResponse = createListResponse(groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/groups"), any());
    }

    @Test
    void listGroups_WithPagination_PassesCorrectParameters() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group A").build()
        );
        Map<String, Object> listResponse = createListResponse(groups, 2, 20, 50);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then
        verify(monicaClient).get(eq("/groups"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listGroups_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group A").build()
        );
        Map<String, Object> listResponse = createListResponse(groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then - verify default pagination values (page=1, limit=10)
        verify(monicaClient).get(eq("/groups"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listGroups_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups, 1, 10, 100);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

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
    void listGroups_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listGroups_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then
        verify(monicaClient).get(eq("/groups"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listGroups_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups, 3, 10, 30);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then
        verify(monicaClient).get(eq("/groups"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listGroups_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> groupWithSnakeCase = new HashMap<>();
        groupWithSnakeCase.put("id", 1L);
        groupWithSnakeCase.put("name", "Snake Group");
        groupWithSnakeCase.put("description", "A group description");
        groupWithSnakeCase.put("account_id", 15);
        groupWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        groupWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(groupWithSnakeCase));

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(15, data.get(0).get("accountId"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("Snake Group", data.get(0).get("name"));
        assertEquals("A group description", data.get(0).get("description"));
    }

    @Test
    void listGroups_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups, 5, 50, 100);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then
        verify(monicaClient).get(eq("/groups"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listGroups_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listGroups_MultipleGroups_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group A").description("First group").build(),
            groupBuilder().id(2L).name("Group B").description("Second group").build(),
            groupBuilder().id(3L).name("Group C").description("Third group").build()
        );
        Map<String, Object> listResponse = createListResponse(groups, 1, 10, 3);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Group A", data.get(0).get("name"));
        assertEquals("Group B", data.get(1).get("name"));
        assertEquals("Group C", data.get(2).get("name"));
    }

    @Test
    void listGroups_FormatterCalledWithListData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        groupService.listGroups(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(any());
    }

    @Test
    void listGroups_ContentFieldHasCorrectFormat() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> groups = List.of(
            groupBuilder().id(1L).name("Group 1").build()
        );
        Map<String, Object> listResponse = createListResponse(groups);

        when(monicaClient.get(eq("/groups"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = groupService.listGroups(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list JSON", content.get(0).get("text"));
    }

    // ========================================================================================
    // EDGE CASES
    // ========================================================================================

    @Test
    void createGroup_SpecialCharactersInName_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Group & Team (1) - Special!");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "Group & Team (1) - Special!".equals(data.get("name"))
        ));
    }

    @Test
    void createGroup_UnicodeInName_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Famille Internationale");
        arguments.put("description", "Un groupe pour la famille");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "Famille Internationale".equals(data.get("name")) &&
            "Un groupe pour la famille".equals(data.get("description"))
        ));
    }

    @Test
    void createGroup_LongName_Succeeds() {
        // Given
        String longName = "A".repeat(200);
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", longName);

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            longName.equals(data.get("name"))
        ));
    }

    @Test
    void createGroup_LongDescription_Succeeds() {
        // Given
        String longDescription = "This is a very long description. ".repeat(100);
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Group with long description");
        arguments.put("description", longDescription);

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            longDescription.equals(data.get("description"))
        ));
    }

    @Test
    void createGroup_EmptyDescription_PassesThroughCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Group with empty description");
        arguments.put("description", "");

        when(monicaClient.post(eq("/groups"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = groupService.createGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/groups"), argThat(data ->
            "Group with empty description".equals(data.get("name")) &&
            "".equals(data.get("description"))
        ));
    }

    @Test
    void getGroup_WithZeroId_ParsesCorrectly() {
        // Given - Edge case: id = 0
        Map<String, Object> arguments = Map.of("id", 0);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            groupBuilder().id(0L).name("Zero ID Group").build()
        );

        when(monicaClient.get(eq("/groups/0"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted group JSON");

        // When
        Map<String, Object> result = groupService.getGroup(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/groups/0"), any());
    }
}
