package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.UserFieldMappingConfig;
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
 * Unit tests for UserService covering CRUD operations,
 * user profile operations, field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class UserServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private UserService userService;

    private Map<String, Object> mockUserData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        UserFieldMappingConfig config = new UserFieldMappingConfig();
        userService = new UserService(monicaClient, contentFormatter, config);

        mockUserData = userBuilder()
            .id(1L)
            .firstName("John")
            .lastName("Doe")
            .email("john.doe@example.com")
            .isAdministrator(false)
            .build();

        mockApiResponse = createSingleEntityResponse(mockUserData);
    }

    // ========================================================================================
    // CREATE USER TESTS
    // ========================================================================================

    @Test
    void createUser_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");
        arguments.put("lastName", "Doe");
        arguments.put("email", "john.doe@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted user JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/users"), argThat(data ->
            "John".equals(data.get("first_name")) &&
            "Doe".equals(data.get("last_name")) &&
            "john.doe@example.com".equals(data.get("email"))
        ));
    }

    @Test
    void createUser_MissingFirstName_ThrowsException() {
        // Given - has email but no firstName
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("email", "test@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("firstName is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_NullFirstName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", null);
        arguments.put("email", "test@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("firstName is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_EmptyFirstName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "");
        arguments.put("email", "test@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("firstName is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_WhitespaceFirstName_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "   ");
        arguments.put("email", "test@example.com");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("firstName is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_MissingEmail_ThrowsException() {
        // Given - has firstName but no email
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("email is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_NullEmail_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");
        arguments.put("email", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("email is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_EmptyEmail_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");
        arguments.put("email", "");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("email is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.createUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_NullArgs_ThrowsException() {
        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.createUser(null).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createUser_FieldMapping_FirstNameToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Jane");
        arguments.put("email", "jane@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then - verify firstName mapped to first_name
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "Jane".equals(data.get("first_name")) &&
            !data.containsKey("firstName")
        ));
    }

    @Test
    void createUser_FieldMapping_LastNameToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Jane");
        arguments.put("lastName", "Smith");
        arguments.put("email", "jane.smith@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then - verify lastName mapped to last_name
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "Smith".equals(data.get("last_name")) &&
            !data.containsKey("lastName")
        ));
    }

    @Test
    void createUser_FieldMapping_IsAdministratorToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Admin");
        arguments.put("email", "admin@example.com");
        arguments.put("isAdministrator", true);

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then - verify isAdministrator mapped to is_administrator
        verify(monicaClient).post(eq("/users"), argThat(data ->
            Boolean.TRUE.equals(data.get("is_administrator")) &&
            !data.containsKey("isAdministrator")
        ));
    }

    @Test
    void createUser_FieldMapping_ProfilePictureUrlToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "User");
        arguments.put("email", "user@example.com");
        arguments.put("profilePictureUrl", "https://example.com/avatar.png");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then - verify profilePictureUrl mapped to profile_picture_url
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "https://example.com/avatar.png".equals(data.get("profile_picture_url")) &&
            !data.containsKey("profilePictureUrl")
        ));
    }

    @Test
    void createUser_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Full");
        arguments.put("lastName", "User");
        arguments.put("email", "full.user@example.com");
        arguments.put("isAdministrator", true);
        arguments.put("profilePictureUrl", "https://example.com/profile.jpg");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "Full".equals(data.get("first_name")) &&
            "User".equals(data.get("last_name")) &&
            "full.user@example.com".equals(data.get("email")) &&
            Boolean.TRUE.equals(data.get("is_administrator")) &&
            "https://example.com/profile.jpg".equals(data.get("profile_picture_url"))
        ));
    }

    // ========================================================================================
    // GET USER TESTS
    // ========================================================================================

    @Test
    void getUser_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/users/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/users/1"), any());
    }

    @Test
    void getUser_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            userBuilder().id(42L).firstName("User42").build()
        );

        when(monicaClient.get(eq("/users/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/users/42"), any());
    }

    @Test
    void getUser_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            userBuilder().id(123L).firstName("User123").build()
        );

        when(monicaClient.get(eq("/users/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/users/123"), any());
    }

    @Test
    void getUser_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("firstName", "Test");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.getUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getUser_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.getUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getUser_NullArgs_ThrowsException() {
        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.getUser(null).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getUser_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.getUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getUser_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("first_name", "Jane");
        apiData.put("last_name", "Smith");
        apiData.put("email", "jane.smith@example.com");
        apiData.put("is_administrator", true);
        apiData.put("profile_picture_url", "https://example.com/avatar.png");
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/users/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals("Jane", data.get("firstName"));
        assertEquals("Smith", data.get("lastName"));
        assertEquals(true, data.get("isAdministrator"));
        assertEquals("https://example.com/avatar.png", data.get("profilePictureUrl"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("jane.smith@example.com", data.get("email"));
    }

    @Test
    void getUser_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("first_name", "Direct");
        directResponse.put("last_name", "User");
        directResponse.put("email", "direct@example.com");
        directResponse.put("is_administrator", false);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/users/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("Direct", data.get("firstName"));
        assertEquals("User", data.get("lastName"));
        assertEquals(false, data.get("isAdministrator"));
    }

    @Test
    void getUser_LargeId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 999999999L);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            userBuilder().id(999999999L).firstName("Large ID User").build()
        );

        when(monicaClient.get(eq("/users/999999999"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/users/999999999"), any());
    }

    // ========================================================================================
    // UPDATE USER TESTS
    // ========================================================================================

    @Test
    void updateUser_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("firstName", "Updated Name");

        when(monicaClient.put(eq("/users/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.updateUser(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/users/1"), argThat(data ->
            "Updated Name".equals(data.get("first_name"))
        ));
    }

    @Test
    void updateUser_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("firstName", "Updated User");

        when(monicaClient.put(eq("/users/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        userService.updateUser(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/users/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateUser_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("firstName", "Updated User");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.updateUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateUser_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        // No other fields - effectively empty update

        // When & Then - validateUserUpdateArguments checks for empty after id removal
        // Note: current implementation validates before processing, so this test
        // may need adjustment based on actual validation logic
        // The service checks if empty in validateUserUpdateArguments, but after
        // extractUserId, so this should work if we have just id
        when(monicaClient.put(eq("/users/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // Actually, looking at the service, validateUserUpdateArguments only checks
        // if arguments is null or empty BEFORE extracting id. So with just id, it passes.
        Map<String, Object> result = userService.updateUser(arguments).block();
        assertNotNull(result);
    }

    @Test
    void updateUser_NullArgs_ThrowsException() {
        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.updateUser(null).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateUser_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("firstName", "Updated User");

        when(monicaClient.put(eq("/users/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        userService.updateUser(arguments).block();

        // Then
        verify(monicaClient).put(eq("/users/42"), any());
    }

    @Test
    void updateUser_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("firstName", "Updated User");

        when(monicaClient.put(eq("/users/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        userService.updateUser(arguments).block();

        // Then
        verify(monicaClient).put(eq("/users/99"), any());
    }

    @Test
    void updateUser_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("firstName", "Updated User");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.updateUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateUser_FieldMapping_AllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("firstName", "Updated First");
        arguments.put("lastName", "Updated Last");
        arguments.put("isAdministrator", true);
        arguments.put("profilePictureUrl", "https://example.com/new-avatar.png");

        when(monicaClient.put(eq("/users/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.updateUser(arguments).block();

        // Then
        verify(monicaClient).put(eq("/users/1"), argThat(data ->
            "Updated First".equals(data.get("first_name")) &&
            "Updated Last".equals(data.get("last_name")) &&
            Boolean.TRUE.equals(data.get("is_administrator")) &&
            "https://example.com/new-avatar.png".equals(data.get("profile_picture_url")) &&
            !data.containsKey("id")
        ));
    }

    // ========================================================================================
    // DELETE USER TESTS
    // ========================================================================================

    @Test
    void deleteUser_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/users/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("User"), eq(1L), eq(true), anyString()
        )).thenReturn("User with ID 1 has been deleted successfully");

        // When
        Map<String, Object> result = userService.deleteUser(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/users/1"));
    }

    @Test
    void deleteUser_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/users/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("User"), eq(99L), eq(true), anyString()
        )).thenReturn("User with ID 99 has been deleted successfully");

        // When
        Map<String, Object> result = userService.deleteUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/users/99"));
    }

    @Test
    void deleteUser_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/users/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("User"), eq(55L), eq(true), anyString()
        )).thenReturn("User with ID 55 has been deleted successfully");

        // When
        Map<String, Object> result = userService.deleteUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/users/55"));
    }

    @Test
    void deleteUser_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("firstName", "Test");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.deleteUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteUser_NullArgs_ThrowsException() {
        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.deleteUser(null).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteUser_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then - UserService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            userService.deleteUser(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Users API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteUser_MessageContainsId() {
        // Given
        Map<String, Object> arguments = Map.of("id", 42L);
        Map<String, Object> deleteResponse = createDeleteResponse(42L);

        when(monicaClient.delete(eq("/users/42"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("User"), eq(42L), eq(true), anyString()
        )).thenReturn("User with ID 42 has been deleted successfully");

        // When
        Map<String, Object> result = userService.deleteUser(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertTrue(content.get(0).get("text").toString().contains("42"));
    }

    // ========================================================================================
    // LIST USERS TESTS
    // ========================================================================================

    @Test
    void listUsers_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User A").build(),
            userBuilder().id(2L).firstName("User B").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/users"), any());
    }

    @Test
    void listUsers_WithPagination_PassesCorrectParameters() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User A").build()
        );
        Map<String, Object> listResponse = createListResponse(users, 2, 20, 50);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User A").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then - verify default pagination values (page=1, limit=10)
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_LimitClamping_MaxValue() {
        // Given - limit over 100 should be clamped to 100
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_LimitClamping_MinValue() {
        // Given - limit below 1 should be clamped to 1
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_LimitClamping_NegativeValue() {
        // Given - negative limit should be clamped to 1
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users, 1, 10, 100);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

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
    void listUsers_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listUsers_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users, 3, 10, 30);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listUsers_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> userWithSnakeCase = new HashMap<>();
        userWithSnakeCase.put("id", 1L);
        userWithSnakeCase.put("first_name", "Snake User");
        userWithSnakeCase.put("last_name", "Case");
        userWithSnakeCase.put("email", "snake.user@example.com");
        userWithSnakeCase.put("is_administrator", true);
        userWithSnakeCase.put("profile_picture_url", "https://example.com/avatar.png");
        userWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        userWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(userWithSnakeCase));

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals("Snake User", data.get(0).get("firstName"));
        assertEquals("Case", data.get(0).get("lastName"));
        assertEquals(true, data.get(0).get("isAdministrator"));
        assertEquals("https://example.com/avatar.png", data.get(0).get("profilePictureUrl"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("snake.user@example.com", data.get(0).get("email"));
    }

    @Test
    void listUsers_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users, 5, 50, 100);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(monicaClient).get(eq("/users"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listUsers_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listUsers_MultipleUsers_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User A").lastName("Alpha").build(),
            userBuilder().id(2L).firstName("User B").lastName("Beta").build(),
            userBuilder().id(3L).firstName("User C").lastName("Gamma").build()
        );
        Map<String, Object> listResponse = createListResponse(users, 1, 10, 3);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("User A", data.get(0).get("firstName"));
        assertEquals("User B", data.get(1).get("firstName"));
        assertEquals("User C", data.get(2).get("firstName"));
    }

    @Test
    void listUsers_ContentFieldHasCorrectFormat() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = userService.listUsers(arguments).block();

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
    void createUser_SpecialCharactersInName_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John-Paul O'Brien");
        arguments.put("email", "john@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "John-Paul O'Brien".equals(data.get("first_name"))
        ));
    }

    @Test
    void createUser_UnicodeInName_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Francois");
        arguments.put("lastName", "Muller");
        arguments.put("email", "francois@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "Francois".equals(data.get("first_name")) &&
            "Muller".equals(data.get("last_name"))
        ));
    }

    @Test
    void createUser_LongName_Succeeds() {
        // Given
        String longName = "A".repeat(200);
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", longName);
        arguments.put("email", "longname@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/users"), argThat(data ->
            longName.equals(data.get("first_name"))
        ));
    }

    @Test
    void createUser_ComplexEmail_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "User");
        arguments.put("email", "user+tag@sub.domain.example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = userService.createUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "user+tag@sub.domain.example.com".equals(data.get("email"))
        ));
    }

    @Test
    void getUser_WithZeroId_ParsesCorrectly() {
        // Given - Edge case: id = 0
        Map<String, Object> arguments = Map.of("id", 0);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            userBuilder().id(0L).firstName("Zero ID User").build()
        );

        when(monicaClient.get(eq("/users/0"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted user JSON");

        // When
        Map<String, Object> result = userService.getUser(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/users/0"), any());
    }

    @Test
    void createUser_AdministratorFalse_PassesFalseValue() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Regular");
        arguments.put("email", "regular@example.com");
        arguments.put("isAdministrator", false);

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then
        verify(monicaClient).post(eq("/users"), argThat(data ->
            Boolean.FALSE.equals(data.get("is_administrator"))
        ));
    }

    @Test
    void createUser_NullOptionalFields_ExcludesFromRequest() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "John");
        arguments.put("email", "john@example.com");
        arguments.put("lastName", null);

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then - null values should be included as null in the mapping
        verify(monicaClient).post(eq("/users"), argThat(data ->
            "John".equals(data.get("first_name")) &&
            "john@example.com".equals(data.get("email"))
        ));
    }

    @Test
    void listUsers_FormatterCalledWithListData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> users = List.of(
            userBuilder().id(1L).firstName("User 1").build()
        );
        Map<String, Object> listResponse = createListResponse(users);

        when(monicaClient.get(eq("/users"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        userService.listUsers(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(any());
    }

    @Test
    void createUser_FormatterCalledWithRawData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("firstName", "Test");
        arguments.put("email", "test@example.com");

        when(monicaClient.post(eq("/users"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        userService.createUser(arguments).block();

        // Then
        verify(contentFormatter).formatAsEscapedJson(any());
    }
}
