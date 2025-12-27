package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ContactFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
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
 * Comprehensive unit tests for ContactService covering CRUD operations,
 * validation, field mapping, birthdate parsing, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
@org.mockito.junit.jupiter.MockitoSettings(strictness = org.mockito.quality.Strictness.LENIENT)
class ContactServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ContactFieldMappingConfig fieldMappingConfig;

    private ContactService contactService;

    @BeforeEach
    void setUp() {
        fieldMappingConfig = new ContactFieldMappingConfig();
        contactService = new ContactService(monicaClient, contentFormatter, fieldMappingConfig);
        mockContentFormatter(contentFormatter);
    }

    // ========================================================================================
    // CREATE CONTACT TESTS
    // ========================================================================================

    @Nested
    @DisplayName("createContact")
    class CreateContactTests {

        @Test
        @DisplayName("valid args returns formatted response")
        void createContact_ValidArgs_ReturnsFormattedResponse() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("lastName", "Doe");
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(
                contactBuilder().firstName("John").lastName("Doe").build()
            );

            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals("John", data.get("firstName"));
            assertEquals("Doe", data.get("lastName"));
        }

        @Test
        @DisplayName("missing firstName throws exception")
        void createContact_MissingFirstName_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("lastName", "Doe");
            arguments.put("genderId", 1);

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("firstName is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("null firstName throws exception")
        void createContact_NullFirstName_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", null);
            arguments.put("genderId", 1);

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("firstName is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("empty firstName throws exception")
        void createContact_EmptyFirstName_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "   ");
            arguments.put("genderId", 1);

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("firstName is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("missing genderId throws exception")
        void createContact_MissingGenderId_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("genderId is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("null genderId throws exception")
        void createContact_NullGenderId_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", null);

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("genderId is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("sets birthdate boolean defaults")
        void createContact_SetsBirthdateBooleanDefaults() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data -> {
                Boolean isBirthdateKnown = (Boolean) data.get("is_birthdate_known");
                Boolean isDeceased = (Boolean) data.get("is_deceased");
                Boolean isDeceasedDateKnown = (Boolean) data.get("is_deceased_date_known");
                return Boolean.FALSE.equals(isBirthdateKnown) &&
                       Boolean.FALSE.equals(isDeceased) &&
                       Boolean.FALSE.equals(isDeceasedDateKnown);
            }));
        }

        @Test
        @DisplayName("empty arguments throws exception")
        void createContact_EmptyArgs_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.createContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("cannot be empty"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("maps firstName to first_name in API request")
        void createContact_MapsFirstName_ToSnakeCase() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("lastName", "Doe");
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                "John".equals(data.get("first_name")) &&
                "Doe".equals(data.get("last_name")) &&
                Integer.valueOf(1).equals(data.get("gender_id"))
            ));
        }

        @Test
        @DisplayName("maps jobTitle to job_title in API request")
        void createContact_MapsJobTitle_ToSnakeCase() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("jobTitle", "Software Engineer");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                "Software Engineer".equals(data.get("job_title"))
            ));
        }

        @Test
        @DisplayName("with all optional fields maps correctly")
        void createContact_WithAllFields_MapsCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("lastName", "Doe");
            arguments.put("genderId", 2);
            arguments.put("nickname", "Johnny");
            arguments.put("jobTitle", "Engineer");
            arguments.put("company", "TechCorp");

            Map<String, Object> apiResponse = createSingleEntityResponse(
                contactBuilder()
                    .firstName("John")
                    .lastName("Doe")
                    .nickname("Johnny")
                    .jobTitle("Engineer")
                    .company("TechCorp")
                    .build()
            );
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals("John", data.get("firstName"));
            assertEquals("Doe", data.get("lastName"));
        }

        @Test
        @DisplayName("string genderId works correctly")
        void createContact_StringGenderId_WorksCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", "1");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/contacts"), any());
        }
    }

    // ========================================================================================
    // GET CONTACT TESTS
    // ========================================================================================

    @Nested
    @DisplayName("getContact")
    class GetContactTests {

        @Test
        @DisplayName("valid Long ID returns formatted response")
        void getContact_ValidLongId_ReturnsFormattedResponse() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().id(1L).build());

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));
        }

        @Test
        @DisplayName("valid Integer ID returns formatted response")
        void getContact_ValidIntegerId_ReturnsFormattedResponse() {
            // Given
            Map<String, Object> arguments = Map.of("id", 42);
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().id(42L).build());

            when(monicaClient.get(eq("/contacts/42"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).get(eq("/contacts/42"), any());
        }

        @Test
        @DisplayName("String ID parses correctly")
        void getContact_StringId_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", "123");
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().id(123L).build());

            when(monicaClient.get(eq("/contacts/123"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).get(eq("/contacts/123"), any());
        }

        @Test
        @DisplayName("missing ID throws exception")
        void getContact_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.getContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("Contact ID is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("null arguments throws exception")
        void getContact_NullArgs_ThrowsException() {
            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.getContact(null).block();
            });
            assertTrue(exception.getMessage().contains("Contact ID is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("invalid ID format throws exception")
        void getContact_InvalidIdFormat_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("id", "not-a-number");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.getContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("Invalid contact ID format"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("maps snake_case response to camelCase")
        void getContact_MapsSnakeCaseToCamelCase() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> contactData = contactBuilder()
                .id(1L)
                .firstName("John")
                .lastName("Doe")
                .custom("is_birthdate_known", true)
                .custom("is_deceased", false)
                .custom("is_deceased_date_known", false)
                .build();
            Map<String, Object> apiResponse = createSingleEntityResponse(contactData);

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");
            assertEquals("John", data.get("firstName"));
            assertEquals("Doe", data.get("lastName"));
            assertEquals(true, data.get("isBirthdateKnown"));
            assertEquals(false, data.get("isDeceased"));
            assertEquals(false, data.get("isDeceasedDateKnown"));
        }

        @Test
        @DisplayName("handles direct response without data wrapper")
        void getContact_DirectResponse_HandlesCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> directResponse = contactBuilder()
                .id(1L)
                .firstName("Jane")
                .build();

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(directResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
        }

        @Test
        @DisplayName("large ID works correctly")
        void getContact_LargeId_WorksCorrectly() {
            // Given
            Map<String, Object> arguments = Map.of("id", 999999999L);
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().id(999999999L).build());

            when(monicaClient.get(eq("/contacts/999999999"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
        }
    }

    // ========================================================================================
    // UPDATE CONTACT TESTS
    // ========================================================================================

    @Nested
    @DisplayName("updateContact")
    class UpdateContactTests {

        @Test
        @DisplayName("fetches existing contact first")
        void updateContact_FetchesExistingContactFirst() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("firstName", "Jane");

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .lastName("Doe")
                    .genderId(1)
                    .custom("is_birthdate_known", false)
                    .custom("is_deceased", false)
                    .custom("is_deceased_date_known", false)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("Jane")
                    .lastName("Doe")
                    .build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then - verify GET was called before PUT
            verify(monicaClient).get(eq("/contacts/1"), any());
            verify(monicaClient).put(eq("/contacts/1"), any());
        }

        @Test
        @DisplayName("preserves required fields from existing contact")
        void updateContact_PreservesRequiredFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("nickname", "Johnny");

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .lastName("Doe")
                    .genderId(1)
                    .custom("is_birthdate_known", false)
                    .custom("is_deceased", false)
                    .custom("is_deceased_date_known", false)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(1L).firstName("John").nickname("Johnny").build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then - verify required fields were preserved
            verify(monicaClient).put(eq("/contacts/1"), argThat(data -> {
                // firstName should be preserved from existing contact
                return "John".equals(data.get("first_name"));
            }));
        }

        @Test
        @DisplayName("removes id from update body")
        void updateContact_RemovesIdFromUpdateData() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("firstName", "Jane");
            arguments.put("genderId", 2);

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .custom("is_birthdate_known", false)
                    .custom("is_deceased", false)
                    .custom("is_deceased_date_known", false)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(1L).firstName("Jane").build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then - verify id is not in request body
            verify(monicaClient).put(eq("/contacts/1"), argThat(data ->
                !data.containsKey("id")
            ));
        }

        @Test
        @DisplayName("auto-sets isBirthdateKnown when birthdate provided")
        void updateContact_AutoSetsBirthdateKnown_WhenBirthdateProvided() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("birthdate", "1990-05-15");

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .custom("is_birthdate_known", false)
                    .custom("is_deceased", false)
                    .custom("is_deceased_date_known", false)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(1L).birthdate("1990-05-15").build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then - verify isBirthdateKnown is set to true
            verify(monicaClient).put(eq("/contacts/1"), argThat(data ->
                Boolean.TRUE.equals(data.get("is_birthdate_known"))
            ));
        }

        @Test
        @DisplayName("preserves boolean fields from existing contact")
        void updateContact_PreservesBooleanFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("lastName", "Smith");

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .lastName("Doe")
                    .custom("is_birthdate_known", true)
                    .custom("is_deceased", true)
                    .custom("is_deceased_date_known", true)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(1L).lastName("Smith").build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then - verify boolean fields were preserved
            verify(monicaClient).put(eq("/contacts/1"), argThat(data ->
                Boolean.TRUE.equals(data.get("is_birthdate_known")) &&
                Boolean.TRUE.equals(data.get("is_deceased")) &&
                Boolean.TRUE.equals(data.get("is_deceased_date_known"))
            ));
        }

        @Test
        @DisplayName("missing ID throws exception")
        void updateContact_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "Jane");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.updateContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("Contact ID is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("string ID works correctly")
        void updateContact_StringId_WorksCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", "123");
            arguments.put("firstName", "Jane");

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder()
                    .id(123L)
                    .firstName("John")
                    .custom("is_birthdate_known", false)
                    .custom("is_deceased", false)
                    .custom("is_deceased_date_known", false)
                    .build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(123L).firstName("Jane").build()
            );

            when(monicaClient.get(eq("/contacts/123"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/123"), any())).thenReturn(Mono.just(updatedContact));

            // When
            Map<String, Object> result = contactService.updateContact(arguments).block();

            // Then
            assertNotNull(result);
        }

        @Test
        @DisplayName("maps camelCase fields to snake_case")
        void updateContact_MapsCamelCaseToSnakeCase() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("firstName", "Jane");
            arguments.put("lastName", "Smith");
            arguments.put("jobTitle", "Manager");
            arguments.put("isBirthdateKnown", true);
            arguments.put("isDeceased", false);
            arguments.put("isDeceasedDateKnown", false);

            Map<String, Object> existingContact = createSingleEntityResponse(
                contactBuilder().id(1L).firstName("John").build()
            );

            Map<String, Object> updatedContact = createSingleEntityResponse(
                contactBuilder().id(1L).firstName("Jane").build()
            );

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(existingContact));
            when(monicaClient.put(eq("/contacts/1"), any())).thenReturn(Mono.just(updatedContact));

            // When
            contactService.updateContact(arguments).block();

            // Then
            verify(monicaClient).put(eq("/contacts/1"), argThat(data ->
                "Jane".equals(data.get("first_name")) &&
                "Smith".equals(data.get("last_name")) &&
                "Manager".equals(data.get("job_title"))
            ));
        }
    }

    // ========================================================================================
    // DELETE CONTACT TESTS
    // ========================================================================================

    @Nested
    @DisplayName("deleteContact")
    class DeleteContactTests {

        @Test
        @DisplayName("returns formatted success message")
        void deleteContact_ReturnsFormattedSuccessMessage() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> deleteResponse = createDeleteResponse(1L);

            when(monicaClient.delete(eq("/contacts/1"))).thenReturn(Mono.just(deleteResponse));
            when(contentFormatter.formatOperationResult(
                eq("Delete"), eq("Contact"), eq(1L), eq(true), anyString()
            )).thenReturn("Contact deleted successfully");

            // When
            Map<String, Object> result = contactService.deleteContact(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
            assertEquals(1, content.size());
            assertEquals("text", content.get(0).get("type"));
        }

        @Test
        @DisplayName("valid ID types work correctly")
        void deleteContact_ValidIdTypes_WorkCorrectly() {
            // Given - Long ID
            Map<String, Object> deleteResponse = createDeleteResponse(42L);
            when(monicaClient.delete(eq("/contacts/42"))).thenReturn(Mono.just(deleteResponse));
            when(contentFormatter.formatOperationResult(any(), any(), any(), anyBoolean(), anyString()))
                .thenReturn("Deleted");

            // When & Then - Long
            Map<String, Object> result1 = contactService.deleteContact(Map.of("id", 42L)).block();
            assertNotNull(result1);

            // When & Then - Integer
            when(monicaClient.delete(eq("/contacts/43"))).thenReturn(Mono.just(createDeleteResponse(43L)));
            Map<String, Object> result2 = contactService.deleteContact(Map.of("id", 43)).block();
            assertNotNull(result2);

            // When & Then - String
            when(monicaClient.delete(eq("/contacts/44"))).thenReturn(Mono.just(createDeleteResponse(44L)));
            Map<String, Object> result3 = contactService.deleteContact(Map.of("id", "44")).block();
            assertNotNull(result3);
        }

        @Test
        @DisplayName("missing ID throws exception")
        void deleteContact_MissingId_ThrowsException() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.deleteContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("Contact ID is required"));
            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("invalid ID format throws exception")
        void deleteContact_InvalidIdFormat_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("id", "invalid");

            // When & Then
            IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
                contactService.deleteContact(arguments).block();
            });
            assertTrue(exception.getMessage().contains("Invalid contact ID format"));
            verifyNoInteractions(monicaClient);
        }
    }

    // ========================================================================================
    // LIST CONTACTS TESTS
    // ========================================================================================

    @Nested
    @DisplayName("listContacts")
    class ListContactsTests {

        @Test
        @DisplayName("with default pagination")
        void listContacts_DefaultPagination_AppliesDefaults() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "1".equals(params.get("page")) && "10".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("clamps limit to 100")
        void listContacts_ClampsLimitTo100() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("limit", 150);

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 100, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "100".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("clamps limit to minimum 1")
        void listContacts_ClampsLimitToMin1() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("limit", 0);

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 1, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "1".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("negative limit clamps to 1")
        void listContacts_NegativeLimit_ClampsTo1() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("limit", -5);

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 1, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "1".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("custom pagination values work")
        void listContacts_CustomPagination_Works() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("page", 3);
            arguments.put("limit", 25);

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                3, 25, 100
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "3".equals(params.get("page")) && "25".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("string pagination values parse correctly")
        void listContacts_StringPagination_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("page", "2");
            arguments.put("limit", "15");

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                2, 15, 50
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "2".equals(params.get("page")) && "15".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("returns formatted response with metadata")
        void listContacts_ReturnsFormattedResponse_WithMetadata() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiResponse = createListResponse(
                List.of(
                    contactBuilder().id(1L).firstName("John").build(),
                    contactBuilder().id(2L).firstName("Jane").build()
                ),
                1, 10, 2
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.listContacts(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("meta"));
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertEquals(2, data.size());
        }

        @Test
        @DisplayName("empty results handled correctly")
        void listContacts_EmptyResults_HandledCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiResponse = createListResponse(List.of(), 1, 10, 0);

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.listContacts(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertTrue(data.isEmpty());
        }

        @Test
        @DisplayName("search filter maps to query param")
        void listContacts_SearchFilter_MapsToQueryParam() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("search", "john");

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().firstName("John").build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "john".equals(params.get("query"))
            ));
        }

        @Test
        @DisplayName("tag filter maps to tags param")
        void listContacts_TagFilter_MapsToTagsParam() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("tagId", 5L);

            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "5".equals(params.get("tags"))
            ));
        }

        @Test
        @DisplayName("maps response fields from snake_case to camelCase")
        void listContacts_MapsResponseFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder()
                    .id(1L)
                    .firstName("John")
                    .lastName("Doe")
                    .custom("job_title", "Engineer")
                    .custom("is_birthdate_known", true)
                    .build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.listContacts(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertEquals(1, data.size());

            Map<String, Object> contact = data.get(0);
            assertEquals("John", contact.get("firstName"));
            assertEquals("Doe", contact.get("lastName"));
            assertEquals("Engineer", contact.get("jobTitle"));
            assertEquals(true, contact.get("isBirthdateKnown"));
        }
    }

    // ========================================================================================
    // SEARCH CONTACTS TESTS
    // ========================================================================================

    @Nested
    @DisplayName("searchContacts")
    class SearchContactsTests {

        @Test
        @DisplayName("should return formatted results")
        void searchContacts_ShouldReturnFormattedResults() {
            // Given
            Map<String, Object> arguments = Map.of(
                "query", "John",
                "limit", 10
            );

            Map<String, Object> mockApiResponse = createListResponse(
                List.of(contactBuilder().firstName("John").build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));

            // When
            Map<String, Object> result = contactService.searchContacts(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));
            assertTrue(result.containsKey("meta"));

            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                params.containsKey("query") && "John".equals(params.get("query"))
            ));
        }

        @Test
        @DisplayName("with invalid limit should clamp to valid range")
        void searchContacts_WithInvalidLimit_ShouldClampToValidRange() {
            // Given
            Map<String, Object> arguments = Map.of(
                "query", "test",
                "limit", 150 // Above maximum
            );

            Map<String, Object> mockApiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 100, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));

            // When
            contactService.searchContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "100".equals(params.get("limit")) // Clamped to maximum
            ));
        }

        @Test
        @DisplayName("empty query defaults to empty string")
        void searchContacts_EmptyQuery_DefaultsToEmptyString() {
            // Given
            Map<String, Object> arguments = new HashMap<>();

            Map<String, Object> mockApiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(mockApiResponse));

            // When
            contactService.searchContacts(arguments).block();

            // Then
            verify(monicaClient).get(eq("/contacts"), argThat(params ->
                "".equals(params.get("query"))
            ));
        }
    }

    // ========================================================================================
    // UPDATE CONTACT CAREER TESTS
    // ========================================================================================

    @Nested
    @DisplayName("updateContactCareer")
    class UpdateContactCareerTests {

        @Test
        @DisplayName("should call correct endpoint")
        void updateContactCareer_ShouldCallCorrectEndpoint() {
            // Given
            Map<String, Object> arguments = Map.of(
                "id", 1L,
                "jobTitle", "Software Engineer",
                "company", "TechCorp",
                "startDate", "2023-01-15"
            );

            Map<String, Object> careerResponse = createSingleEntityResponse(
                Map.of(
                    "id", 1L,
                    "job_title", "Software Engineer",
                    "company", "TechCorp"
                )
            );

            when(monicaClient.put(eq("/contacts/1/work"), any())).thenReturn(Mono.just(careerResponse));

            // When
            Map<String, Object> result = contactService.updateContactCareer(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));

            verify(monicaClient).put(eq("/contacts/1/work"), argThat(data ->
                "Software Engineer".equals(data.get("job_title")) &&
                "TechCorp".equals(data.get("company"))
            ));
        }

        @Test
        @DisplayName("without contact ID should throw exception")
        void updateContactCareer_WithoutContactId_ShouldThrowException() {
            // Given
            Map<String, Object> arguments = Map.of(
                "jobTitle", "Software Engineer",
                "company", "TechCorp"
            );

            // When & Then
            assertThrows(IllegalArgumentException.class, () -> {
                contactService.updateContactCareer(arguments).block();
            });

            verifyNoInteractions(monicaClient);
        }

        @Test
        @DisplayName("maps all career fields correctly")
        void updateContactCareer_MapsAllCareerFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("id", 1L);
            arguments.put("jobTitle", "Senior Developer");
            arguments.put("company", "TechCorp");
            arguments.put("startDate", "2020-01-01");
            arguments.put("endDate", "2023-12-31");
            arguments.put("salary", "100000");

            Map<String, Object> careerResponse = createSingleEntityResponse(Map.of("id", 1L));
            when(monicaClient.put(eq("/contacts/1/work"), any())).thenReturn(Mono.just(careerResponse));

            // When
            contactService.updateContactCareer(arguments).block();

            // Then
            verify(monicaClient).put(eq("/contacts/1/work"), argThat(data ->
                "Senior Developer".equals(data.get("job_title")) &&
                "TechCorp".equals(data.get("company")) &&
                "2020-01-01".equals(data.get("start_date")) &&
                "2023-12-31".equals(data.get("end_date")) &&
                "100000".equals(data.get("salary"))
            ));
        }
    }

    // ========================================================================================
    // GET CONTACT AUDIT LOGS TESTS
    // ========================================================================================

    @Nested
    @DisplayName("getContactAuditLogs")
    class GetContactAuditLogsTests {

        @Test
        @DisplayName("should return formatted logs")
        void getContactAuditLogs_ShouldReturnFormattedLogs() {
            // Given
            Map<String, Object> arguments = Map.of(
                "id", 1L,
                "limit", 20
            );

            Map<String, Object> auditResponse = createListResponse(
                List.of(
                    Map.of(
                        "id", 1L,
                        "action", "create",
                        "description", "Contact created",
                        "created_at", "2023-01-15T10:00:00Z"
                    )
                ),
                1, 20, 1
            );

            when(monicaClient.get(eq("/contacts/1/logs"), any())).thenReturn(Mono.just(auditResponse));

            // When
            Map<String, Object> result = contactService.getContactAuditLogs(arguments).block();

            // Then
            assertNotNull(result);
            assertTrue(result.containsKey("data"));
            assertTrue(result.containsKey("content"));

            @SuppressWarnings("unchecked")
            List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
            assertEquals(1, data.size());

            verify(monicaClient).get(eq("/contacts/1/logs"), argThat(params ->
                "20".equals(params.get("limit"))
            ));
        }

        @Test
        @DisplayName("without contact ID should throw exception")
        void getContactAuditLogs_WithoutContactId_ShouldThrowException() {
            // Given
            Map<String, Object> arguments = Map.of("limit", 20);

            // When & Then
            assertThrows(IllegalArgumentException.class, () -> {
                contactService.getContactAuditLogs(arguments).block();
            });

            verifyNoInteractions(monicaClient);
        }
    }

    // ========================================================================================
    // BIRTHDATE PARSING TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Birthdate Parsing")
    class BirthdateParsingTests {

        @Test
        @DisplayName("parses YYYY-MM-DD to year, month, day integers")
        void birthdate_ParsesYYYYMMDD_ToIntegers() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "1990-05-15");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(1990).equals(data.get("year")) &&
                Integer.valueOf(5).equals(data.get("month")) &&
                Integer.valueOf(15).equals(data.get("day"))
            ));
        }

        @Test
        @DisplayName("parses birthdate with leading zeros correctly")
        void birthdate_ParsesLeadingZeros_Correctly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "Jane");
            arguments.put("genderId", 2);
            arguments.put("birthdate", "2001-03-07");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(2001).equals(data.get("year")) &&
                Integer.valueOf(3).equals(data.get("month")) &&
                Integer.valueOf(7).equals(data.get("day"))
            ));
        }

        @Test
        @DisplayName("parses December 31st correctly")
        void birthdate_ParsesDecember31_Correctly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "1985-12-31");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(1985).equals(data.get("year")) &&
                Integer.valueOf(12).equals(data.get("month")) &&
                Integer.valueOf(31).equals(data.get("day"))
            ));
        }

        @Test
        @DisplayName("parses January 1st correctly")
        void birthdate_ParsesJanuary1_Correctly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "2000-01-01");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(2000).equals(data.get("year")) &&
                Integer.valueOf(1).equals(data.get("month")) &&
                Integer.valueOf(1).equals(data.get("day"))
            ));
        }

        @Test
        @DisplayName("null birthdate does not add year/month/day fields")
        void birthdate_NullValue_DoesNotAddFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", null);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                !data.containsKey("year") &&
                !data.containsKey("month") &&
                !data.containsKey("day")
            ));
        }

        @Test
        @DisplayName("empty string birthdate does not add year/month/day fields")
        void birthdate_EmptyString_DoesNotAddFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                !data.containsKey("year") &&
                !data.containsKey("month") &&
                !data.containsKey("day")
            ));
        }

        @Test
        @DisplayName("whitespace birthdate does not add year/month/day fields")
        void birthdate_WhitespaceValue_DoesNotAddFields() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "   ");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                !data.containsKey("year") &&
                !data.containsKey("month") &&
                !data.containsKey("day")
            ));
        }

        @Test
        @DisplayName("leap year date parses correctly")
        void birthdate_LeapYear_ParsesCorrectly() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("birthdate", "2000-02-29");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(2000).equals(data.get("year")) &&
                Integer.valueOf(2).equals(data.get("month")) &&
                Integer.valueOf(29).equals(data.get("day"))
            ));
        }
    }

    // ========================================================================================
    // API FORMAT MAPPING TESTS
    // ========================================================================================

    @Nested
    @DisplayName("API Format Mapping")
    class ApiFormatMappingTests {

        @Test
        @DisplayName("maps all camelCase fields to snake_case for create")
        void apiMapping_Create_CamelCaseToSnakeCase() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("lastName", "Doe");
            arguments.put("genderId", 1);
            arguments.put("jobTitle", "Engineer");
            arguments.put("isBirthdateKnown", true);
            arguments.put("isDeceased", false);
            arguments.put("isDeceasedDateKnown", false);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                data.containsKey("first_name") &&
                data.containsKey("last_name") &&
                data.containsKey("gender_id") &&
                data.containsKey("job_title") &&
                data.containsKey("is_birthdate_known") &&
                data.containsKey("is_deceased") &&
                data.containsKey("is_deceased_date_known") &&
                !data.containsKey("firstName") &&
                !data.containsKey("lastName") &&
                !data.containsKey("genderId") &&
                !data.containsKey("jobTitle") &&
                !data.containsKey("isBirthdateKnown") &&
                !data.containsKey("isDeceased") &&
                !data.containsKey("isDeceasedDateKnown")
            ));
        }

        @Test
        @DisplayName("maps all snake_case response fields to camelCase")
        void apiMapping_Response_SnakeCaseToCamelCase() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> contactData = new HashMap<>();
            contactData.put("id", 1L);
            contactData.put("first_name", "John");
            contactData.put("last_name", "Doe");
            contactData.put("gender_id", 1);
            contactData.put("job_title", "Engineer");
            contactData.put("is_birthdate_known", true);
            contactData.put("is_deceased", false);
            contactData.put("is_deceased_date_known", false);
            contactData.put("created_at", "2023-01-15T10:00:00Z");
            contactData.put("updated_at", "2023-01-15T10:00:00Z");

            Map<String, Object> apiResponse = createSingleEntityResponse(contactData);
            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then
            assertNotNull(result);
            @SuppressWarnings("unchecked")
            Map<String, Object> data = (Map<String, Object>) result.get("data");

            assertEquals("John", data.get("firstName"));
            assertEquals("Doe", data.get("lastName"));
            assertEquals(1, data.get("genderId"));
            assertEquals("Engineer", data.get("jobTitle"));
            assertEquals(true, data.get("isBirthdateKnown"));
            assertEquals(false, data.get("isDeceased"));
            assertEquals(false, data.get("isDeceasedDateKnown"));
            assertEquals("2023-01-15T10:00:00Z", data.get("createdAt"));
            assertEquals("2023-01-15T10:00:00Z", data.get("updatedAt"));
        }

        @Test
        @DisplayName("unknown fields pass through unchanged")
        void apiMapping_UnknownFields_PassThrough() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 1);
            arguments.put("customField", "custom value");
            arguments.put("another_field", 42);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.createContact(arguments).block();

            // Then
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                "custom value".equals(data.get("customField")) &&
                Integer.valueOf(42).equals(data.get("another_field"))
            ));
        }
    }

    // ========================================================================================
    // EDGE CASE TESTS
    // ========================================================================================

    @Nested
    @DisplayName("Edge Cases")
    class EdgeCaseTests {

        @Test
        @DisplayName("special characters in firstName handled correctly")
        void edgeCase_SpecialCharacters_InFirstName() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "José María");
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(
                contactBuilder().firstName("José María").build()
            );
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                "José María".equals(data.get("first_name"))
            ));
        }

        @Test
        @DisplayName("unicode characters in name handled correctly")
        void edgeCase_UnicodeCharacters_InName() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "田中");
            arguments.put("lastName", "太郎");
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(
                contactBuilder().firstName("田中").lastName("太郎").build()
            );
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                "田中".equals(data.get("first_name")) &&
                "太郎".equals(data.get("last_name"))
            ));
        }

        @Test
        @DisplayName("very long firstName accepted")
        void edgeCase_VeryLongFirstName_Accepted() {
            // Given
            String longName = "A".repeat(200);
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", longName);
            arguments.put("genderId", 1);

            Map<String, Object> apiResponse = createSingleEntityResponse(
                contactBuilder().firstName(longName).build()
            );
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                longName.equals(data.get("first_name"))
            ));
        }

        @Test
        @DisplayName("large genderId value works")
        void edgeCase_LargeGenderId_Works() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("firstName", "John");
            arguments.put("genderId", 999);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());
            when(monicaClient.post(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.createContact(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/contacts"), argThat(data ->
                Integer.valueOf(999).equals(data.get("gender_id"))
            ));
        }

        @Test
        @DisplayName("zero contact ID throws exception")
        void edgeCase_ZeroContactId_ThrowsException() {
            // Given
            Map<String, Object> arguments = Map.of("id", 0L);

            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().id(0L).build());
            when(monicaClient.get(eq("/contacts/0"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

            // Then - zero ID should be accepted and passed to API
            assertNotNull(result);
            verify(monicaClient).get(eq("/contacts/0"), any());
        }

        @Test
        @DisplayName("content field contains escaped JSON")
        void edgeCase_ContentField_ContainsEscapedJson() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(apiResponse));

            // When
            Map<String, Object> result = contactService.getContact(arguments).block();

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
        @DisplayName("formatter is invoked for single entity response")
        void edgeCase_FormatterInvoked_ForSingleEntity() {
            // Given
            Map<String, Object> arguments = Map.of("id", 1L);
            Map<String, Object> apiResponse = createSingleEntityResponse(contactBuilder().build());

            when(monicaClient.get(eq("/contacts/1"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.getContact(arguments).block();

            // Then
            verify(contentFormatter).formatAsEscapedJson(any());
        }

        @Test
        @DisplayName("formatter is invoked for list response")
        void edgeCase_FormatterInvoked_ForList() {
            // Given
            Map<String, Object> arguments = new HashMap<>();
            Map<String, Object> apiResponse = createListResponse(
                List.of(contactBuilder().build()),
                1, 10, 1
            );

            when(monicaClient.get(eq("/contacts"), any())).thenReturn(Mono.just(apiResponse));

            // When
            contactService.listContacts(arguments).block();

            // Then
            verify(contentFormatter).formatListAsEscapedJson(any());
        }
    }
}
