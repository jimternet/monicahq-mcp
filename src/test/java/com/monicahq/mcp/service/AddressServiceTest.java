package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.AddressFieldMappingConfig;
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
 * Unit tests for AddressService covering CRUD operations, validation,
 * country linking, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class AddressServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private AddressService addressService;

    private Map<String, Object> mockAddressData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        AddressFieldMappingConfig fieldMappingConfig = new AddressFieldMappingConfig();
        addressService = new AddressService(monicaClient, contentFormatter, fieldMappingConfig);

        mockAddressData = addressBuilder()
            .id(1L)
            .contactId(100L)
            .name("Home")
            .street("123 Main St")
            .city("Springfield")
            .province("IL")
            .postalCode("62701")
            .country("US")
            .build();

        mockApiResponse = createSingleEntityResponse(mockAddressData);
    }

    // ========================================================================================
    // CREATE ADDRESS TESTS
    // ========================================================================================

    @Test
    void createAddress_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("name", "Home");
        arguments.put("street", "123 Main St");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted address JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "Home".equals(data.get("name")) &&
            "123 Main St".equals(data.get("street"))
        ));
    }

    @Test
    void createAddress_MissingContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("name", "Home");
        arguments.put("street", "123 Main St");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.createAddress(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createAddress_NullContactId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);
        arguments.put("name", "Home");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.createAddress(arguments).block();
        });
        assertEquals("contactId is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createAddress_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.createAddress(arguments).block();
        });
        assertEquals("Address arguments cannot be empty", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createAddress_StringContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", "100");
        arguments.put("name", "Home");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "100".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createAddress_IntegerContactId_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100);
        arguments.put("name", "Home");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            Integer.valueOf(100).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createAddress_MapsContactIdToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.containsKey("contact_id") &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void createAddress_MapsPostalCodeToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("postalCode", "12345");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "12345".equals(data.get("postal_code")) &&
            !data.containsKey("postalCode")
        ));
    }

    @Test
    void createAddress_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("name", "Home");
        arguments.put("street", "123 Main St");
        arguments.put("city", "Springfield");
        arguments.put("province", "IL");
        arguments.put("postalCode", "62701");
        arguments.put("country", "US");
        arguments.put("latitude", 39.7817);
        arguments.put("longitude", -89.6501);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            "Home".equals(data.get("name")) &&
            "123 Main St".equals(data.get("street")) &&
            "Springfield".equals(data.get("city")) &&
            "IL".equals(data.get("province")) &&
            "62701".equals(data.get("postal_code")) &&
            "US".equals(data.get("country")) &&
            Double.valueOf(39.7817).equals(data.get("latitude")) &&
            Double.valueOf(-89.6501).equals(data.get("longitude"))
        ));
    }

    @Test
    void createAddress_PassThroughFields_NotRemapped() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("name", "Office");
        arguments.put("street", "456 Business Ave");
        arguments.put("city", "Chicago");
        arguments.put("province", "IL");
        arguments.put("country", "US");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then - verify these fields pass through unchanged
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "Office".equals(data.get("name")) &&
            "456 Business Ave".equals(data.get("street")) &&
            "Chicago".equals(data.get("city")) &&
            "IL".equals(data.get("province")) &&
            "US".equals(data.get("country"))
        ));
    }

    // ========================================================================================
    // GET ADDRESS TESTS
    // ========================================================================================

    @Test
    void getAddress_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/addresses/1"), any());
    }

    @Test
    void getAddress_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            addressBuilder().id(42L).name("Work").build()
        );

        when(monicaClient.get(eq("/addresses/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/addresses/42"), any());
    }

    @Test
    void getAddress_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            addressBuilder().id(123L).name("Vacation").build()
        );

        when(monicaClient.get(eq("/addresses/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/addresses/123"), any());
    }

    @Test
    void getAddress_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Home");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.getAddress(arguments).block();
        });
        assertEquals("Address ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAddress_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.getAddress(arguments).block();
        });
        assertEquals("Address ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAddress_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.getAddress(arguments).block();
        });
        assertEquals("Invalid address ID format: not-a-number", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getAddress_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("name", "Home");
        apiData.put("street", "123 Main St");
        apiData.put("city", "Springfield");
        apiData.put("province", "IL");
        apiData.put("contact_id", 100L);
        apiData.put("postal_code", "62701");
        apiData.put("country", "US");
        apiData.put("latitude", 39.7817);
        apiData.put("longitude", -89.6501);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T11:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/addresses/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(100L, data.get("contactId"));
        assertEquals("62701", data.get("postalCode"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get("updatedAt"));
        // These fields should pass through unchanged
        assertEquals("Home", data.get("name"));
        assertEquals("123 Main St", data.get("street"));
        assertEquals("Springfield", data.get("city"));
        assertEquals("IL", data.get("province"));
        assertEquals("US", data.get("country"));
        assertEquals(39.7817, data.get("latitude"));
        assertEquals(-89.6501, data.get("longitude"));
    }

    @Test
    void getAddress_DirectResponse_HandlesCorrectly() {
        // Given - response without data wrapper
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> directApiResponse = new HashMap<>();
        directApiResponse.put("id", 1L);
        directApiResponse.put("name", "Direct Response Address");
        directApiResponse.put("contact_id", 100L);
        directApiResponse.put("postal_code", "12345");

        when(monicaClient.get(eq("/addresses/1"), any())).thenReturn(Mono.just(directApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertEquals("Direct Response Address", data.get("name"));
        assertEquals(100L, data.get("contactId"));
        assertEquals("12345", data.get("postalCode"));
    }

    // ========================================================================================
    // UPDATE ADDRESS TESTS
    // ========================================================================================

    @Test
    void updateAddress_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Home");
        arguments.put("street", "456 Oak Ave");

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        Map<String, Object> result = addressService.updateAddress(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            "Updated Home".equals(data.get("name")) &&
            "456 Oak Ave".equals(data.get("street"))
        ));
    }

    @Test
    void updateAddress_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("name", "Updated Address");

        when(monicaClient.put(eq("/addresses/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/addresses/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateAddress_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Updated Address");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.updateAddress(arguments).block();
        });
        assertEquals("Address ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateAddress_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("name", "Updated Address");

        when(monicaClient.put(eq("/addresses/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/42"), any());
    }

    @Test
    void updateAddress_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("name", "Updated Address");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.updateAddress(arguments).block();
        });
        assertEquals("Invalid address ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateAddress_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("name", "Updated Address");

        when(monicaClient.put(eq("/addresses/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted address JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/99"), any());
    }

    @Test
    void updateAddress_WithPostalCode_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("postalCode", "54321");

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            "54321".equals(data.get("postal_code")) &&
            !data.containsKey("postalCode")
        ));
    }

    @Test
    void updateAddress_WithContactId_MapsToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            data.get("contact_id").equals(200L) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void updateAddress_WithAllFields_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);
        arguments.put("name", "New Office");
        arguments.put("street", "789 Business Blvd");
        arguments.put("city", "New York");
        arguments.put("province", "NY");
        arguments.put("postalCode", "10001");
        arguments.put("country", "US");
        arguments.put("latitude", 40.7128);
        arguments.put("longitude", -74.0060);

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            data.get("contact_id").equals(200L) &&
            "New Office".equals(data.get("name")) &&
            "789 Business Blvd".equals(data.get("street")) &&
            "New York".equals(data.get("city")) &&
            "NY".equals(data.get("province")) &&
            "10001".equals(data.get("postal_code")) &&
            "US".equals(data.get("country")) &&
            Double.valueOf(40.7128).equals(data.get("latitude")) &&
            Double.valueOf(-74.0060).equals(data.get("longitude")) &&
            !data.containsKey("id") &&
            !data.containsKey("contactId") &&
            !data.containsKey("postalCode")
        ));
    }

    // ========================================================================================
    // DELETE ADDRESS TESTS
    // ========================================================================================

    @Test
    void deleteAddress_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/addresses/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Address"), eq(1L), eq(true), anyString()
        )).thenReturn("Address deleted successfully");

        // When
        Map<String, Object> result = addressService.deleteAddress(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));

        verify(monicaClient).delete(eq("/addresses/1"));
    }

    @Test
    void deleteAddress_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/addresses/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Address"), eq(99L), eq(true), anyString()
        )).thenReturn("Address deleted successfully");

        // When
        Map<String, Object> result = addressService.deleteAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/addresses/99"));
    }

    @Test
    void deleteAddress_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/addresses/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Address"), eq(55L), eq(true), anyString()
        )).thenReturn("Address deleted successfully");

        // When
        Map<String, Object> result = addressService.deleteAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/addresses/55"));
    }

    @Test
    void deleteAddress_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("name", "Home");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.deleteAddress(arguments).block();
        });
        assertEquals("Address ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteAddress_NullId_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", null);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.deleteAddress(arguments).block();
        });
        assertEquals("Address ID is required", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteAddress_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            addressService.deleteAddress(arguments).block();
        });
        assertEquals("Invalid address ID format: invalid", exception.getMessage());
        verifyNoInteractions(monicaClient);
    }

    // ========================================================================================
    // LIST ADDRESSES TESTS
    // ========================================================================================

    @Test
    void listAddresses_WithPagination_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build(),
            addressBuilder().id(2L).name("Work").build()
        );
        Map<String, Object> listResponse = createListResponse(addresses, 2, 20, 50);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));
        assertTrue(result.containsKey("meta"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/addresses"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listAddresses_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build()
        );
        Map<String, Object> listResponse = createListResponse(addresses);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        addressService.listAddresses(arguments).block();

        // Then - verify default pagination values
        verify(monicaClient).get(eq("/addresses"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listAddresses_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build()
        );
        Map<String, Object> listResponse = createListResponse(addresses, 1, 10, 100);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

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
    void listAddresses_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listAddresses_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build()
        );
        Map<String, Object> listResponse = createListResponse(addresses);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        addressService.listAddresses(arguments).block();

        // Then
        verify(monicaClient).get(eq("/addresses"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listAddresses_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build()
        );
        Map<String, Object> listResponse = createListResponse(addresses, 3, 10, 30);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        addressService.listAddresses(arguments).block();

        // Then
        verify(monicaClient).get(eq("/addresses"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listAddresses_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> addressWithFields = new HashMap<>();
        addressWithFields.put("id", 1L);
        addressWithFields.put("name", "Home");
        addressWithFields.put("street", "123 Main St");
        addressWithFields.put("city", "Springfield");
        addressWithFields.put("province", "IL");
        addressWithFields.put("contact_id", 100L);
        addressWithFields.put("postal_code", "62701");
        addressWithFields.put("country", "US");
        addressWithFields.put("latitude", 39.7817);
        addressWithFields.put("longitude", -89.6501);
        addressWithFields.put("created_at", "2024-01-15T10:00:00Z");
        addressWithFields.put("updated_at", "2024-01-15T11:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(addressWithFields));

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify field mapping
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("62701", data.get(0).get("postalCode"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T11:00:00Z", data.get(0).get("updatedAt"));
        // Pass-through fields
        assertEquals("Home", data.get(0).get("name"));
        assertEquals("123 Main St", data.get(0).get("street"));
        assertEquals("Springfield", data.get(0).get("city"));
        assertEquals("IL", data.get(0).get("province"));
        assertEquals("US", data.get(0).get("country"));
        assertEquals(39.7817, data.get(0).get("latitude"));
        assertEquals(-89.6501, data.get(0).get("longitude"));
    }

    @Test
    void listAddresses_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> addresses = List.of(
            addressBuilder().id(1L).name("Home").build()
        );
        Map<String, Object> responseWithoutMeta = new HashMap<>();
        responseWithoutMeta.put("data", addresses);
        // No meta field

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(responseWithoutMeta));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

        // Then
        assertNotNull(result);
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listAddresses_MultipleAddresses_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> address1 = new HashMap<>();
        address1.put("id", 1L);
        address1.put("name", "Home");
        address1.put("contact_id", 100L);
        address1.put("postal_code", "12345");
        address1.put("country", "US");

        Map<String, Object> address2 = new HashMap<>();
        address2.put("id", 2L);
        address2.put("name", "Work");
        address2.put("contact_id", 101L);
        address2.put("postal_code", "67890");
        address2.put("country", "CA");

        Map<String, Object> address3 = new HashMap<>();
        address3.put("id", 3L);
        address3.put("name", "Vacation");
        address3.put("contact_id", 102L);
        address3.put("postal_code", "ABCDE");
        address3.put("country", "UK");

        Map<String, Object> listResponse = createListResponse(List.of(address1, address2, address3), 1, 10, 3);

        when(monicaClient.get(eq("/addresses"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = addressService.listAddresses(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("Home", data.get(0).get("name"));
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals("12345", data.get(0).get("postalCode"));
        assertEquals("US", data.get(0).get("country"));

        assertEquals("Work", data.get(1).get("name"));
        assertEquals(101L, data.get(1).get("contactId"));
        assertEquals("67890", data.get(1).get("postalCode"));
        assertEquals("CA", data.get(1).get("country"));

        assertEquals("Vacation", data.get(2).get("name"));
        assertEquals(102L, data.get(2).get("contactId"));
        assertEquals("ABCDE", data.get(2).get("postalCode"));
        assertEquals("UK", data.get(2).get("country"));
    }

    // ========================================================================================
    // COUNTRY LINKING TESTS
    // ========================================================================================

    @Test
    void createAddress_WithCountryCode_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("country", "US");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "US".equals(data.get("country"))
        ));
    }

    @Test
    void createAddress_WithFullCountryName_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("country", "United States");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "United States".equals(data.get("country"))
        ));
    }

    @Test
    void createAddress_InternationalCountries_MapCorrectly() {
        // Test various international country codes
        String[] countries = {"US", "CA", "UK", "DE", "FR", "JP", "AU", "BR"};

        for (String country : countries) {
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 100L);
            arguments.put("country", country);

            when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

            // When
            Map<String, Object> result = addressService.createAddress(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/addresses"), argThat(data ->
                country.equals(data.get("country"))
            ));

            reset(monicaClient, contentFormatter);
        }
    }

    @Test
    void updateAddress_ChangeCountry_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("country", "CA");

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            "CA".equals(data.get("country"))
        ));
    }

    // ========================================================================================
    // GEOLOCATION TESTS
    // ========================================================================================

    @Test
    void createAddress_WithLatLong_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", 40.7128);
        arguments.put("longitude", -74.0060);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            Double.valueOf(40.7128).equals(data.get("latitude")) &&
            Double.valueOf(-74.0060).equals(data.get("longitude"))
        ));
    }

    @Test
    void createAddress_NegativeCoordinates_MapCorrectly() {
        // Given - Southern hemisphere and western hemisphere coordinates
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", -33.8688);  // Sydney, Australia
        arguments.put("longitude", 151.2093);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            Double.valueOf(-33.8688).equals(data.get("latitude")) &&
            Double.valueOf(151.2093).equals(data.get("longitude"))
        ));
    }

    @Test
    void createAddress_ZeroCoordinates_MapCorrectly() {
        // Given - Coordinates at origin (Atlantic Ocean)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", 0.0);
        arguments.put("longitude", 0.0);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            Double.valueOf(0.0).equals(data.get("latitude")) &&
            Double.valueOf(0.0).equals(data.get("longitude"))
        ));
    }

    @Test
    void createAddress_ExtremeBoundaryCoordinates_MapCorrectly() {
        // Given - Extreme valid coordinates
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", 89.9999);  // Near North Pole
        arguments.put("longitude", 179.9999);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            Double.valueOf(89.9999).equals(data.get("latitude")) &&
            Double.valueOf(179.9999).equals(data.get("longitude"))
        ));
    }

    @Test
    void createAddress_IntegerCoordinates_MapCorrectly() {
        // Given - Integer coordinates (should be accepted)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", 40);
        arguments.put("longitude", -74);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("latitude") != null &&
            data.get("longitude") != null
        ));
    }

    @Test
    void createAddress_StringCoordinates_MapCorrectly() {
        // Given - String coordinates
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", "40.7128");
        arguments.put("longitude", "-74.0060");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "40.7128".equals(data.get("latitude").toString()) &&
            "-74.0060".equals(data.get("longitude").toString())
        ));
    }

    // ========================================================================================
    // EDGE CASE TESTS
    // ========================================================================================

    @Test
    void createAddress_SpecialCharactersInName_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("name", "Mom & Dad's House (Primary)");

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            "Mom & Dad's House (Primary)".equals(data.get("name"))
        ));
    }

    @Test
    void createAddress_UnicodeCharactersInAddress_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("name", "\u4E2D\u6587\u5730\u5740");  // Chinese characters
        arguments.put("street", "\u00C9lys\u00E9es");  // French accents

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/addresses"), any());
    }

    @Test
    void createAddress_LongStreetName_Succeeds() {
        // Given
        String longStreet = "A".repeat(200) + " Street";
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("street", longStreet);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            longStreet.equals(data.get("street"))
        ));
    }

    @Test
    void getAddress_LargeId_Succeeds() {
        // Given
        Map<String, Object> arguments = Map.of("id", Long.MAX_VALUE);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            addressBuilder().id(Long.MAX_VALUE).build()
        );

        when(monicaClient.get(eq("/addresses/" + Long.MAX_VALUE), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.getAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/addresses/" + Long.MAX_VALUE), any());
    }

    @Test
    void createAddress_EmptyOptionalFields_Succeeds() {
        // Given - only required contactId
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            !data.containsKey("name") &&
            !data.containsKey("street") &&
            !data.containsKey("city")
        ));
    }

    @Test
    void createAddress_InternationalPostalCodes_MapCorrectly() {
        // Test various international postal code formats
        String[] postalCodes = {
            "12345",       // US
            "12345-6789",  // US extended
            "M5V 2T6",     // Canada
            "SW1A 1AA",    // UK
            "100-0001",    // Japan
            "75001",       // France
            "1050",        // Austria
        };

        for (String postalCode : postalCodes) {
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("contactId", 100L);
            arguments.put("postalCode", postalCode);

            when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

            // When
            Map<String, Object> result = addressService.createAddress(arguments).block();

            // Then
            assertNotNull(result);
            verify(monicaClient).post(eq("/addresses"), argThat(data ->
                postalCode.equals(data.get("postal_code"))
            ));

            reset(monicaClient, contentFormatter);
        }
    }

    @Test
    void createAddress_HighPrecisionCoordinates_MapCorrectly() {
        // Given - High precision coordinates
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);
        arguments.put("latitude", 40.712776123456789);
        arguments.put("longitude", -74.005974123456789);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.createAddress(arguments).block();

        // Then
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("latitude") != null &&
            data.get("longitude") != null
        ));
    }

    @Test
    void updateAddress_RemoveContactId_NotSent() {
        // Given - update without contactId should not include it
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("name", "Updated Name");
        // Not including contactId intentionally

        when(monicaClient.put(eq("/addresses/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        addressService.updateAddress(arguments).block();

        // Then
        verify(monicaClient).put(eq("/addresses/1"), argThat(data ->
            !data.containsKey("contact_id") &&
            "Updated Name".equals(data.get("name"))
        ));
    }

    @Test
    void createAddress_OnlyContactId_Succeeds() {
        // Given - minimum valid request
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/addresses"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = addressService.createAddress(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/addresses"), argThat(data ->
            data.get("contact_id").equals(100L) &&
            data.size() == 1  // Only contact_id should be present
        ));
    }
}
