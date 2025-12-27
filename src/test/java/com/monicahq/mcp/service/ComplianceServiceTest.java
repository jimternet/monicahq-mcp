package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.config.ComplianceFieldMappingConfig;
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
 * Unit tests for ComplianceService covering CRUD operations,
 * compliance and data export, field mapping, validation, and edge cases.
 */
@ExtendWith(MockitoExtension.class)
class ComplianceServiceTest extends ServiceTestBase {

    @Mock
    private MonicaHqClient monicaClient;

    @Mock
    private ContentFormatter contentFormatter;

    private ComplianceService complianceService;

    private Map<String, Object> mockComplianceData;
    private Map<String, Object> mockApiResponse;

    @BeforeEach
    void setUp() {
        ComplianceFieldMappingConfig config = new ComplianceFieldMappingConfig();
        complianceService = new ComplianceService(monicaClient, contentFormatter, config);

        mockComplianceData = createComplianceData(1L, "gdpr");
        mockApiResponse = createSingleEntityResponse(mockComplianceData);
    }

    // Helper method to create compliance data
    private Map<String, Object> createComplianceData(Long id, String type) {
        Map<String, Object> data = new HashMap<>();
        data.put("id", id);
        data.put("type", type);
        data.put("is_active", true);
        data.put("contact_id", 100L);
        data.put("consent_given", true);
        data.put("consent_date", "2024-01-15");
        data.put("data_retention_days", 365);
        data.put("privacy_level", "high");
        data.put("audit_required", false);
        data.put("created_at", "2024-01-15T10:00:00Z");
        data.put("updated_at", "2024-01-15T10:00:00Z");
        return data;
    }

    // ========================================================================================
    // CREATE COMPLIANCE TESTS
    // ========================================================================================

    @Test
    void createCompliance_ValidArgs_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("contactId", 100L);
        arguments.put("isActive", true);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted compliance JSON", content.get(0).get("text"));

        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "gdpr".equals(data.get("type")) &&
            Long.valueOf(100L).equals(data.get("contact_id")) &&
            Boolean.TRUE.equals(data.get("is_active"))
        ));
    }

    @Test
    void createCompliance_MissingType_ThrowsException() {
        // Given - has contactId but no type
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            complianceService.createCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("type is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_NullType_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", null);
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            complianceService.createCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("type is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_EmptyType_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "");
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            complianceService.createCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("type is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_WhitespaceType_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "   ");
        arguments.put("contactId", 100L);

        // When & Then
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            complianceService.createCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("type is required"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_EmptyArgs_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.createCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_NullArgs_ThrowsException() {
        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.createCompliance(null).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void createCompliance_FieldMapping_ContactIdToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("contactId", 200L);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify contactId mapped to contact_id
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Long.valueOf(200L).equals(data.get("contact_id")) &&
            !data.containsKey("contactId")
        ));
    }

    @Test
    void createCompliance_FieldMapping_IsActiveToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "privacy");
        arguments.put("isActive", false);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify isActive mapped to is_active
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Boolean.FALSE.equals(data.get("is_active")) &&
            !data.containsKey("isActive")
        ));
    }

    @Test
    void createCompliance_FieldMapping_DataRetentionDaysToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("dataRetentionDays", 730);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify dataRetentionDays mapped to data_retention_days
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Integer.valueOf(730).equals(data.get("data_retention_days")) &&
            !data.containsKey("dataRetentionDays")
        ));
    }

    @Test
    void createCompliance_FieldMapping_PrivacyLevelToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "privacy");
        arguments.put("privacyLevel", "maximum");

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify privacyLevel mapped to privacy_level
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "maximum".equals(data.get("privacy_level")) &&
            !data.containsKey("privacyLevel")
        ));
    }

    @Test
    void createCompliance_FieldMapping_ConsentGivenToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("consentGiven", true);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify consentGiven mapped to consent_given
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Boolean.TRUE.equals(data.get("consent_given")) &&
            !data.containsKey("consentGiven")
        ));
    }

    @Test
    void createCompliance_FieldMapping_ConsentDateToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("consentDate", "2024-06-15");

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify consentDate mapped to consent_date
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "2024-06-15".equals(data.get("consent_date")) &&
            !data.containsKey("consentDate")
        ));
    }

    @Test
    void createCompliance_FieldMapping_AuditRequiredToSnakeCase() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "audit");
        arguments.put("auditRequired", true);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify auditRequired mapped to audit_required
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Boolean.TRUE.equals(data.get("audit_required")) &&
            !data.containsKey("auditRequired")
        ));
    }

    @Test
    void createCompliance_WithAllFields_MapsAllFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "full_compliance");
        arguments.put("contactId", 300L);
        arguments.put("isActive", true);
        arguments.put("dataRetentionDays", 365);
        arguments.put("privacyLevel", "high");
        arguments.put("consentGiven", true);
        arguments.put("consentDate", "2024-01-01");
        arguments.put("auditRequired", false);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "full_compliance".equals(data.get("type")) &&
            Long.valueOf(300L).equals(data.get("contact_id")) &&
            Boolean.TRUE.equals(data.get("is_active")) &&
            Integer.valueOf(365).equals(data.get("data_retention_days")) &&
            "high".equals(data.get("privacy_level")) &&
            Boolean.TRUE.equals(data.get("consent_given")) &&
            "2024-01-01".equals(data.get("consent_date")) &&
            Boolean.FALSE.equals(data.get("audit_required"))
        ));
    }

    @Test
    void createCompliance_StringContactId_PassedDirectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("contactId", "150");

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - verify contactId is passed as-is to API
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "150".equals(data.get("contact_id").toString())
        ));
    }

    @Test
    void createCompliance_IntegerContactId_PassedDirectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr");
        arguments.put("contactId", 250);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Integer.valueOf(250).equals(data.get("contact_id"))
        ));
    }

    // ========================================================================================
    // GET COMPLIANCE TESTS
    // ========================================================================================

    @Test
    void getCompliance_ValidId_ReturnsFormattedResponse() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        when(monicaClient.get(eq("/compliance/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");
        assertNotNull(data);

        verify(monicaClient).get(eq("/compliance/1"), any());
    }

    @Test
    void getCompliance_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "42");

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createComplianceData(42L, "privacy")
        );

        when(monicaClient.get(eq("/compliance/42"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/compliance/42"), any());
    }

    @Test
    void getCompliance_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 123);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createComplianceData(123L, "audit")
        );

        when(monicaClient.get(eq("/compliance/123"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/compliance/123"), any());
    }

    @Test
    void getCompliance_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("type", "gdpr");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.getCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompliance_NullArgs_ThrowsException() {
        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.getCompliance(null).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompliance_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "not-a-number");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.getCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void getCompliance_MapsResponseFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        Map<String, Object> apiData = new HashMap<>();
        apiData.put("id", 1L);
        apiData.put("type", "gdpr");
        apiData.put("contact_id", 100L);
        apiData.put("is_active", true);
        apiData.put("data_retention_days", 365);
        apiData.put("privacy_level", "high");
        apiData.put("consent_given", true);
        apiData.put("consent_date", "2024-01-15");
        apiData.put("audit_required", false);
        apiData.put("created_at", "2024-01-15T10:00:00Z");
        apiData.put("updated_at", "2024-01-15T09:00:00Z");
        Map<String, Object> response = createSingleEntityResponse(apiData);

        when(monicaClient.get(eq("/compliance/1"), any())).thenReturn(Mono.just(response));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        // Verify field mapping from snake_case to camelCase
        assertEquals(100L, data.get("contactId"));
        assertEquals(true, data.get("isActive"));
        assertEquals(365, data.get("dataRetentionDays"));
        assertEquals("high", data.get("privacyLevel"));
        assertEquals(true, data.get("consentGiven"));
        assertEquals("2024-01-15", data.get("consentDate"));
        assertEquals(false, data.get("auditRequired"));
        assertEquals("2024-01-15T10:00:00Z", data.get("createdAt"));
        assertEquals("2024-01-15T09:00:00Z", data.get("updatedAt"));
        // These should remain unchanged
        assertEquals(1L, data.get("id"));
        assertEquals("gdpr", data.get("type"));
    }

    @Test
    void getCompliance_DirectResponseWithoutDataWrapper_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);

        // API response without "data" wrapper
        Map<String, Object> directResponse = new HashMap<>();
        directResponse.put("id", 1L);
        directResponse.put("type", "direct");
        directResponse.put("contact_id", 50L);
        directResponse.put("is_active", false);
        directResponse.put("created_at", "2024-01-20T10:00:00Z");
        directResponse.put("updated_at", "2024-01-20T10:00:00Z");

        when(monicaClient.get(eq("/compliance/1"), any())).thenReturn(Mono.just(directResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        Map<String, Object> data = (Map<String, Object>) result.get("data");

        assertEquals("direct", data.get("type"));
        assertEquals(50L, data.get("contactId"));
        assertEquals(false, data.get("isActive"));
    }

    @Test
    void getCompliance_LargeId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 999999999L);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createComplianceData(999999999L, "large_id")
        );

        when(monicaClient.get(eq("/compliance/999999999"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/compliance/999999999"), any());
    }

    // ========================================================================================
    // UPDATE COMPLIANCE TESTS
    // ========================================================================================

    @Test
    void updateCompliance_ValidArgs_CallsCorrectEndpoint() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("isActive", false);

        when(monicaClient.put(eq("/compliance/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.updateCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        verify(monicaClient).put(eq("/compliance/1"), argThat(data ->
            Boolean.FALSE.equals(data.get("is_active"))
        ));
    }

    @Test
    void updateCompliance_RemovesIdFromUpdateData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 5L);
        arguments.put("type", "updated_type");

        when(monicaClient.put(eq("/compliance/5"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        complianceService.updateCompliance(arguments).block();

        // Then - verify that id is NOT included in the request body
        verify(monicaClient).put(eq("/compliance/5"), argThat(data ->
            !data.containsKey("id")
        ));
    }

    @Test
    void updateCompliance_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("type", "updated");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.updateCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompliance_EmptyArgsOnlyId_Succeeds() {
        // Given - just id, no other fields
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);

        when(monicaClient.put(eq("/compliance/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.updateCompliance(arguments).block();

        // Then - should succeed with just id
        assertNotNull(result);
    }

    @Test
    void updateCompliance_NullArgs_ThrowsException() {
        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.updateCompliance(null).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompliance_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "42");
        arguments.put("type", "updated");

        when(monicaClient.put(eq("/compliance/42"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        complianceService.updateCompliance(arguments).block();

        // Then
        verify(monicaClient).put(eq("/compliance/42"), any());
    }

    @Test
    void updateCompliance_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 99);
        arguments.put("type", "updated");

        when(monicaClient.put(eq("/compliance/99"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        complianceService.updateCompliance(arguments).block();

        // Then
        verify(monicaClient).put(eq("/compliance/99"), any());
    }

    @Test
    void updateCompliance_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", "invalid");
        arguments.put("type", "updated");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.updateCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void updateCompliance_FieldMapping_AllFields() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("id", 1L);
        arguments.put("contactId", 200L);
        arguments.put("isActive", false);
        arguments.put("dataRetentionDays", 180);
        arguments.put("privacyLevel", "medium");
        arguments.put("consentGiven", false);
        arguments.put("consentDate", "2024-12-01");
        arguments.put("auditRequired", true);

        when(monicaClient.put(eq("/compliance/1"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.updateCompliance(arguments).block();

        // Then
        verify(monicaClient).put(eq("/compliance/1"), argThat(data ->
            Long.valueOf(200L).equals(data.get("contact_id")) &&
            Boolean.FALSE.equals(data.get("is_active")) &&
            Integer.valueOf(180).equals(data.get("data_retention_days")) &&
            "medium".equals(data.get("privacy_level")) &&
            Boolean.FALSE.equals(data.get("consent_given")) &&
            "2024-12-01".equals(data.get("consent_date")) &&
            Boolean.TRUE.equals(data.get("audit_required")) &&
            !data.containsKey("id")
        ));
    }

    // ========================================================================================
    // DELETE COMPLIANCE TESTS
    // ========================================================================================

    @Test
    void deleteCompliance_ValidId_ReturnsSuccessMessage() {
        // Given
        Map<String, Object> arguments = Map.of("id", 1L);
        Map<String, Object> deleteResponse = createDeleteResponse(1L);

        when(monicaClient.delete(eq("/compliance/1"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Compliance"), eq(1L), eq(true), anyString()
        )).thenReturn("Compliance record with ID 1 has been deleted successfully");

        // When
        Map<String, Object> result = complianceService.deleteCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertTrue(content.get(0).get("text").toString().contains("deleted successfully"));

        verify(monicaClient).delete(eq("/compliance/1"));
    }

    @Test
    void deleteCompliance_StringId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", "99");
        Map<String, Object> deleteResponse = createDeleteResponse(99L);

        when(monicaClient.delete(eq("/compliance/99"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Compliance"), eq(99L), eq(true), anyString()
        )).thenReturn("Compliance record with ID 99 has been deleted successfully");

        // When
        Map<String, Object> result = complianceService.deleteCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/compliance/99"));
    }

    @Test
    void deleteCompliance_IntegerId_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("id", 55);
        Map<String, Object> deleteResponse = createDeleteResponse(55L);

        when(monicaClient.delete(eq("/compliance/55"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Compliance"), eq(55L), eq(true), anyString()
        )).thenReturn("Compliance record with ID 55 has been deleted successfully");

        // When
        Map<String, Object> result = complianceService.deleteCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).delete(eq("/compliance/55"));
    }

    @Test
    void deleteCompliance_MissingId_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("type", "gdpr");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.deleteCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCompliance_NullArgs_ThrowsException() {
        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.deleteCompliance(null).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCompliance_InvalidIdFormat_ThrowsException() {
        // Given
        Map<String, Object> arguments = Map.of("id", "invalid");

        // When & Then - ComplianceService wraps base class errors in IllegalStateException
        IllegalStateException exception = assertThrows(IllegalStateException.class, () -> {
            complianceService.deleteCompliance(arguments).block();
        });
        assertTrue(exception.getMessage().contains("Compliance API is not available"));
        verifyNoInteractions(monicaClient);
    }

    @Test
    void deleteCompliance_MessageContainsId() {
        // Given
        Map<String, Object> arguments = Map.of("id", 42L);
        Map<String, Object> deleteResponse = createDeleteResponse(42L);

        when(monicaClient.delete(eq("/compliance/42"))).thenReturn(Mono.just(deleteResponse));
        when(contentFormatter.formatOperationResult(
            eq("Delete"), eq("Compliance"), eq(42L), eq(true), anyString()
        )).thenReturn("Compliance record with ID 42 has been deleted successfully");

        // When
        Map<String, Object> result = complianceService.deleteCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertTrue(content.get(0).get("text").toString().contains("42"));
    }

    // ========================================================================================
    // LIST COMPLIANCE TESTS
    // ========================================================================================

    @Test
    void listCompliance_ReturnsFormattedList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr"),
            createComplianceData(2L, "privacy")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertTrue(result.containsKey("content"));

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(2, data.size());

        verify(monicaClient).get(eq("/compliance"), any());
    }

    @Test
    void listCompliance_WithPagination_PassesCorrectParameters() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 2,
            "limit", 20
        );

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords, 2, 20, 50);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "2".equals(params.get("page")) &&
            "20".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_DefaultPagination_UsesCorrectDefaults() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then - verify default pagination values (page=1, limit=10)
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "1".equals(params.get("page")) &&
            "10".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_LimitClamping_MaxValue() {
        // Given - limit over 100 should be clamped to 100
        Map<String, Object> arguments = Map.of("limit", 200);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "100".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_LimitClamping_MinValue() {
        // Given - limit below 1 should be clamped to 1
        Map<String, Object> arguments = Map.of("limit", 0);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_LimitClamping_NegativeValue() {
        // Given - negative limit should be clamped to 1
        Map<String, Object> arguments = Map.of("limit", -5);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "1".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_WithContactIdFilter_MapsToContactIdParam() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", 100L);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then - verify contactId mapped to contact_id query param
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "100".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listCompliance_StringContactId_PassedCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("contactId", "150");

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "150".equals(params.get("contact_id"))
        ));
    }

    @Test
    void listCompliance_NullContactId_NotIncludedInParams() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("contactId", null);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then - null contactId should not be included
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            !params.containsKey("contact_id")
        ));
    }

    @Test
    void listCompliance_ReturnsMetadata() {
        // Given
        Map<String, Object> arguments = Map.of("page", 1, "limit", 10);

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords, 1, 10, 100);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

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
    void listCompliance_EmptyResults_ReturnsEmptyList() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> emptyResponse = createListResponse(List.of(), 1, 10, 0);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(emptyResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("[]");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertTrue(data.isEmpty());
    }

    @Test
    void listCompliance_StringLimit_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("limit", "25");

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "25".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_StringPage_ParsesCorrectly() {
        // Given
        Map<String, Object> arguments = Map.of("page", "3");

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords, 3, 10, 30);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "3".equals(params.get("page"))
        ));
    }

    @Test
    void listCompliance_MapsFieldsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        Map<String, Object> complianceWithSnakeCase = new HashMap<>();
        complianceWithSnakeCase.put("id", 1L);
        complianceWithSnakeCase.put("type", "gdpr");
        complianceWithSnakeCase.put("contact_id", 100L);
        complianceWithSnakeCase.put("is_active", true);
        complianceWithSnakeCase.put("data_retention_days", 365);
        complianceWithSnakeCase.put("privacy_level", "high");
        complianceWithSnakeCase.put("consent_given", true);
        complianceWithSnakeCase.put("consent_date", "2024-01-15");
        complianceWithSnakeCase.put("audit_required", false);
        complianceWithSnakeCase.put("created_at", "2024-01-15T10:00:00Z");
        complianceWithSnakeCase.put("updated_at", "2024-01-15T10:00:00Z");

        Map<String, Object> listResponse = createListResponse(List.of(complianceWithSnakeCase));

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(1, data.size());

        // Verify snake_case is mapped to camelCase
        assertEquals(100L, data.get(0).get("contactId"));
        assertEquals(true, data.get(0).get("isActive"));
        assertEquals(365, data.get(0).get("dataRetentionDays"));
        assertEquals("high", data.get(0).get("privacyLevel"));
        assertEquals(true, data.get(0).get("consentGiven"));
        assertEquals("2024-01-15", data.get(0).get("consentDate"));
        assertEquals(false, data.get(0).get("auditRequired"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("createdAt"));
        assertEquals("2024-01-15T10:00:00Z", data.get(0).get("updatedAt"));
        // These should remain unchanged
        assertEquals("gdpr", data.get(0).get("type"));
    }

    @Test
    void listCompliance_IntegerPageAndLimit_ConvertsToString() {
        // Given
        Map<String, Object> arguments = Map.of(
            "page", 5,
            "limit", 50
        );

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords, 5, 50, 100);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(monicaClient).get(eq("/compliance"), argThat(params ->
            "5".equals(params.get("page")) &&
            "50".equals(params.get("limit"))
        ));
    }

    @Test
    void listCompliance_NoMetaInResponse_HandlesGracefully() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        // Response without meta
        Map<String, Object> listResponse = new HashMap<>();
        listResponse.put("data", complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        assertTrue(result.containsKey("data"));
        assertFalse(result.containsKey("meta"));
    }

    @Test
    void listCompliance_MultipleRecords_MapsAllCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr"),
            createComplianceData(2L, "privacy"),
            createComplianceData(3L, "audit")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords, 1, 10, 3);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> data = (List<Map<String, Object>>) result.get("data");
        assertEquals(3, data.size());

        assertEquals("gdpr", data.get(0).get("type"));
        assertEquals("privacy", data.get(1).get("type"));
        assertEquals("audit", data.get(2).get("type"));
    }

    @Test
    void listCompliance_ContentFieldHasCorrectFormat() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        Map<String, Object> result = complianceService.listCompliance(arguments).block();

        // Then
        assertNotNull(result);
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> content = (List<Map<String, Object>>) result.get("content");
        assertEquals(1, content.size());
        assertEquals("text", content.get(0).get("type"));
        assertEquals("Formatted list JSON", content.get(0).get("text"));
    }

    // ========================================================================================
    // DATA EXPORT & COMPLIANCE STATUS TESTS
    // ========================================================================================

    @Test
    void createCompliance_DataExportType_Succeeds() {
        // Given - test data export request
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "data_export");
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "data_export".equals(data.get("type")) &&
            Long.valueOf(100L).equals(data.get("contact_id"))
        ));
    }

    @Test
    void createCompliance_GdprRequestType_Succeeds() {
        // Given - GDPR data request
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr_request");
        arguments.put("contactId", 200L);
        arguments.put("consentGiven", true);
        arguments.put("consentDate", "2024-06-01");

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "gdpr_request".equals(data.get("type")) &&
            Long.valueOf(200L).equals(data.get("contact_id")) &&
            Boolean.TRUE.equals(data.get("consent_given")) &&
            "2024-06-01".equals(data.get("consent_date"))
        ));
    }

    @Test
    void createCompliance_DataRetentionPolicy_Succeeds() {
        // Given - data retention policy
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "retention_policy");
        arguments.put("dataRetentionDays", 90);
        arguments.put("auditRequired", true);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "retention_policy".equals(data.get("type")) &&
            Integer.valueOf(90).equals(data.get("data_retention_days")) &&
            Boolean.TRUE.equals(data.get("audit_required"))
        ));
    }

    // ========================================================================================
    // EDGE CASES
    // ========================================================================================

    @Test
    void createCompliance_SpecialCharactersInType_Succeeds() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "gdpr_2024-v1");
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "gdpr_2024-v1".equals(data.get("type"))
        ));
    }

    @Test
    void createCompliance_LongDataRetentionDays_Succeeds() {
        // Given - large retention period (10 years)
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "long_retention");
        arguments.put("dataRetentionDays", 3650);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Integer.valueOf(3650).equals(data.get("data_retention_days"))
        ));
    }

    @Test
    void createCompliance_ZeroDataRetentionDays_Succeeds() {
        // Given - immediate deletion policy
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "immediate_delete");
        arguments.put("dataRetentionDays", 0);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        Map<String, Object> result = complianceService.createCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Integer.valueOf(0).equals(data.get("data_retention_days"))
        ));
    }

    @Test
    void getCompliance_WithZeroId_ParsesCorrectly() {
        // Given - Edge case: id = 0
        Map<String, Object> arguments = Map.of("id", 0);

        Map<String, Object> mockResponse = createSingleEntityResponse(
            createComplianceData(0L, "zero_id")
        );

        when(monicaClient.get(eq("/compliance/0"), any())).thenReturn(Mono.just(mockResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted compliance JSON");

        // When
        Map<String, Object> result = complianceService.getCompliance(arguments).block();

        // Then
        assertNotNull(result);
        verify(monicaClient).get(eq("/compliance/0"), any());
    }

    @Test
    void createCompliance_MultipleBooleanFields_MapsCorrectly() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "mixed_booleans");
        arguments.put("isActive", true);
        arguments.put("consentGiven", false);
        arguments.put("auditRequired", true);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            Boolean.TRUE.equals(data.get("is_active")) &&
            Boolean.FALSE.equals(data.get("consent_given")) &&
            Boolean.TRUE.equals(data.get("audit_required"))
        ));
    }

    @Test
    void createCompliance_PrivacyLevelValues_Succeeds() {
        // Test various privacy levels
        String[] privacyLevels = {"low", "medium", "high", "maximum"};

        for (String level : privacyLevels) {
            Map<String, Object> arguments = new HashMap<>();
            arguments.put("type", "privacy_test");
            arguments.put("privacyLevel", level);

            when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
            when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

            // When
            Map<String, Object> result = complianceService.createCompliance(arguments).block();

            // Then
            assertNotNull(result);
        }
    }

    @Test
    void listCompliance_FormatterCalledWithListData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();

        List<Map<String, Object>> complianceRecords = List.of(
            createComplianceData(1L, "gdpr")
        );
        Map<String, Object> listResponse = createListResponse(complianceRecords);

        when(monicaClient.get(eq("/compliance"), any())).thenReturn(Mono.just(listResponse));
        when(contentFormatter.formatListAsEscapedJson(any())).thenReturn("Formatted list JSON");

        // When
        complianceService.listCompliance(arguments).block();

        // Then
        verify(contentFormatter).formatListAsEscapedJson(any());
    }

    @Test
    void createCompliance_FormatterCalledWithRawData() {
        // Given
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "test");
        arguments.put("contactId", 100L);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then
        verify(contentFormatter).formatAsEscapedJson(any());
    }

    @Test
    void createCompliance_UnknownFieldsPassThrough() {
        // Given - fields that aren't explicitly mapped should pass through
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("type", "custom");
        arguments.put("customField", "customValue");
        arguments.put("anotherField", 123);

        when(monicaClient.post(eq("/compliance"), any())).thenReturn(Mono.just(mockApiResponse));
        when(contentFormatter.formatAsEscapedJson(any())).thenReturn("Formatted JSON");

        // When
        complianceService.createCompliance(arguments).block();

        // Then - unknown fields should be passed through unchanged
        verify(monicaClient).post(eq("/compliance"), argThat(data ->
            "custom".equals(data.get("type")) &&
            "customValue".equals(data.get("customField")) &&
            Integer.valueOf(123).equals(data.get("anotherField"))
        ));
    }
}
