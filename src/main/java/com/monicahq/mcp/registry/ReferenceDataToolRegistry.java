package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.AuditLogService;
import com.monicahq.mcp.service.ContactFieldTypeService;
import com.monicahq.mcp.service.CountryService;
import com.monicahq.mcp.service.CurrencyService;
import com.monicahq.mcp.service.GenderService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Domain-specific tool registry for Reference Data and Discovery operations.
 *
 * This registry handles reference data and discovery MCP tools:
 * - Audit Log operations (get, list, search)
 * - Country operations (get, list, search)
 * - Currency operations (get, list, search)
 * - Gender management (create, get, update, delete, list) - Gap Analysis Phase 1
 * - Contact Field Type management (create, get, update, delete, list) - Gap Analysis Phase 2
 *
 * Reference data tools provide access to system lookup values and custom definitions.
 * Discovery tools help users find valid IDs for use in other operations.
 *
 * The registry delegates execution to:
 * - AuditLogService for audit log operations
 * - CountryService for country operations
 * - CurrencyService for currency operations
 * - GenderService for gender management (CRUD enabled in Phase 1)
 * - ContactFieldTypeService for contact field type management (CRUD enabled in Phase 2)
 */
@Component
@Slf4j
public class ReferenceDataToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "ReferenceData";
    private static final String SYSTEM_CATEGORY = "System & Reference";
    private static final String DISCOVERY_CATEGORY = "Discovery & Reference";

    private final AuditLogService auditLogService;
    private final CountryService countryService;
    private final CurrencyService currencyService;
    private final GenderService genderService;
    private final ContactFieldTypeService contactFieldTypeService;

    public ReferenceDataToolRegistry(
            AuditLogService auditLogService,
            CountryService countryService,
            CurrencyService currencyService,
            GenderService genderService,
            ContactFieldTypeService contactFieldTypeService) {
        this.auditLogService = auditLogService;
        this.countryService = countryService;
        this.currencyService = currencyService;
        this.genderService = genderService;
        this.contactFieldTypeService = contactFieldTypeService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // === AUDIT LOG MANAGEMENT (3 operations) ===
        registerTool(
            "auditlog_get",
            "[Audit Log] Get an audit log by ID",
            createIdSchema("Audit Log ID"),
            SYSTEM_CATEGORY
        );

        registerTool(
            "auditlog_list",
            "[Audit Log] List audit logs with pagination",
            createListSchema(),
            SYSTEM_CATEGORY
        );

        registerTool(
            "auditlog_search",
            "[Audit Log] Search audit logs by criteria",
            createAuditLogSearchSchema(),
            SYSTEM_CATEGORY
        );

        // === COUNTRY REFERENCE DATA (3 operations) ===
        registerTool(
            "country_get",
            "[Country] Get a country by ID",
            createIdSchema("Country ID"),
            SYSTEM_CATEGORY
        );

        registerTool(
            "country_list",
            "[Country] List all countries",
            createListSchema(),
            SYSTEM_CATEGORY
        );

        registerTool(
            "country_search",
            "[Country] Search countries by name",
            createCountrySearchSchema(),
            SYSTEM_CATEGORY
        );

        // === CURRENCY REFERENCE DATA (3 operations) ===
        registerTool(
            "currency_get",
            "[Currency] Get a currency by ID",
            createIdSchema("Currency ID"),
            SYSTEM_CATEGORY
        );

        registerTool(
            "currency_list",
            "[Currency] List all currencies",
            createListSchema(),
            SYSTEM_CATEGORY
        );

        registerTool(
            "currency_search",
            "[Currency] Search currencies by code or name",
            createCurrencySearchSchema(),
            SYSTEM_CATEGORY
        );

        // === GENDER MANAGEMENT (5 operations) === Gap Analysis Phase 1
        registerTool(
            "gender_create",
            "[Gender] Create a new custom gender type",
            createGenderCreateSchema(),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "gender_get",
            "[Gender] Get a gender by ID",
            createIdSchema("Gender ID"),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "gender_update",
            "[Gender] Update an existing gender",
            createGenderUpdateSchema(),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "gender_delete",
            "[Gender] Delete a gender type",
            createIdSchema("Gender ID"),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "gender_list",
            "[Gender] List all available genders",
            createListSchema(),
            DISCOVERY_CATEGORY
        );

        // === CONTACT FIELD TYPE MANAGEMENT (5 operations) === Gap Analysis Phase 2
        registerTool(
            "contact_field_type_create",
            "[Contact Field Type] Create a new custom field type definition",
            createContactFieldTypeCreateSchema(),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "contact_field_type_get",
            "[Contact Field Type] Get a contact field type by ID",
            createIdSchema("Contact Field Type ID"),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "contact_field_type_update",
            "[Contact Field Type] Update an existing contact field type",
            createContactFieldTypeUpdateSchema(),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "contact_field_type_delete",
            "[Contact Field Type] Delete a contact field type",
            createIdSchema("Contact Field Type ID"),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "contact_field_type_list",
            "[Contact Field Type] List all available contact field types",
            createListSchema(),
            DISCOVERY_CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // Audit Log operations
            case "auditlog_get" -> auditLogService.getAuditLog(arguments);
            case "auditlog_list" -> auditLogService.listAuditLogs(arguments);
            case "auditlog_search" -> auditLogService.searchAuditLogs(arguments);

            // Country operations
            case "country_get" -> countryService.getCountry(arguments);
            case "country_list" -> countryService.listCountries(arguments);
            case "country_search" -> countryService.searchCountries(arguments);

            // Currency operations
            case "currency_get" -> currencyService.getCurrency(arguments);
            case "currency_list" -> currencyService.listCurrencies(arguments);
            case "currency_search" -> currencyService.searchCurrencies(arguments);

            // Gender operations (Gap Analysis Phase 1)
            case "gender_create" -> genderService.createGender(arguments);
            case "gender_get" -> genderService.getGender(arguments);
            case "gender_update" -> genderService.updateGender(arguments);
            case "gender_delete" -> genderService.deleteGender(arguments);
            case "gender_list" -> genderService.listGenders(arguments);

            // Contact Field Type operations (Gap Analysis Phase 2)
            case "contact_field_type_create" -> contactFieldTypeService.createContactFieldType(arguments);
            case "contact_field_type_get" -> contactFieldTypeService.getContactFieldType(arguments);
            case "contact_field_type_update" -> contactFieldTypeService.updateContactFieldType(arguments);
            case "contact_field_type_delete" -> contactFieldTypeService.deleteContactFieldType(arguments);
            case "contact_field_type_list" -> contactFieldTypeService.listContactFieldTypes(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Audit Log Schema Methods ==========

    /**
     * Creates the schema for audit log search operations.
     * Allows filtering by action type, entity type, and user ID.
     */
    private Map<String, Object> createAuditLogSearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "action", Map.of(
                    "type", "string",
                    "description", "Filter by action type (e.g., 'create', 'update', 'delete')"
                ),
                "auditableType", Map.of(
                    "type", "string",
                    "description", "Filter by entity type (e.g., 'Contact', 'Activity')"
                ),
                "userId", Map.of(
                    "type", "integer",
                    "description", "Filter by user ID who performed the action"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 25,
                    "maximum", 100
                )
            ),
            "additionalProperties", false
        );
    }

    // ========== Country Schema Methods ==========

    /**
     * Creates the schema for country search operations.
     * Allows searching by country name with pagination.
     */
    private Map<String, Object> createCountrySearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "search", Map.of(
                    "type", "string",
                    "description", "Search query for country name"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 50,
                    "maximum", 200
                )
            ),
            "additionalProperties", false
        );
    }

    // ========== Currency Schema Methods ==========

    /**
     * Creates the schema for currency search operations.
     * Allows searching by currency code or name with pagination.
     */
    private Map<String, Object> createCurrencySearchSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "search", Map.of(
                    "type", "string",
                    "description", "Search query for currency code or name"
                ),
                "page", Map.of(
                    "type", "integer",
                    "description", "Page number (starting from 1)",
                    "default", 1
                ),
                "limit", Map.of(
                    "type", "integer",
                    "description", "Number of items per page",
                    "default", 50,
                    "maximum", 200
                )
            ),
            "additionalProperties", false
        );
    }

    // ========== Gender Schema Methods (Gap Analysis Phase 1) ==========

    /**
     * Creates the schema for gender creation.
     * Requires a name for the custom gender type.
     */
    private Map<String, Object> createGenderCreateSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Name of the gender (e.g., 'Non-binary', 'Genderqueer', etc.)",
                    "minLength", 1,
                    "maxLength", 255
                )
            ),
            "required", java.util.List.of("name"),
            "additionalProperties", false
        );
    }

    /**
     * Creates the schema for gender updates.
     * Allows updating the gender name.
     */
    private Map<String, Object> createGenderUpdateSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Gender ID (required)"
                ),
                "name", Map.of(
                    "type", "string",
                    "description", "Updated name of the gender",
                    "minLength", 1,
                    "maxLength", 255
                )
            ),
            "required", java.util.List.of("id"),
            "additionalProperties", false
        );
    }

    // ========== Contact Field Type Schema Methods (Gap Analysis Phase 2) ==========

    /**
     * Creates the schema for contact field type creation.
     * Requires name and type, with optional protocol, icon, and delibility settings.
     */
    private Map<String, Object> createContactFieldTypeCreateSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Name of the contact field type (e.g., 'Email', 'Phone', 'LinkedIn')",
                    "minLength", 1,
                    "maxLength", 255
                ),
                "type", Map.of(
                    "type", "string",
                    "description", "Type of the field (determines behavior, e.g., 'email', 'url', 'text')"
                ),
                "protocol", Map.of(
                    "type", "string",
                    "description", "Optional protocol for the field type (e.g., 'mailto:', 'https:')"
                ),
                "fontawesomeIcon", Map.of(
                    "type", "string",
                    "description", "Optional Font Awesome icon class for UI display"
                ),
                "delible", Map.of(
                    "type", "boolean",
                    "description", "Whether users can delete instances of this field type",
                    "default", true
                )
            ),
            "required", java.util.List.of("name", "type"),
            "additionalProperties", false
        );
    }

    /**
     * Creates the schema for contact field type updates.
     * Allows updating any field type properties.
     */
    private Map<String, Object> createContactFieldTypeUpdateSchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "id", Map.of(
                    "type", "integer",
                    "description", "Contact Field Type ID (required)"
                ),
                "name", Map.of(
                    "type", "string",
                    "description", "Updated name of the contact field type",
                    "minLength", 1,
                    "maxLength", 255
                ),
                "type", Map.of(
                    "type", "string",
                    "description", "Updated type of the field"
                ),
                "protocol", Map.of(
                    "type", "string",
                    "description", "Updated protocol for the field type"
                ),
                "fontawesomeIcon", Map.of(
                    "type", "string",
                    "description", "Updated Font Awesome icon class"
                ),
                "delible", Map.of(
                    "type", "boolean",
                    "description", "Updated delibility setting"
                )
            ),
            "required", java.util.List.of("id"),
            "additionalProperties", false
        );
    }
}
