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
 * - Gender discovery (list)
 * - Contact Field Type discovery (list)
 *
 * Reference data tools provide read-only access to system lookup values.
 * Discovery tools help users find valid IDs for use in other operations.
 *
 * The registry delegates execution to:
 * - AuditLogService for audit log operations
 * - CountryService for country operations
 * - CurrencyService for currency operations
 * - GenderService for gender discovery
 * - ContactFieldTypeService for contact field type discovery
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

        // === DISCOVERY TOOLS (2 operations) ===
        registerTool(
            "gender_list",
            "[Discovery] List all available genders",
            createListOnlySchema(),
            DISCOVERY_CATEGORY
        );

        registerTool(
            "contact_field_type_list",
            "[Discovery] List all available contact field types",
            createListOnlySchema(),
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

            // Discovery operations
            case "gender_list" -> genderService.listGenders(arguments);
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
}
