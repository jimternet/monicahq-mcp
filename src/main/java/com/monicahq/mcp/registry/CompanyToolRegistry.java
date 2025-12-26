package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.CompanyService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Company operations.
 *
 * This registry handles company-related MCP tools:
 * - Company CRUD operations (create, get, update, delete, list)
 *
 * Companies represent organizations/businesses that contacts may work for
 * or be associated with. They are linked to contacts through occupations.
 *
 * The registry delegates execution to CompanyService.
 */
@Component
@Slf4j
public class CompanyToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Company";
    private static final String CATEGORY = "Company Management";

    private final CompanyService companyService;

    public CompanyToolRegistry(CompanyService companyService) {
        this.companyService = companyService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // Company CRUD operations (5)
        registerTool(
            "company_create",
            "[Company] Create a new company",
            createCompanySchema(),
            CATEGORY
        );

        registerTool(
            "company_get",
            "[Company] Get a company by ID",
            createIdSchema("Company ID"),
            CATEGORY
        );

        registerTool(
            "company_update",
            "[Company] Update an existing company",
            createCompanyUpdateSchema(),
            CATEGORY
        );

        registerTool(
            "company_delete",
            "[Company] Delete a company",
            createIdSchema("Company ID"),
            CATEGORY
        );

        registerTool(
            "company_list",
            "[Company] List companies with pagination",
            createListSchema(),
            CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            case "company_create" -> companyService.createCompany(arguments);
            case "company_get" -> companyService.getCompany(arguments);
            case "company_update" -> companyService.updateCompany(arguments);
            case "company_delete" -> companyService.deleteCompany(arguments);
            case "company_list" -> companyService.listCompanies(arguments);
            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== Company Schema Methods ==========

    /**
     * Creates the schema for company creation.
     * Defines all fields for creating a new company.
     */
    private Map<String, Object> createCompanySchema() {
        return Map.of(
            "type", "object",
            "properties", Map.of(
                "name", Map.of(
                    "type", "string",
                    "description", "Company name (required)",
                    "maxLength", 255
                ),
                "website", Map.of(
                    "type", "string",
                    "description", "Company website URL (optional)"
                )
            ),
            "required", List.of("name")
        );
    }

    /**
     * Creates the schema for company updates.
     * Wraps the create schema with an ID field requirement.
     */
    private Map<String, Object> createCompanyUpdateSchema() {
        return createUpdateSchema(createCompanySchema());
    }
}
