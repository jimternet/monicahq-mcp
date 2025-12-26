package com.monicahq.mcp.registry;

import com.monicahq.mcp.service.ComplianceService;
import com.monicahq.mcp.service.UserService;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Domain-specific tool registry for Administration operations.
 *
 * This registry handles administrative MCP tools:
 * - User Management operations (create, get, update, delete, list)
 * - Compliance Management operations (create, get, update, delete, list)
 *
 * Note: Users API may not be available in all Monica configurations.
 * Compliance API endpoints may not be clearly defined in all Monica versions.
 *
 * The registry delegates execution to:
 * - UserService for user operations
 * - ComplianceService for compliance operations
 */
@Component
@Slf4j
public class AdminToolRegistry extends AbstractDomainToolRegistry {

    private static final String DOMAIN = "Admin";
    private static final String USER_CATEGORY = "User Management";
    private static final String COMPLIANCE_CATEGORY = "Compliance Management";

    private final UserService userService;
    private final ComplianceService complianceService;

    public AdminToolRegistry(
            UserService userService,
            ComplianceService complianceService) {
        this.userService = userService;
        this.complianceService = complianceService;
    }

    @Override
    public String getDomain() {
        return DOMAIN;
    }

    @Override
    protected void initializeTools() {
        // === USER MANAGEMENT (5 operations) ===
        // Note: Users API may not be available in all Monica configurations
        registerTool(
            "user_create",
            "[User] Create a new user account",
            createUserSchema(),
            USER_CATEGORY
        );

        registerTool(
            "user_get",
            "[User] Get a user by ID",
            createIdSchema("User ID"),
            USER_CATEGORY
        );

        registerTool(
            "user_update",
            "[User] Update an existing user",
            createUserUpdateSchema(),
            USER_CATEGORY
        );

        registerTool(
            "user_delete",
            "[User] Delete a user",
            createIdSchema("User ID"),
            USER_CATEGORY
        );

        registerTool(
            "user_list",
            "[User] List users with pagination",
            createListSchema(),
            USER_CATEGORY
        );

        // === COMPLIANCE MANAGEMENT (5 operations) ===
        // Note: Compliance API endpoints may not be clearly defined in all Monica versions
        registerTool(
            "compliance_create",
            "[Compliance] Create a new compliance record",
            createComplianceSchema(),
            COMPLIANCE_CATEGORY
        );

        registerTool(
            "compliance_get",
            "[Compliance] Get a compliance record by ID",
            createIdSchema("Compliance ID"),
            COMPLIANCE_CATEGORY
        );

        registerTool(
            "compliance_update",
            "[Compliance] Update an existing compliance record",
            createComplianceUpdateSchema(),
            COMPLIANCE_CATEGORY
        );

        registerTool(
            "compliance_delete",
            "[Compliance] Delete a compliance record",
            createIdSchema("Compliance ID"),
            COMPLIANCE_CATEGORY
        );

        registerTool(
            "compliance_list",
            "[Compliance] List compliance records with pagination",
            createListSchema(),
            COMPLIANCE_CATEGORY
        );
    }

    @Override
    protected Mono<Map<String, Object>> executeToolInternal(String toolName, Map<String, Object> arguments) {
        return switch (toolName) {
            // User operations
            case "user_create" -> userService.createUser(arguments);
            case "user_get" -> userService.getUser(arguments);
            case "user_update" -> userService.updateUser(arguments);
            case "user_delete" -> userService.deleteUser(arguments);
            case "user_list" -> userService.listUsers(arguments);

            // Compliance operations
            case "compliance_create" -> complianceService.createCompliance(arguments);
            case "compliance_get" -> complianceService.getCompliance(arguments);
            case "compliance_update" -> complianceService.updateCompliance(arguments);
            case "compliance_delete" -> complianceService.deleteCompliance(arguments);
            case "compliance_list" -> complianceService.listCompliance(arguments);

            default -> Mono.error(new UnsupportedOperationException(
                "Tool '" + toolName + "' is not implemented in " + DOMAIN + " domain registry"));
        };
    }

    // ========== User Management Schema Methods ==========

    /**
     * Creates the schema for user creation operations.
     * Requires firstName and email, optional fields for profile settings.
     */
    private Map<String, Object> createUserSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("firstName", Map.of(
            "type", "string",
            "description", "First name (required)",
            "maxLength", 255
        ));

        properties.put("lastName", Map.of(
            "type", "string",
            "description", "Last name",
            "maxLength", 255
        ));

        properties.put("email", Map.of(
            "type", "string",
            "format", "email",
            "description", "Email address (required)",
            "maxLength", 255
        ));

        properties.put("timezone", Map.of(
            "type", "string",
            "description", "User timezone"
        ));

        properties.put("locale", Map.of(
            "type", "string",
            "description", "User locale"
        ));

        properties.put("currency", Map.of(
            "type", "string",
            "description", "User currency"
        ));

        properties.put("isAdministrator", Map.of(
            "type", "boolean",
            "description", "Administrator flag"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("firstName", "email"));

        return schema;
    }

    /**
     * Creates the schema for user update operations.
     * Includes ID for identifying the user, plus all user properties.
     */
    private Map<String, Object> createUserUpdateSchema() {
        return createUpdateSchema(createUserSchema());
    }

    // ========== Compliance Management Schema Methods ==========

    /**
     * Creates the schema for compliance record creation operations.
     * Requires type, optional fields for consent and audit settings.
     */
    private Map<String, Object> createComplianceSchema() {
        Map<String, Object> properties = new HashMap<>();

        properties.put("contactId", Map.of(
            "type", "integer",
            "description", "Associated contact ID"
        ));

        properties.put("type", Map.of(
            "type", "string",
            "description", "Compliance type (required)",
            "maxLength", 100
        ));

        properties.put("description", Map.of(
            "type", "string",
            "description", "Compliance description",
            "maxLength", 500
        ));

        properties.put("isActive", Map.of(
            "type", "boolean",
            "description", "Active status"
        ));

        properties.put("dataRetentionDays", Map.of(
            "type", "integer",
            "description", "Data retention period in days"
        ));

        properties.put("privacyLevel", Map.of(
            "type", "string",
            "description", "Privacy level"
        ));

        properties.put("consentGiven", Map.of(
            "type", "boolean",
            "description", "Consent status"
        ));

        properties.put("consentDate", Map.of(
            "type", "string",
            "format", "date-time",
            "description", "Consent date"
        ));

        properties.put("auditRequired", Map.of(
            "type", "boolean",
            "description", "Audit requirement flag"
        ));

        Map<String, Object> schema = new HashMap<>();
        schema.put("type", "object");
        schema.put("properties", properties);
        schema.put("required", List.of("type"));

        return schema;
    }

    /**
     * Creates the schema for compliance record update operations.
     * Includes ID for identifying the record, plus all compliance properties.
     */
    private Map<String, Object> createComplianceUpdateSchema() {
        return createUpdateSchema(createComplianceSchema());
    }
}
