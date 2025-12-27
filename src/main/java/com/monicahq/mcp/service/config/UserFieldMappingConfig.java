package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for User entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for User CRUD operations.
 * </p>
 * <p>
 * Note: The Users API may return 404 responses in some Monica configurations.
 * This may be an admin-only feature or not implemented in all Monica versions.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>firstName (client) -> first_name (API)</li>
 *   <li>lastName (client) -> last_name (API)</li>
 *   <li>isAdministrator (client) -> is_administrator (API)</li>
 *   <li>profilePictureUrl (client) -> profile_picture_url (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: firstName, email
 * </p>
 */
@Component
public class UserFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/users";
    private static final String ENTITY_NAME = "User";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "firstName", "first_name",
        "lastName", "last_name",
        "isAdministrator", "is_administrator",
        "profilePictureUrl", "profile_picture_url"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "first_name", "firstName",
        "last_name", "lastName",
        "is_administrator", "isAdministrator",
        "profile_picture_url", "profilePictureUrl",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for User creation.
     * Note: firstName and email require non-empty string validation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("firstName", "email");

    @Override
    public String getEndpointPath() {
        return ENDPOINT_PATH;
    }

    @Override
    public String getEntityName() {
        return ENTITY_NAME;
    }

    @Override
    public Map<String, String> getToApiMappings() {
        return TO_API_MAPPINGS;
    }

    @Override
    public Map<String, String> getFromApiMappings() {
        return FROM_API_MAPPINGS;
    }

    @Override
    public Set<String> getRequiredCreateFields() {
        return REQUIRED_CREATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }
}
