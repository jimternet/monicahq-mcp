package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Pet entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Pet CRUD operations.
 * This service uses root-level POST endpoint (/pets) instead of nested endpoint
 * (/contacts/{id}/pets) to avoid HTTP 405 errors. The contact_id is passed in the
 * request body as required by the MonicaHQ API.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>contactId (client) -> contact_id (API)</li>
 *   <li>petCategoryId (client) -> pet_category_id (API)</li>
 * </ul>
 * </p>
 * <p>
 * Required fields for creation: contactId, petCategoryId
 * </p>
 * <p>
 * Required fields for update: petCategoryId (required by API)
 * </p>
 *
 * @see <a href="docs/API-LIMITATIONS.md">API Limitations documentation</a>
 */
@Component
public class PetFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/pets";
    private static final String ENTITY_NAME = "Pet";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "petCategoryId", "pet_category_id"
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "pet_category_id", "petCategoryId",
        "pet_category", "petCategory",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Required fields for Pet creation.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("contactId", "petCategoryId");

    /**
     * Required fields for Pet update.
     * The petCategoryId is required by the API for updates.
     */
    private static final Set<String> REQUIRED_UPDATE_FIELDS = Set.of("petCategoryId");

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
    public Set<String> getRequiredUpdateFields() {
        return REQUIRED_UPDATE_FIELDS;
    }

    @Override
    public List<String> getListFilterFields() {
        return Collections.emptyList();
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return Collections.emptyMap();
    }
}
