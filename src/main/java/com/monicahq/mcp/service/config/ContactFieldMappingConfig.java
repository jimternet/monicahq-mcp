package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for Contact entities.
 * <p>
 * Defines the field mappings, validation rules, and API endpoint for Contact CRUD operations.
 * </p>
 * <p>
 * Field mappings:
 * <ul>
 *   <li>firstName (client) -> first_name (API)</li>
 *   <li>lastName (client) -> last_name (API)</li>
 *   <li>genderId (client) -> gender_id (API)</li>
 *   <li>isBirthdateKnown (client) -> is_birthdate_known (API)</li>
 *   <li>isDeceased (client) -> is_deceased (API)</li>
 *   <li>isDeceasedDateKnown (client) -> is_deceased_date_known (API)</li>
 *   <li>jobTitle (client) -> job_title (API)</li>
 * </ul>
 * </p>
 * <p>
 * Note: The updateContact operation uses a custom "fetch-before-update" pattern
 * that preserves required fields from the existing contact. This is handled in
 * ContactService by overriding the update method.
 * </p>
 * <p>
 * Note: Birthdate parsing (YYYY-MM-DD to year/month/day integers) is handled
 * separately in ContactService because it requires complex transformation.
 * </p>
 * <p>
 * Required fields for creation: firstName, genderId
 * </p>
 * <p>
 * Available list filters: search (mapped to 'query'), tagId (mapped to 'tags')
 * </p>
 */
@Component
public class ContactFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/contacts";
    private static final String ENTITY_NAME = "Contact";

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     * Note: 'birthdate' is not mapped here as it requires complex transformation
     * to year/month/day integers in ContactService.
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.ofEntries(
        Map.entry("firstName", "first_name"),
        Map.entry("lastName", "last_name"),
        Map.entry("genderId", "gender_id"),
        Map.entry("isBirthdateKnown", "is_birthdate_known"),
        Map.entry("isDeceased", "is_deceased"),
        Map.entry("isDeceasedDateKnown", "is_deceased_date_known"),
        Map.entry("jobTitle", "job_title")
    );

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.ofEntries(
        Map.entry("first_name", "firstName"),
        Map.entry("last_name", "lastName"),
        Map.entry("gender_id", "genderId"),
        Map.entry("is_birthdate_known", "isBirthdateKnown"),
        Map.entry("is_deceased", "isDeceased"),
        Map.entry("is_deceased_date_known", "isDeceasedDateKnown"),
        Map.entry("job_title", "jobTitle"),
        Map.entry("created_at", "createdAt"),
        Map.entry("updated_at", "updatedAt")
    );

    /**
     * Required fields for Contact creation.
     * Note: firstName has additional non-empty string validation handled in ContactService.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of("firstName", "genderId");

    /**
     * Fields that can be used as filters in list operations.
     */
    private static final List<String> LIST_FILTER_FIELDS = List.of("search", "tagId");

    /**
     * Query parameter mappings for list operations.
     * Maps client filter names to API query parameter names.
     */
    private static final Map<String, String> QUERY_PARAM_MAPPINGS = Map.of(
        "search", "query",
        "tagId", "tags"
    );

    /**
     * Default values for boolean fields during creation.
     */
    private static final Map<String, Object> CREATE_DEFAULTS = Map.of(
        "isBirthdateKnown", false,
        "isDeceased", false,
        "isDeceasedDateKnown", false
    );

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
        return LIST_FILTER_FIELDS;
    }

    @Override
    public Map<String, String> getQueryParamMappings() {
        return QUERY_PARAM_MAPPINGS;
    }

    @Override
    public Map<String, Object> getCreateDefaults() {
        return CREATE_DEFAULTS;
    }
}
