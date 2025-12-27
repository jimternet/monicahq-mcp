package com.monicahq.mcp.service.base;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Interface that defines entity-specific field mappings, validation rules, and API endpoint paths.
 * <p>
 * This configuration is used by {@link AbstractCrudService} to handle entity-specific differences
 * while keeping common CRUD logic centralized. Each entity service should provide an implementation
 * of this interface that describes its unique characteristics.
 * </p>
 * <p>
 * Key responsibilities:
 * <ul>
 *   <li>Define the API endpoint path for CRUD operations</li>
 *   <li>Provide field name mappings between camelCase (client) and snake_case (API)</li>
 *   <li>Specify required fields for create and update operations</li>
 *   <li>Define query parameter mappings for list operations</li>
 * </ul>
 * </p>
 */
public interface FieldMappingConfig {

    /**
     * Returns the API endpoint path for this entity (e.g., "/notes", "/tasks", "/debts").
     * This path is used as the base for all CRUD operations:
     * <ul>
     *   <li>POST to [path] for create</li>
     *   <li>GET to [path]/{id} for single entity retrieval</li>
     *   <li>PUT to [path]/{id} for update</li>
     *   <li>DELETE to [path]/{id} for deletion</li>
     *   <li>GET to [path] for list</li>
     * </ul>
     *
     * @return the API endpoint path starting with "/"
     */
    String getEndpointPath();

    /**
     * Returns the human-readable entity name (e.g., "Note", "Task", "Debt").
     * Used in log messages and formatted responses for operations like delete.
     *
     * @return the entity name in singular form with proper capitalization
     */
    String getEntityName();

    /**
     * Returns the field name mappings from camelCase (client/MCP) to snake_case (Monica API).
     * <p>
     * These mappings are applied when converting request data before sending to the API.
     * Common mappings include:
     * <ul>
     *   <li>"contactId" -> "contact_id"</li>
     *   <li>"isFavorited" -> "is_favorited"</li>
     *   <li>"completedAt" -> "completed_at"</li>
     * </ul>
     * </p>
     * <p>
     * Fields not in this map are passed through unchanged.
     * </p>
     *
     * @return a map of camelCase field names to their snake_case API equivalents
     */
    Map<String, String> getToApiMappings();

    /**
     * Returns the field name mappings from snake_case (Monica API) to camelCase (client/MCP).
     * <p>
     * These mappings are applied when converting API responses for client consumption.
     * Common mappings include:
     * <ul>
     *   <li>"contact_id" -> "contactId"</li>
     *   <li>"created_at" -> "createdAt"</li>
     *   <li>"updated_at" -> "updatedAt"</li>
     * </ul>
     * </p>
     * <p>
     * Fields not in this map are passed through unchanged.
     * Note: "created_at" and "updated_at" are automatically included by default.
     * </p>
     *
     * @return a map of snake_case API field names to their camelCase equivalents
     */
    Map<String, String> getFromApiMappings();

    /**
     * Returns the set of field names that are required for entity creation.
     * <p>
     * These fields will be validated before making a create API call.
     * Example: {"contactId", "body"} for Note creation.
     * </p>
     *
     * @return a set of required field names (in camelCase)
     */
    Set<String> getRequiredCreateFields();

    /**
     * Returns the set of field names that are required for entity update.
     * <p>
     * These fields will be validated before making an update API call.
     * Often empty for entities that allow partial updates.
     * </p>
     *
     * @return a set of required field names (in camelCase)
     */
    default Set<String> getRequiredUpdateFields() {
        return Collections.emptySet();
    }

    /**
     * Returns query parameter mappings for list operations.
     * <p>
     * These mappings convert camelCase filter parameters to their API query parameter equivalents.
     * For example:
     * <ul>
     *   <li>"contactId" -> "contact_id"</li>
     *   <li>"favorited" -> "is_favorited"</li>
     * </ul>
     * </p>
     * <p>
     * If not overridden, defaults to using the same mappings as {@link #getToApiMappings()}.
     * </p>
     *
     * @return a map of camelCase query parameter names to their API equivalents
     */
    default Map<String, String> getQueryParamMappings() {
        return getToApiMappings();
    }

    /**
     * Returns the list of filter fields that can be used in list operations.
     * <p>
     * Only these fields will be passed as query parameters when listing entities.
     * Common filter fields include:
     * <ul>
     *   <li>"contactId" - filter by associated contact</li>
     *   <li>"completed" - filter by completion status (for tasks)</li>
     *   <li>"favorited" - filter by favorite status (for notes)</li>
     * </ul>
     * </p>
     *
     * @return a list of field names (in camelCase) that can be used as filters
     */
    default List<String> getListFilterFields() {
        return Collections.emptyList();
    }

    /**
     * Returns the ID field name used in request arguments.
     * <p>
     * Usually "id", but may differ for entities with composite keys or special naming.
     * </p>
     *
     * @return the ID field name, defaults to "id"
     */
    default String getIdFieldName() {
        return "id";
    }

    /**
     * Returns default values to apply during entity creation.
     * <p>
     * These values are applied if the corresponding field is not provided in the request.
     * For example, TaskService defaults "completed" to false.
     * </p>
     *
     * @return a map of field names to their default values
     */
    default Map<String, Object> getCreateDefaults() {
        return Collections.emptyMap();
    }

    /**
     * Indicates whether this entity supports create operations.
     * <p>
     * Lookup/reference entities (like Gender, Currency, Country) may be read-only.
     * </p>
     *
     * @return true if create is supported, false otherwise
     */
    default boolean supportsCreate() {
        return true;
    }

    /**
     * Indicates whether this entity supports update operations.
     * <p>
     * Some entities may be immutable after creation.
     * </p>
     *
     * @return true if update is supported, false otherwise
     */
    default boolean supportsUpdate() {
        return true;
    }

    /**
     * Indicates whether this entity supports delete operations.
     * <p>
     * Some entities may not allow deletion (e.g., system-defined lookup values).
     * </p>
     *
     * @return true if delete is supported, false otherwise
     */
    default boolean supportsDelete() {
        return true;
    }

    /**
     * Indicates whether this entity supports list operations.
     *
     * @return true if list is supported, false otherwise (defaults to true)
     */
    default boolean supportsList() {
        return true;
    }
}
