package com.monicahq.mcp.service.base;

import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Interface defining standard CRUD operations that all Monica API services should implement.
 * <p>
 * This interface provides a consistent contract across all entity services, ensuring
 * uniform behavior for create, read, update, delete, and list operations. Each method
 * accepts a {@code Map<String, Object>} for flexible argument passing and returns a
 * {@code Mono<Map<String, Object>>} for reactive, non-blocking execution.
 * </p>
 * <p>
 * Services may implement this interface directly or extend {@link AbstractCrudService}
 * which provides default implementations of these operations using the
 * {@link FieldMappingConfig} for entity-specific behavior.
 * </p>
 * <p>
 * Response format follows Constitutional Principle VI for Claude Desktop visibility,
 * with data structured as:
 * <pre>
 * {
 *   "data": {...entity data...},
 *   "content": [{"type": "text", "text": "...formatted JSON..."}]
 * }
 * </pre>
 * </p>
 *
 * @see AbstractCrudService
 * @see FieldMappingConfig
 */
public interface CrudOperations {

    /**
     * Creates a new entity with the provided data.
     * <p>
     * Required fields are validated according to the entity's configuration.
     * Field names are mapped from camelCase to snake_case before API submission.
     * </p>
     *
     * @param arguments the creation arguments containing entity data
     *                  (e.g., {@code {"contactId": 123, "body": "Note content"}})
     * @return a Mono emitting the formatted response with the created entity,
     *         or an error if validation fails or the API returns an error
     * @throws IllegalArgumentException if required fields are missing or invalid
     * @throws UnsupportedOperationException if the entity does not support creation
     */
    Mono<Map<String, Object>> create(Map<String, Object> arguments);

    /**
     * Retrieves a single entity by its ID.
     * <p>
     * The ID is extracted from the arguments map using the entity's configured
     * ID field name (typically "id").
     * </p>
     *
     * @param arguments the arguments containing the entity ID
     *                  (e.g., {@code {"id": 123}})
     * @return a Mono emitting the formatted response with the entity data,
     *         or an error if the entity is not found
     * @throws IllegalArgumentException if the ID is missing or has invalid format
     */
    Mono<Map<String, Object>> get(Map<String, Object> arguments);

    /**
     * Updates an existing entity with the provided data.
     * <p>
     * The ID is extracted from arguments to identify the entity. The remaining
     * fields are used as the update payload. Field names are mapped from
     * camelCase to snake_case before API submission.
     * </p>
     *
     * @param arguments the arguments containing the entity ID and update data
     *                  (e.g., {@code {"id": 123, "body": "Updated content"}})
     * @return a Mono emitting the formatted response with the updated entity,
     *         or an error if the entity is not found or validation fails
     * @throws IllegalArgumentException if the ID is missing or required update
     *                                  fields are invalid
     * @throws UnsupportedOperationException if the entity does not support updates
     */
    Mono<Map<String, Object>> update(Map<String, Object> arguments);

    /**
     * Deletes an entity by its ID.
     * <p>
     * Returns a confirmation message upon successful deletion.
     * </p>
     *
     * @param arguments the arguments containing the entity ID
     *                  (e.g., {@code {"id": 123}})
     * @return a Mono emitting a formatted delete confirmation response,
     *         or an error if the entity is not found
     * @throws IllegalArgumentException if the ID is missing or has invalid format
     * @throws UnsupportedOperationException if the entity does not support deletion
     */
    Mono<Map<String, Object>> delete(Map<String, Object> arguments);

    /**
     * Lists entities with optional filtering and pagination.
     * <p>
     * Supported pagination parameters:
     * <ul>
     *   <li>{@code page} - page number (default: 1)</li>
     *   <li>{@code limit} - items per page (default: 10, max: 100)</li>
     * </ul>
     * Additional filter parameters depend on the entity type.
     * </p>
     *
     * @param arguments the arguments containing optional filters and pagination
     *                  (e.g., {@code {"page": 1, "limit": 20, "contactId": 123}})
     * @return a Mono emitting the formatted list response with entities and
     *         pagination metadata, or an error if the request fails
     * @throws UnsupportedOperationException if the entity does not support listing
     */
    Mono<Map<String, Object>> list(Map<String, Object> arguments);
}
