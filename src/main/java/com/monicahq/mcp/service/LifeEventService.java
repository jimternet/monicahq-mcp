package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.LifeEventFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing life events in MonicaHQ API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link LifeEventFieldMappingConfig} for LifeEvent-specific field mappings.
 * </p>
 * <p>
 * Gap Analysis Phase 1: New direct CRUD operations implementation.
 * Previously only supported contact-scoped operations (createContactLifeEvent, listContactLifeevents).
 * Now enables direct CRUD operations for standalone life event management.
 * </p>
 * <p>
 * LifeEvents track significant milestones in a contact's life including:
 * - Birth, death, marriage, divorce
 * - Graduation, job changes
 * - Moving, major purchases
 * - Achievements, awards
 * - Any other significant personal events
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createLifeEvent - Create a new life event (alternative to contact-scoped creation)</li>
 *   <li>getLifeEvent - Retrieve a specific life event by ID</li>
 *   <li>updateLifeEvent - Modify life event details</li>
 *   <li>deleteLifeEvent - Remove a life event</li>
 * </ul>
 * </p>
 * <p>
 * Note: List operation is available via contact-scoped listContactLifeevents.
 * This service focuses on direct CRUD operations by life event ID.
 * </p>
 */
@Service
@Slf4j
public class LifeEventService extends AbstractCrudService<Object> {

    private final LifeEventFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a LifeEventService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for LifeEvents
     */
    public LifeEventService(MonicaHqClient monicaClient,
                            ContentFormatter contentFormatter,
                            LifeEventFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new life event.
     * <p>
     * Alternative to contact-scoped createContactLifeEvent operation.
     * Enables direct life event creation without navigating through contact context.
     * </p>
     *
     * @param arguments must contain: contactId, lifeEventTypeId, name, happenedAt
     *                  optional: note, happenedAtMonthUnknown, happenedAtDayUnknown, reminderId
     * @return a Mono containing the created life event with full Monica API data
     */
    public Mono<Map<String, Object>> createLifeEvent(Map<String, Object> arguments) {
        return create(arguments);
    }

    /**
     * Retrieves a specific life event by ID.
     * <p>
     * Provides complete life event information including event type, date, and associated contact.
     * </p>
     *
     * @param arguments must contain 'id' (the life event ID)
     * @return a Mono containing the life event details with full Monica API data
     */
    public Mono<Map<String, Object>> getLifeEvent(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing life event.
     * <p>
     * Allows modification of event details, dates, and associated information.
     * </p>
     *
     * @param arguments must contain 'id' (the life event ID) and fields to update
     * @return a Mono containing the updated life event with full Monica API data
     */
    public Mono<Map<String, Object>> updateLifeEvent(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a life event.
     * <p>
     * Removes a life event from the contact's history.
     * </p>
     *
     * @param arguments must contain 'id' (the life event ID to delete)
     * @return a Mono containing the deletion confirmation
     */
    public Mono<Map<String, Object>> deleteLifeEvent(Map<String, Object> arguments) {
        return delete(arguments);
    }
}
