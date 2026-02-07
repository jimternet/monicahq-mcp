package com.monicahq.mcp.service.config;

import com.monicahq.mcp.service.base.FieldMappingConfig;
import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Field mapping configuration for LifeEvent entities.
 * <p>
 * Defines the field mappings and API endpoint for LifeEvent operations.
 * LifeEvents represent significant milestones in a contact's life.
 * </p>
 * <p>
 * Gap Analysis Phase 1: New direct CRUD operations.
 * Previously only supported contact-scoped operations (createContactLifeEvent, listContactLifeevents).
 * Now enables direct CRUD operations for standalone life event management.
 * </p>
 * <p>
 * LifeEvent fields include:
 * - contact_id (required) - The contact this event belongs to
 * - life_event_type_id (required) - Type of event (graduation, marriage, birth, etc.)
 * - name (required) - Event name/title
 * - note (optional) - Additional details
 * - happened_at (required) - Date the event occurred
 * - happened_at_month_unknown (optional) - Flag if month is unknown
 * - happened_at_day_unknown (optional) - Flag if day is unknown
 * - reminder_id (optional) - Associated reminder
 * </p>
 */
@Component
public class LifeEventFieldMappingConfig implements FieldMappingConfig {

    private static final String ENDPOINT_PATH = "/lifeevents";
    private static final String ENTITY_NAME = "LifeEvent";

    /**
     * Field mappings from snake_case (API) to camelCase (client).
     */
    private static final Map<String, String> FROM_API_MAPPINGS = Map.of(
        "contact_id", "contactId",
        "life_event_type_id", "lifeEventTypeId",
        "happened_at", "happenedAt",
        "happened_at_month_unknown", "happenedAtMonthUnknown",
        "happened_at_day_unknown", "happenedAtDayUnknown",
        "reminder_id", "reminderId",
        "created_at", "createdAt",
        "updated_at", "updatedAt"
    );

    /**
     * Field mappings from camelCase (client) to snake_case (API).
     */
    private static final Map<String, String> TO_API_MAPPINGS = Map.of(
        "contactId", "contact_id",
        "lifeEventTypeId", "life_event_type_id",
        "happenedAt", "happened_at",
        "happenedAtMonthUnknown", "happened_at_month_unknown",
        "happenedAtDayUnknown", "happened_at_day_unknown",
        "reminderId", "reminder_id"
    );

    /**
     * Required fields for creating a new life event.
     */
    private static final Set<String> REQUIRED_CREATE_FIELDS = Set.of(
        "contactId",
        "lifeEventTypeId",
        "name",
        "happenedAt"
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
        return List.of("contactId", "lifeEventTypeId");
    }

    @Override
    public boolean supportsCreate() {
        return true;
    }

    @Override
    public boolean supportsUpdate() {
        return true;
    }

    @Override
    public boolean supportsDelete() {
        return true;
    }
}
