package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ReminderFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.Map;

/**
 * Service for managing Reminder entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Uses {@link ReminderFieldMappingConfig} for Reminder-specific field mappings and validation.
 * </p>
 * <p>
 * Supported operations:
 * <ul>
 *   <li>createReminder - Create a new reminder for a contact</li>
 *   <li>getReminder - Retrieve a reminder by ID</li>
 *   <li>updateReminder - Update an existing reminder</li>
 *   <li>deleteReminder - Delete a reminder by ID</li>
 *   <li>listReminders - List reminders with optional filtering and pagination</li>
 * </ul>
 * </p>
 */
@Service
@Slf4j
public class ReminderService extends AbstractCrudService<Object> {

    private final ReminderFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ReminderService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Reminders
     */
    public ReminderService(MonicaHqClient monicaClient,
                           ContentFormatter contentFormatter,
                           ReminderFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new reminder for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>contactId - The ID of the contact to associate the reminder with</li>
     *   <li>title - The title of the reminder (cannot be empty)</li>
     *   <li>initialDate - The date for the reminder in YYYY-MM-DD format</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>frequency_type - How often the reminder repeats (once, yearly, monthly, etc.)</li>
     *   <li>nextExpectedDate - The next expected date for the reminder</li>
     *   <li>lastTriggered - The last time the reminder was triggered</li>
     *   <li>description - Additional details about the reminder</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created reminder data
     */
    public Mono<Map<String, Object>> createReminder(Map<String, Object> arguments) {
        // Additional validation: title must be a non-empty string
        if (arguments != null && !arguments.isEmpty()) {
            validateRequiredString(arguments, "title");
        }
        return create(arguments);
    }

    /**
     * Retrieves a reminder by its ID.
     *
     * @param arguments map containing "id" - the reminder ID to retrieve
     * @return a Mono containing the reminder data
     */
    public Mono<Map<String, Object>> getReminder(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing reminder.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the reminder to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>title - New title for the reminder</li>
     *   <li>contactId - New contact association</li>
     *   <li>initialDate - New initial date</li>
     *   <li>nextExpectedDate - New next expected date</li>
     *   <li>lastTriggered - New last triggered date</li>
     *   <li>frequency_type - New frequency type</li>
     *   <li>description - New description</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the reminder ID
     * @return a Mono containing the updated reminder data
     */
    public Mono<Map<String, Object>> updateReminder(Map<String, Object> arguments) {
        return update(arguments);
    }

    /**
     * Deletes a reminder by its ID.
     *
     * @param arguments map containing "id" - the reminder ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteReminder(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists reminders with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>contactId - Filter by contact ID</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of reminders and pagination metadata
     */
    public Mono<Map<String, Object>> listReminders(Map<String, Object> arguments) {
        return list(arguments);
    }

    public Mono<Map<String, Object>> listRemindersByContact(Map<String, Object> arguments) {
        try {
            if (arguments == null || !arguments.containsKey("contactId")) {
                throw new IllegalArgumentException("contactId is required");
            }
            Object contactIdValue = arguments.get("contactId");
            if (contactIdValue == null) {
                throw new IllegalArgumentException("contactId is required");
            }
            Long contactId = contactIdValue instanceof Number
                ? ((Number) contactIdValue).longValue()
                : Long.parseLong(contactIdValue.toString().trim());

            int page = 1;
            int limit = 10;
            if (arguments.containsKey("page")) {
                page = Integer.parseInt(arguments.get("page").toString());
            }
            if (arguments.containsKey("limit")) {
                limit = parseLimit(arguments.get("limit"));
            }

            String endpoint = "/contacts/" + contactId + "/reminders";
            Map<String, String> queryParams = Map.of(
                "page", String.valueOf(page),
                "limit", String.valueOf(limit)
            );

            return monicaClient.get(endpoint, queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Reminders for contact {} listed successfully", contactId))
                .doOnError(error -> log.error("Failed to list reminders for contact {}: {}", contactId, error.getMessage()));
        } catch (IllegalArgumentException e) {
            return Mono.error(new IllegalArgumentException("Invalid arguments: " + e.getMessage()));
        }
    }
}
