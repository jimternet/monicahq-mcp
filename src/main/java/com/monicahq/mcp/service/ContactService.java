package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.dto.Contact;
import com.monicahq.mcp.service.base.AbstractCrudService;
import com.monicahq.mcp.service.base.FieldMappingConfig;
import com.monicahq.mcp.service.config.ContactFieldMappingConfig;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

/**
 * Service for managing Contact entities via the Monica API.
 * <p>
 * Extends {@link AbstractCrudService} to inherit standard CRUD operation implementations.
 * Overrides the update operation to implement a "fetch-before-update" pattern that preserves
 * required fields from the existing contact when not explicitly provided in the update request.
 * </p>
 * <p>
 * Custom operations:
 * <ul>
 *   <li>searchContacts - Search contacts by query string</li>
 *   <li>updateContactCareer - Update contact career information via /contacts/{id}/work</li>
 *   <li>getContactAuditLogs - Retrieve audit logs for a contact via /contacts/{id}/logs</li>
 * </ul>
 * </p>
 * <p>
 * Overrides mapToApiFormat to handle birthdate parsing (YYYY-MM-DD to year/month/day integers).
 * </p>
 */
@Service
@Slf4j
public class ContactService extends AbstractCrudService<Contact> {

    private final ContactFieldMappingConfig fieldMappingConfig;

    /**
     * Constructs a ContactService with required dependencies.
     *
     * @param monicaClient the HTTP client for Monica API calls
     * @param contentFormatter the formatter for response content
     * @param fieldMappingConfig the field mapping configuration for Contacts
     */
    public ContactService(MonicaHqClient monicaClient,
                          ContentFormatter contentFormatter,
                          ContactFieldMappingConfig fieldMappingConfig) {
        super(monicaClient, contentFormatter);
        this.fieldMappingConfig = fieldMappingConfig;
    }

    @Override
    protected FieldMappingConfig getFieldMappingConfig() {
        return fieldMappingConfig;
    }

    /**
     * Creates a new contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>firstName - The contact's first name (non-empty string)</li>
     *   <li>genderId - The gender ID (1=Man, 2=Woman, 3=Other)</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>lastName - The contact's last name</li>
     *   <li>nickname - A nickname for the contact</li>
     *   <li>birthdate - Birth date in YYYY-MM-DD format (full) or MM-DD format (partial, year unknown)</li>
     *   <li>jobTitle - The contact's job title</li>
     *   <li>company - The contact's company</li>
     * </ul>
     * </p>
     *
     * @param arguments the creation arguments
     * @return a Mono containing the created contact data
     */
    public Mono<Map<String, Object>> createContact(Map<String, Object> arguments) {
        // Validate firstName is non-empty string before delegating to base class
        if (arguments != null && !arguments.isEmpty()) {
            validateContactCreateArguments(arguments);
        }
        return create(arguments);
    }

    /**
     * Retrieves a contact by its ID.
     *
     * @param arguments map containing "id" - the contact ID to retrieve
     * @return a Mono containing the contact data
     */
    public Mono<Map<String, Object>> getContact(Map<String, Object> arguments) {
        return get(arguments);
    }

    /**
     * Updates an existing contact using the "fetch-before-update" pattern.
     * <p>
     * This method fetches the existing contact first to preserve required fields
     * that MonicaHQ API requires even when not changing them.
     * </p>
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The ID of the contact to update</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>firstName - New first name</li>
     *   <li>lastName - New last name</li>
     *   <li>birthdate - New birth date in YYYY-MM-DD format (full) or MM-DD format (partial, year unknown). Auto-sets isBirthdateKnown=true.</li>
     *   <li>jobTitle - New job title</li>
     *   <li>All other contact fields</li>
     * </ul>
     * </p>
     *
     * @param arguments the update arguments including the contact ID
     * @return a Mono containing the updated contact data
     */
    public Mono<Map<String, Object>> updateContact(Map<String, Object> arguments) {
        log.info("Updating contact with arguments: {}", arguments);

        try {
            Long contactId = extractId(arguments);

            // Fetch existing contact to ensure all required fields are present
            return monicaClient.get("/contacts/" + contactId, null)
                .flatMap(existingContact -> {
                    // Extract the data portion of the response
                    @SuppressWarnings("unchecked")
                    Map<String, Object> existingData = existingContact.containsKey("data") ?
                        (Map<String, Object>) existingContact.get("data") : existingContact;

                    // Create update data by merging with existing data
                    Map<String, Object> updateData = new HashMap<>(arguments);
                    updateData.remove("id");

                    // Ensure required fields from existing contact are preserved
                    // MonicaHQ requires these fields even when not changing them
                    if (!updateData.containsKey("firstName") && existingData.containsKey("first_name")) {
                        updateData.put("firstName", existingData.get("first_name"));
                    }
                    if (!updateData.containsKey("lastName") && existingData.containsKey("last_name")) {
                        updateData.put("lastName", existingData.get("last_name"));
                    }
                    if (!updateData.containsKey("genderId") && existingData.containsKey("gender_id")) {
                        updateData.put("genderId", existingData.get("gender_id"));
                    }

                    // Handle birthdate logic - if birthdate is provided, automatically set isBirthdateKnown to true
                    if (updateData.containsKey("birthdate") && updateData.get("birthdate") != null
                        && !updateData.get("birthdate").toString().trim().isEmpty()) {
                        // If birthdate is provided, ensure isBirthdateKnown is true
                        updateData.put("isBirthdateKnown", true);
                        log.info("Setting isBirthdateKnown=true because birthdate was provided");
                    }

                    // Always include required boolean fields
                    if (!updateData.containsKey("isBirthdateKnown")) {
                        Object value = existingData.get("is_birthdate_known");
                        updateData.put("isBirthdateKnown", value != null ? value : false);
                    }
                    if (!updateData.containsKey("isDeceased")) {
                        Object value = existingData.get("is_deceased");
                        updateData.put("isDeceased", value != null ? value : false);
                    }
                    if (!updateData.containsKey("isDeceasedDateKnown")) {
                        Object value = existingData.get("is_deceased_date_known");
                        updateData.put("isDeceasedDateKnown", value != null ? value : false);
                    }
                    // Add missing required fields per OpenAPI spec (lines 1195-1202)
                    if (!updateData.containsKey("isPartial")) {
                        Object value = existingData.get("is_partial");
                        updateData.put("isPartial", value != null ? value : false);
                    }
                    if (!updateData.containsKey("birthdateIsAgeBased")) {
                        Object value = existingData.get("birthdate_is_age_based");
                        updateData.put("birthdateIsAgeBased", value != null ? value : false);
                    }
                    if (!updateData.containsKey("deceasedDateIsAgeBased")) {
                        Object value = existingData.get("deceased_date_is_age_based");
                        updateData.put("deceasedDateIsAgeBased", value != null ? value : false);
                    }
                    if (!updateData.containsKey("deceasedDateIsYearUnknown")) {
                        Object value = existingData.get("deceased_date_is_year_unknown");
                        updateData.put("deceasedDateIsYearUnknown", value != null ? value : false);
                    }

                    // Validate that we have required fields
                    validateContactUpdateArguments(updateData);

                    Map<String, Object> apiRequest = mapToApiFormat(updateData);

                    log.info("Sending update request for contact {} with data: {}", contactId, apiRequest);
                    log.debug("===== CONTACT UPDATE DEBUG =====");
                    log.debug("Contact ID: {}", contactId);
                    log.debug("Original update arguments: {}", arguments);
                    log.debug("Merged updateData (before API mapping): {}", updateData);
                    log.debug("API request payload (after mapToApiFormat): {}", apiRequest);
                    log.debug("Birthdate-related fields in payload:");
                    log.debug("  - birthdate_year: {} (type: {})",
                        apiRequest.get("birthdate_year"),
                        apiRequest.get("birthdate_year") != null ? apiRequest.get("birthdate_year").getClass().getSimpleName() : "null");
                    log.debug("  - birthdate_month: {} (type: {})",
                        apiRequest.get("birthdate_month"),
                        apiRequest.get("birthdate_month") != null ? apiRequest.get("birthdate_month").getClass().getSimpleName() : "null");
                    log.debug("  - birthdate_day: {} (type: {})",
                        apiRequest.get("birthdate_day"),
                        apiRequest.get("birthdate_day") != null ? apiRequest.get("birthdate_day").getClass().getSimpleName() : "null");
                    log.debug("  - is_birthdate_known: {} (type: {})",
                        apiRequest.get("is_birthdate_known"),
                        apiRequest.get("is_birthdate_known") != null ? apiRequest.get("is_birthdate_known").getClass().getSimpleName() : "null");
                    log.debug("Required boolean fields in payload:");
                    log.debug("  - is_partial: {} (type: {})",
                        apiRequest.get("is_partial"),
                        apiRequest.get("is_partial") != null ? apiRequest.get("is_partial").getClass().getSimpleName() : "null");
                    log.debug("  - birthdate_is_age_based: {} (type: {})",
                        apiRequest.get("birthdate_is_age_based"),
                        apiRequest.get("birthdate_is_age_based") != null ? apiRequest.get("birthdate_is_age_based").getClass().getSimpleName() : "null");
                    log.debug("  - deceased_date_is_age_based: {} (type: {})",
                        apiRequest.get("deceased_date_is_age_based"),
                        apiRequest.get("deceased_date_is_age_based") != null ? apiRequest.get("deceased_date_is_age_based").getClass().getSimpleName() : "null");
                    log.debug("  - deceased_date_is_year_unknown: {} (type: {})",
                        apiRequest.get("deceased_date_is_year_unknown"),
                        apiRequest.get("deceased_date_is_year_unknown") != null ? apiRequest.get("deceased_date_is_year_unknown").getClass().getSimpleName() : "null");
                    log.debug("================================");

                    return monicaClient.put("/contacts/" + contactId, apiRequest)
                        .map(this::formatSingleResponse)
                        .doOnSuccess(result -> log.info("Contact updated successfully: {}", contactId))
                        .doOnError(error -> log.error("Failed to update contact {} with request {}: {}",
                            contactId, apiRequest, error.getMessage()));
                })
                .onErrorResume(error -> {
                    log.error("Failed to fetch existing contact {} for update: {}", contactId, error.getMessage());
                    return Mono.error(new IllegalStateException(
                        "Unable to update contact - failed to fetch existing data: " + error.getMessage(), error));
                });

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Deletes a contact by its ID.
     *
     * @param arguments map containing "id" - the contact ID to delete
     * @return a Mono containing the delete confirmation
     */
    public Mono<Map<String, Object>> deleteContact(Map<String, Object> arguments) {
        return delete(arguments);
    }

    /**
     * Lists contacts with optional filtering and pagination.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     *   <li>search - Search query to filter contacts</li>
     *   <li>tagId - Filter by tag ID</li>
     * </ul>
     * </p>
     *
     * @param arguments the list arguments including optional filters and pagination
     * @return a Mono containing the list of contacts and pagination metadata
     */
    public Mono<Map<String, Object>> listContacts(Map<String, Object> arguments) {
        return list(arguments);
    }

    /**
     * Searches contacts by query string.
     * <p>
     * Optional arguments:
     * <ul>
     *   <li>query - The search query (defaults to empty string)</li>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the search arguments
     * @return a Mono containing the search results and pagination metadata
     */
    public Mono<Map<String, Object>> searchContacts(Map<String, Object> arguments) {
        log.info("Searching contacts with arguments: {}", arguments);

        try {
            Map<String, String> queryParams = buildSearchQueryParams(arguments);

            return monicaClient.get("/contacts", queryParams)
                .map(this::formatListResponse)
                .doOnSuccess(result -> log.info("Contact search completed successfully"))
                .doOnError(error -> log.error("Failed to search contacts: {}", error.getMessage()));

        } catch (Exception e) {
            log.error("Error building search parameters: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Updates contact career information.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The contact ID</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>jobTitle - The job title</li>
     *   <li>company - The company name</li>
     *   <li>startDate - Employment start date</li>
     *   <li>endDate - Employment end date</li>
     *   <li>salary - Salary information</li>
     * </ul>
     * </p>
     *
     * @param arguments the career update arguments
     * @return a Mono containing the updated contact data
     */
    public Mono<Map<String, Object>> updateContactCareer(Map<String, Object> arguments) {
        log.info("Updating contact career with arguments: {}", arguments);

        try {
            Long contactId = extractId(arguments);

            // Create career update data
            Map<String, Object> careerData = new HashMap<>();
            if (arguments.containsKey("jobTitle")) {
                careerData.put("job_title", arguments.get("jobTitle"));
            }
            if (arguments.containsKey("company")) {
                careerData.put("company", arguments.get("company"));
            }
            if (arguments.containsKey("startDate")) {
                careerData.put("start_date", arguments.get("startDate"));
            }
            if (arguments.containsKey("endDate")) {
                careerData.put("end_date", arguments.get("endDate"));
            }
            if (arguments.containsKey("salary")) {
                careerData.put("salary", arguments.get("salary"));
            }

            return monicaClient.put("/contacts/" + contactId + "/work", careerData)
                .map(this::formatSingleResponse)
                .doOnSuccess(result -> log.info("Contact career updated successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to update contact career {}: {}", contactId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact career update: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    /**
     * Retrieves audit logs for a contact.
     * <p>
     * Required arguments:
     * <ul>
     *   <li>id - The contact ID</li>
     * </ul>
     * Optional arguments:
     * <ul>
     *   <li>page - Page number (default: 1)</li>
     *   <li>limit - Number of items per page, max 100 (default: 10)</li>
     * </ul>
     * </p>
     *
     * @param arguments the audit logs arguments
     * @return a Mono containing the audit logs and pagination metadata
     */
    public Mono<Map<String, Object>> getContactAuditLogs(Map<String, Object> arguments) {
        log.info("Getting contact audit logs with arguments: {}", arguments);

        try {
            Long contactId = extractId(arguments);
            Map<String, String> queryParams = buildListQueryParams(arguments);

            return monicaClient.get("/contacts/" + contactId + "/logs", queryParams)
                .map(this::formatAuditLogListResponse)
                .doOnSuccess(result -> log.info("Contact audit logs retrieved successfully: {}", contactId))
                .doOnError(error -> log.error("Failed to get contact audit logs {}: {}", contactId, error.getMessage()));

        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for contact audit logs: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    // ========================================================================================
    // CUSTOM VALIDATION
    // ========================================================================================

    /**
     * Validates contact creation arguments with special firstName handling.
     * <p>
     * The base class validates required fields presence, but contacts need
     * additional validation for firstName as a non-empty string.
     * </p>
     *
     * @param arguments the arguments to validate
     * @throws IllegalArgumentException if validation fails
     */
    private void validateContactCreateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact creation arguments cannot be empty");
        }

        if (!arguments.containsKey("firstName") ||
            arguments.get("firstName") == null ||
            arguments.get("firstName").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("firstName is required - please provide a first name for the contact");
        }

        if (!arguments.containsKey("genderId") || arguments.get("genderId") == null) {
            throw new IllegalArgumentException("genderId is required - please provide a gender ID (1=Man, 2=Woman, 3=Other)");
        }
    }

    /**
     * Validates contact update arguments.
     *
     * @param arguments the arguments to validate
     * @throws IllegalArgumentException if validation fails
     */
    private void validateContactUpdateArguments(Map<String, Object> arguments) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException("Contact update arguments cannot be empty");
        }

        // MonicaHQ API requires these fields for ALL updates
        if (!arguments.containsKey("firstName") ||
            arguments.get("firstName") == null ||
            arguments.get("firstName").toString().trim().isEmpty()) {
            throw new IllegalArgumentException("firstName is required for updates - MonicaHQ requires this field even when not changing it");
        }

        // Ensure required boolean fields are present
        if (!arguments.containsKey("isBirthdateKnown")) {
            throw new IllegalArgumentException("isBirthdateKnown is required for updates - MonicaHQ requires this field");
        }

        if (!arguments.containsKey("isDeceased")) {
            throw new IllegalArgumentException("isDeceased is required for updates - MonicaHQ requires this field");
        }

        if (!arguments.containsKey("isDeceasedDateKnown")) {
            throw new IllegalArgumentException("isDeceasedDateKnown is required for updates - MonicaHQ requires this field");
        }
    }

    // ========================================================================================
    // CUSTOM QUERY PARAMETER BUILDING
    // ========================================================================================

    /**
     * Builds query parameters for search operations.
     *
     * @param arguments the search arguments
     * @return query parameters map
     */
    private Map<String, String> buildSearchQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();

        // Handle pagination
        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }

        if (arguments.containsKey("limit")) {
            int limit = Math.min(100, Math.max(1, Integer.parseInt(arguments.get("limit").toString())));
            queryParams.put("limit", String.valueOf(limit));
        } else {
            queryParams.put("limit", "10");
        }

        // Handle search query (required for search operation)
        if (arguments.containsKey("query") && arguments.get("query") != null) {
            queryParams.put("query", arguments.get("query").toString());
        } else {
            // Default to empty query for search (list all if no query)
            queryParams.put("query", "");
        }

        return queryParams;
    }

    // ========================================================================================
    // CUSTOM RESPONSE FORMATTING
    // ========================================================================================

    /**
     * Formats audit log list response.
     *
     * @param apiResponse the raw API response
     * @return formatted response for Claude Desktop visibility
     */
    private Map<String, Object> formatAuditLogListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> auditLogs = (List<Map<String, Object>>) apiResponse.get("data");

        // Format content as escaped JSON for Claude Desktop accessibility
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);

        Map<String, Object> result = new HashMap<>();
        result.put("data", auditLogs);

        // Extract and preserve meta from API response
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }

        // Add content field for Claude Desktop visibility
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);

        return result;
    }

    // ========================================================================================
    // CUSTOM FIELD MAPPING (for birthdate parsing)
    // ========================================================================================

    /**
     * Maps field names from camelCase to snake_case with special birthdate handling.
     * <p>
     * Overrides the base implementation to handle birthdate parsing:
     * MonicaHQ API expects day, month, year as integers instead of YYYY-MM-DD string.
     * </p>
     *
     * @param arguments the input data with camelCase field names
     * @return a new map with snake_case field names for API consumption
     */
    @Override
    protected Map<String, Object> mapToApiFormat(Map<String, Object> arguments) {
        Map<String, Object> apiRequest = new HashMap<>();
        boolean isPartialBirthdate = false;

        for (Map.Entry<String, Object> entry : arguments.entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();

            if ("birthdate".equals(key)) {
                // MonicaHQ API expects birthdate_day, birthdate_month, birthdate_year as integers
                // Supports both full (YYYY-MM-DD) and partial (MM-DD) formats for year-unknown dates
                if (value != null && !value.toString().trim().isEmpty()) {
                    try {
                        String birthdateStr = value.toString();
                        String[] parts = birthdateStr.split("-");
                        if (parts.length == 3) {
                            // Full birthdate: YYYY-MM-DD
                            apiRequest.put("birthdate_year", Integer.parseInt(parts[0]));
                            apiRequest.put("birthdate_month", Integer.parseInt(parts[1]));
                            apiRequest.put("birthdate_day", Integer.parseInt(parts[2]));
                            log.info("Converted full birthdate {} to birthdate_year={}, birthdate_month={}, birthdate_day={}",
                                birthdateStr, parts[0], parts[1], parts[2]);
                        } else if (parts.length == 2) {
                            // Partial birthdate: MM-DD (year unknown)
                            apiRequest.put("birthdate_month", Integer.parseInt(parts[0]));
                            apiRequest.put("birthdate_day", Integer.parseInt(parts[1]));
                            isPartialBirthdate = true;
                            log.info("Converted partial birthdate {} to birthdate_month={}, birthdate_day={} (year unknown)",
                                birthdateStr, parts[0], parts[1]);
                        } else {
                            log.warn("Invalid birthdate format: {}, expected YYYY-MM-DD or MM-DD", birthdateStr);
                        }
                    } catch (Exception e) {
                        log.error("Error parsing birthdate {}: {}", value, e.getMessage());
                    }
                }
            } else if ("deceasedDate".equals(key)) {
                // MonicaHQ API expects deceased_date_day, deceased_date_month, deceased_date_year as integers
                // Parse YYYY-MM-DD format to separate integer fields
                if (value != null && !value.toString().trim().isEmpty()) {
                    try {
                        String deceasedDateStr = value.toString();
                        String[] parts = deceasedDateStr.split("-");
                        if (parts.length == 3) {
                            // Full deceased date: YYYY-MM-DD
                            apiRequest.put("deceased_date_year", Integer.parseInt(parts[0]));
                            apiRequest.put("deceased_date_month", Integer.parseInt(parts[1]));
                            apiRequest.put("deceased_date_day", Integer.parseInt(parts[2]));
                            log.info("Converted deceased date {} to deceased_date_year={}, deceased_date_month={}, deceased_date_day={}",
                                deceasedDateStr, parts[0], parts[1], parts[2]);
                        } else {
                            log.warn("Invalid deceased date format: {}, expected YYYY-MM-DD", deceasedDateStr);
                        }
                    } catch (Exception e) {
                        log.error("Error parsing deceased date {}: {}", value, e.getMessage());
                    }
                }
            } else {
                // Use parent's mapping for other fields
                String apiKey = getFieldMappingConfig().getToApiMappings().getOrDefault(key, key);
                apiRequest.put(apiKey, value);
            }
        }

        // Add required boolean fields with defaults if not present
        // These fields are required by Monica API but often not provided in updates
        apiRequest.putIfAbsent("is_partial", false);
        apiRequest.putIfAbsent("birthdate_is_age_based", false);
        apiRequest.putIfAbsent("deceased_date_is_age_based", false);
        apiRequest.putIfAbsent("deceased_date_is_year_unknown", false);

        // Set birthdate_is_year_unknown to true for partial birthdates (MM-DD format)
        if (isPartialBirthdate) {
            apiRequest.put("birthdate_is_year_unknown", true);
            log.info("Set birthdate_is_year_unknown=true for partial birthdate");
        } else {
            apiRequest.putIfAbsent("birthdate_is_year_unknown", false);
        }

        return apiRequest;
    }

    // ========================================================================================
    // UTILITY METHODS (for backward compatibility with DTO conversion if needed)
    // ========================================================================================

    private Contact convertToContactDto(Map<String, Object> contactData) {
        return Contact.builder()
            .id(getLongValue(contactData, "id"))
            .firstName((String) contactData.get("firstName"))
            .lastName((String) contactData.get("lastName"))
            .nickname((String) contactData.get("nickname"))
            .genderId(getLongValue(contactData, "genderId"))
            .isBirthdateKnown(getBooleanValue(contactData, "isBirthdateKnown"))
            .isDeceased(getBooleanValue(contactData, "isDeceased"))
            .isDeceasedDateKnown(getBooleanValue(contactData, "isDeceasedDateKnown"))
            .email((String) contactData.get("email"))
            .phone((String) contactData.get("phone"))
            .birthdate(getLocalDateValue(contactData, "birthdate"))
            .company((String) contactData.get("company"))
            .jobTitle((String) contactData.get("jobTitle"))
            .createdAt(getLocalDateTimeValue(contactData, "createdAt"))
            .updatedAt(getLocalDateTimeValue(contactData, "updatedAt"))
            .build();
    }

    private Long getLongValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return null;
    }

    private Boolean getBooleanValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof Boolean) {
            return (Boolean) value;
        }
        return false;
    }

    private java.time.LocalDate getLocalDateValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof String) {
            try {
                return java.time.LocalDate.parse((String) value);
            } catch (Exception e) {
                return null;
            }
        }
        return null;
    }

    private java.time.LocalDateTime getLocalDateTimeValue(Map<String, Object> data, String key) {
        Object value = data.get(key);
        if (value instanceof String) {
            try {
                String dateTimeStr = (String) value;
                // Handle ISO format with Z
                if (dateTimeStr.endsWith("Z")) {
                    return java.time.LocalDateTime.parse(dateTimeStr.substring(0, dateTimeStr.length() - 1));
                }
                return java.time.LocalDateTime.parse(dateTimeStr);
            } catch (Exception e) {
                return null;
            }
        }
        return null;
    }
}
