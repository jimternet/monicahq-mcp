package com.monicahq.mcp.service.base;

import java.util.Map;
import java.util.Set;

/**
 * Utility class providing common validation methods for service layer operations.
 * <p>
 * This class centralizes validation logic that was previously duplicated across 34+ service classes.
 * All methods are static and can be used independently without requiring any configuration.
 * </p>
 * <p>
 * Common validation patterns provided:
 * <ul>
 *   <li>Required field validation ({@link #validateRequired(Map, String)}, {@link #validateRequiredFields(Map, Set, String)})</li>
 *   <li>String field validation ({@link #validateRequiredString(Map, String)})</li>
 *   <li>ID extraction and validation ({@link #extractId(Map, String)}, {@link #extractId(Map, String, String)})</li>
 *   <li>ID format validation ({@link #validateIdFormat(Map, String)})</li>
 *   <li>Numeric range validation ({@link #validateNumericRange(int, int, int)}, {@link #validateLimit(Object)})</li>
 * </ul>
 * </p>
 *
 * @see AbstractCrudService
 */
public final class ValidationUtils {

    /**
     * Default minimum limit for list queries.
     */
    public static final int DEFAULT_MIN_LIMIT = 1;

    /**
     * Default maximum limit for list queries.
     */
    public static final int DEFAULT_MAX_LIMIT = 100;

    /**
     * Default limit value when not specified.
     */
    public static final int DEFAULT_LIMIT = 10;

    /**
     * Private constructor to prevent instantiation.
     */
    private ValidationUtils() {
        // Utility class - no instantiation
    }

    // ========================================================================================
    // REQUIRED FIELD VALIDATION
    // ========================================================================================

    /**
     * Validates that a specific field is present and non-null in the arguments map.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the required field
     * @throws IllegalArgumentException if the field is missing or null
     */
    public static void validateRequired(Map<String, Object> arguments, String fieldName) {
        if (arguments == null || !arguments.containsKey(fieldName) || arguments.get(fieldName) == null) {
            throw new IllegalArgumentException(fieldName + " is required");
        }
    }

    /**
     * Validates that a specific field is present and non-null in the arguments map,
     * with a custom error message.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the required field
     * @param errorMessage the custom error message to use
     * @throws IllegalArgumentException if the field is missing or null
     */
    public static void validateRequired(Map<String, Object> arguments, String fieldName, String errorMessage) {
        if (arguments == null || !arguments.containsKey(fieldName) || arguments.get(fieldName) == null) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    /**
     * Validates that all specified fields are present and non-null in the arguments map.
     *
     * @param arguments the arguments map to check
     * @param requiredFields the set of required field names
     * @param entityName the entity name for error messages (e.g., "Contact", "Note")
     * @throws IllegalArgumentException if arguments are null/empty or any required field is missing
     */
    public static void validateRequiredFields(Map<String, Object> arguments, Set<String> requiredFields, String entityName) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException(entityName + " arguments cannot be empty");
        }

        for (String field : requiredFields) {
            if (!arguments.containsKey(field) || arguments.get(field) == null) {
                throw new IllegalArgumentException(field + " is required");
            }
        }
    }

    /**
     * Validates that the arguments map is not null or empty.
     *
     * @param arguments the arguments map to check
     * @param entityName the entity name for error messages
     * @throws IllegalArgumentException if arguments are null or empty
     */
    public static void validateNotEmpty(Map<String, Object> arguments, String entityName) {
        if (arguments == null || arguments.isEmpty()) {
            throw new IllegalArgumentException(entityName + " arguments cannot be empty");
        }
    }

    // ========================================================================================
    // STRING VALIDATION
    // ========================================================================================

    /**
     * Validates that a field is present, non-null, and is a non-empty string after trimming.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the field to validate
     * @throws IllegalArgumentException if the field is missing, null, or empty/whitespace-only
     */
    public static void validateRequiredString(Map<String, Object> arguments, String fieldName) {
        if (arguments == null ||
            !arguments.containsKey(fieldName) ||
            arguments.get(fieldName) == null ||
            arguments.get(fieldName).toString().trim().isEmpty()) {
            throw new IllegalArgumentException(fieldName + " is required");
        }
    }

    /**
     * Validates that a field is present, non-null, and is a non-empty string after trimming,
     * with a custom error message.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the field to validate
     * @param errorMessage the custom error message to use
     * @throws IllegalArgumentException if the field is missing, null, or empty/whitespace-only
     */
    public static void validateRequiredString(Map<String, Object> arguments, String fieldName, String errorMessage) {
        if (arguments == null ||
            !arguments.containsKey(fieldName) ||
            arguments.get(fieldName) == null ||
            arguments.get(fieldName).toString().trim().isEmpty()) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    // ========================================================================================
    // ID EXTRACTION AND VALIDATION
    // ========================================================================================

    /**
     * Extracts a numeric ID from the arguments map using a standard "id" field.
     * <p>
     * Supports both numeric IDs (Integer, Long) and string representations of numbers.
     * </p>
     *
     * @param arguments the arguments map containing the ID
     * @param entityName the entity name for error messages (e.g., "Contact", "Note")
     * @return the extracted ID as a Long
     * @throws IllegalArgumentException if ID is missing, null, or has invalid format
     */
    public static Long extractId(Map<String, Object> arguments, String entityName) {
        return extractId(arguments, "id", entityName);
    }

    /**
     * Extracts a numeric ID from the arguments map using a specified field name.
     * <p>
     * Supports both numeric IDs (Integer, Long) and string representations of numbers.
     * </p>
     *
     * @param arguments the arguments map containing the ID
     * @param idFieldName the name of the ID field (e.g., "id", "noteId", "contactId")
     * @param entityName the entity name for error messages (e.g., "Contact", "Note")
     * @return the extracted ID as a Long
     * @throws IllegalArgumentException if ID is missing, null, or has invalid format
     */
    public static Long extractId(Map<String, Object> arguments, String idFieldName, String entityName) {
        if (arguments == null || !arguments.containsKey(idFieldName)) {
            throw new IllegalArgumentException(entityName + " ID is required");
        }

        Object idValue = arguments.get(idFieldName);
        if (idValue == null) {
            throw new IllegalArgumentException(entityName + " ID is required");
        }

        if (idValue instanceof Number) {
            return ((Number) idValue).longValue();
        }

        try {
            return Long.parseLong(idValue.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(
                "Invalid " + entityName.toLowerCase() + " ID format: " + idValue);
        }
    }

    /**
     * Validates that a field contains a valid numeric ID format without extracting it.
     * <p>
     * Use this when you need to validate an ID field (like contactId) that references
     * another entity, without extracting it for immediate use.
     * </p>
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the ID field to validate
     * @throws IllegalArgumentException if the field is missing, null, or not a valid number
     */
    public static void validateIdFormat(Map<String, Object> arguments, String fieldName) {
        if (arguments == null || !arguments.containsKey(fieldName) || arguments.get(fieldName) == null) {
            throw new IllegalArgumentException(fieldName + " is required");
        }

        Object value = arguments.get(fieldName);
        if (value instanceof Number) {
            return; // Valid numeric ID
        }

        try {
            Long.parseLong(value.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(fieldName + " must be a valid number");
        }
    }

    /**
     * Validates that a field contains a valid numeric ID format without extracting it,
     * with a custom error message.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the ID field to validate
     * @param errorMessage the custom error message to use for invalid format
     * @throws IllegalArgumentException if the field is missing, null, or not a valid number
     */
    public static void validateIdFormat(Map<String, Object> arguments, String fieldName, String errorMessage) {
        if (arguments == null || !arguments.containsKey(fieldName) || arguments.get(fieldName) == null) {
            throw new IllegalArgumentException(fieldName + " is required");
        }

        Object value = arguments.get(fieldName);
        if (value instanceof Number) {
            return; // Valid numeric ID
        }

        try {
            Long.parseLong(value.toString().trim());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException(errorMessage);
        }
    }

    // ========================================================================================
    // NUMERIC RANGE VALIDATION
    // ========================================================================================

    /**
     * Validates that a value falls within the specified range.
     *
     * @param value the value to validate
     * @param min the minimum allowed value (inclusive)
     * @param max the maximum allowed value (inclusive)
     * @return the value clamped to the valid range
     */
    public static int validateNumericRange(int value, int min, int max) {
        return Math.min(max, Math.max(min, value));
    }

    /**
     * Validates and parses a limit parameter for list queries.
     * <p>
     * Returns a valid limit between {@link #DEFAULT_MIN_LIMIT} and {@link #DEFAULT_MAX_LIMIT}.
     * Returns {@link #DEFAULT_LIMIT} if the value is null or cannot be parsed.
     * </p>
     *
     * @param limitValue the limit value to parse (can be String, Number, or null)
     * @return a valid limit value within bounds
     */
    public static int validateLimit(Object limitValue) {
        if (limitValue == null) {
            return DEFAULT_LIMIT;
        }

        try {
            int limit;
            if (limitValue instanceof Number) {
                limit = ((Number) limitValue).intValue();
            } else {
                limit = Integer.parseInt(limitValue.toString().trim());
            }
            return validateNumericRange(limit, DEFAULT_MIN_LIMIT, DEFAULT_MAX_LIMIT);
        } catch (NumberFormatException e) {
            return DEFAULT_LIMIT;
        }
    }

    /**
     * Validates and parses a page parameter for list queries.
     * <p>
     * Returns a valid page number (minimum 1).
     * Returns 1 if the value is null, invalid, or less than 1.
     * </p>
     *
     * @param pageValue the page value to parse (can be String, Number, or null)
     * @return a valid page number (minimum 1)
     */
    public static int validatePage(Object pageValue) {
        if (pageValue == null) {
            return 1;
        }

        try {
            int page;
            if (pageValue instanceof Number) {
                page = ((Number) pageValue).intValue();
            } else {
                page = Integer.parseInt(pageValue.toString().trim());
            }
            return Math.max(1, page);
        } catch (NumberFormatException e) {
            return 1;
        }
    }

    // ========================================================================================
    // UTILITY METHODS
    // ========================================================================================

    /**
     * Checks if a field is present and has a non-null value in the arguments map.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the field to check
     * @return true if the field exists and has a non-null value, false otherwise
     */
    public static boolean hasValue(Map<String, Object> arguments, String fieldName) {
        return arguments != null &&
               arguments.containsKey(fieldName) &&
               arguments.get(fieldName) != null;
    }

    /**
     * Checks if a field is present and has a non-empty string value in the arguments map.
     *
     * @param arguments the arguments map to check
     * @param fieldName the name of the field to check
     * @return true if the field exists and has a non-empty string value, false otherwise
     */
    public static boolean hasNonEmptyString(Map<String, Object> arguments, String fieldName) {
        if (!hasValue(arguments, fieldName)) {
            return false;
        }
        return !arguments.get(fieldName).toString().trim().isEmpty();
    }

    /**
     * Gets a Long value from the arguments map, returning null if not present or not numeric.
     *
     * @param arguments the arguments map
     * @param fieldName the field name to extract
     * @return the Long value, or null if not present or not a valid number
     */
    public static Long getLongValue(Map<String, Object> arguments, String fieldName) {
        if (!hasValue(arguments, fieldName)) {
            return null;
        }

        Object value = arguments.get(fieldName);
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }

        try {
            return Long.parseLong(value.toString().trim());
        } catch (NumberFormatException e) {
            return null;
        }
    }

    /**
     * Gets a String value from the arguments map, returning null if not present.
     *
     * @param arguments the arguments map
     * @param fieldName the field name to extract
     * @return the trimmed String value, or null if not present
     */
    public static String getStringValue(Map<String, Object> arguments, String fieldName) {
        if (!hasValue(arguments, fieldName)) {
            return null;
        }
        return arguments.get(fieldName).toString().trim();
    }

    /**
     * Gets a Boolean value from the arguments map, returning the default if not present.
     *
     * @param arguments the arguments map
     * @param fieldName the field name to extract
     * @param defaultValue the default value if the field is not present
     * @return the Boolean value, or defaultValue if not present
     */
    public static Boolean getBooleanValue(Map<String, Object> arguments, String fieldName, boolean defaultValue) {
        if (!hasValue(arguments, fieldName)) {
            return defaultValue;
        }

        Object value = arguments.get(fieldName);
        if (value instanceof Boolean) {
            return (Boolean) value;
        }

        String strValue = value.toString().trim().toLowerCase();
        return "true".equals(strValue) || "1".equals(strValue) || "yes".equals(strValue);
    }
}
