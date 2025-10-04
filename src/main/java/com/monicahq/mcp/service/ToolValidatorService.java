package com.monicahq.mcp.service;

import com.monicahq.mcp.util.ParameterFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.util.*;
import java.util.regex.Pattern;

/**
 * Service for validating MCP tool parameters and operations
 * Provides comprehensive validation for all tool calls
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class ToolValidatorService {

    private final ParameterFormatter parameterFormatter;
    
    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));
    
    // Common validation patterns
    private static final Pattern EMAIL_PATTERN = Pattern.compile("^[A-Za-z0-9+_.-]+@(.+)$");
    private static final Pattern PHONE_PATTERN = Pattern.compile("^[+]?[1-9]?[0-9]{7,15}$");
    private static final Pattern ISO_DATE_PATTERN = Pattern.compile("^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d{3})?Z?$");
    
    // Tool parameter schemas
    private static final Map<String, Set<String>> REQUIRED_PARAMETERS = Map.of(
        "activity_create", Set.of("summary", "attendees"),
        "contact_create", Set.of("firstName"),
        "note_create", Set.of("body", "contactId"),
        "task_create", Set.of("title", "contactId"),
        "call_create", Set.of("contactId"),
        "reminder_create", Set.of("title", "contactId", "triggerDate")
    );
    
    private static final Map<String, Set<String>> OPTIONAL_PARAMETERS = Map.of(
        "activity_create", Set.of("description", "happenedAt", "duration", "activityTypeId"),
        "contact_create", Set.of("lastName", "nickname", "gender", "email", "phone", "birthdate"),
        "note_create", Set.of("title"),
        "task_create", Set.of("description", "completed", "completedAt"),
        "call_create", Set.of("duration", "description", "happenedAt"),
        "reminder_create", Set.of("description", "frequency", "numberOfOccurrences")
    );

    /**
     * Validates all parameters for a tool call
     * @param toolName Tool name
     * @param parameters Parameter map
     * @return ValidationResult containing validation status and errors
     */
    public ValidationResult validateToolParameters(String toolName, Map<String, Object> parameters) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Validating parameters for tool: {}", toolName);
            log.debug("[MCP-DEBUG] Parameters: {}", parameterFormatter.formatParameterMap(parameters));
        }
        
        ValidationResult result = new ValidationResult();
        
        // Check for null or empty parameters
        if (parameters == null) {
            result.addError("parameters", null, "Parameters cannot be null");
            return result;
        }
        
        // Validate required parameters
        validateRequiredParameters(toolName, parameters, result);
        
        // Validate parameter types and values
        validateParameterTypes(toolName, parameters, result);
        
        // Check for unknown parameters
        validateUnknownParameters(toolName, parameters, result);
        
        // Tool-specific validations
        performToolSpecificValidation(toolName, parameters, result);
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Validation result for {}: {} errors, {} warnings", 
                toolName, result.getErrors().size(), result.getWarnings().size());
        }
        
        return result;
    }

    /**
     * Validates a single parameter value
     * @param name Parameter name
     * @param value Parameter value
     * @param expectedType Expected type description
     * @return ValidationResult for the parameter
     */
    public ValidationResult validateParameter(String name, Object value, String expectedType) {
        ValidationResult result = new ValidationResult();
        
        if (value == null) {
            result.addError(name, null, "Parameter cannot be null");
            return result;
        }
        
        // Type-specific validations
        switch (expectedType.toLowerCase()) {
            case "string" -> validateStringParameter(name, value, result);
            case "number", "integer" -> validateNumberParameter(name, value, result);
            case "boolean" -> validateBooleanParameter(name, value, result);
            case "array" -> validateArrayParameter(name, value, result);
            case "object" -> validateObjectParameter(name, value, result);
            case "email" -> validateEmailParameter(name, value, result);
            case "phone" -> validatePhoneParameter(name, value, result);
            case "date" -> validateDateParameter(name, value, result);
            default -> log.warn("Unknown expected type: {}", expectedType);
        }
        
        return result;
    }

    /**
     * Checks if a tool name is valid and supported
     * @param toolName Tool name to check
     * @return true if valid, false otherwise
     */
    public boolean isValidToolName(String toolName) {
        if (toolName == null || toolName.trim().isEmpty()) {
            return false;
        }
        
        // Check against known tool patterns
        return toolName.matches("^[a-z_]+_(create|get|update|delete|list|add|remove|search)$");
    }

    /**
     * Gets suggestions for invalid tool names
     * @param invalidToolName Invalid tool name
     * @param availableTools List of available tool names
     * @return Suggestion string
     */
    public String suggestToolName(String invalidToolName, List<String> availableTools) {
        if (availableTools == null || availableTools.isEmpty()) {
            return "No tools available";
        }
        
        return parameterFormatter.suggestParameterCorrection(invalidToolName, null, availableTools);
    }

    private void validateRequiredParameters(String toolName, Map<String, Object> parameters, ValidationResult result) {
        Set<String> required = REQUIRED_PARAMETERS.get(toolName);
        if (required == null) return;
        
        for (String requiredParam : required) {
            if (!parameters.containsKey(requiredParam)) {
                result.addError(requiredParam, null, 
                    String.format("Required parameter '%s' is missing", requiredParam));
            } else if (parameters.get(requiredParam) == null) {
                result.addError(requiredParam, null, 
                    String.format("Required parameter '%s' cannot be null", requiredParam));
            }
        }
    }

    private void validateParameterTypes(String toolName, Map<String, Object> parameters, ValidationResult result) {
        for (Map.Entry<String, Object> entry : parameters.entrySet()) {
            String paramName = entry.getKey();
            Object value = entry.getValue();
            
            if (value == null) continue;
            
            // Parameter-specific type validations
            switch (paramName) {
                case "summary", "description", "title", "body", "firstName", "lastName", "nickname" -> 
                    validateStringParameter(paramName, value, result);
                case "contactId", "activityTypeId", "duration", "page", "limit" -> 
                    validateNumberParameter(paramName, value, result);
                case "completed" -> 
                    validateBooleanParameter(paramName, value, result);
                case "attendees" -> 
                    validateAttendeesParameter(paramName, value, result);
                case "email" -> 
                    validateEmailParameter(paramName, value, result);
                case "phone" -> 
                    validatePhoneParameter(paramName, value, result);
                case "happenedAt", "triggerDate", "completedAt", "birthdate" -> 
                    validateDateParameter(paramName, value, result);
            }
        }
    }

    private void validateUnknownParameters(String toolName, Map<String, Object> parameters, ValidationResult result) {
        Set<String> allowedParams = new HashSet<>();
        allowedParams.addAll(REQUIRED_PARAMETERS.getOrDefault(toolName, Set.of()));
        allowedParams.addAll(OPTIONAL_PARAMETERS.getOrDefault(toolName, Set.of()));
        allowedParams.add("id"); // Always allowed for updates/gets/deletes
        
        for (String paramName : parameters.keySet()) {
            if (!allowedParams.contains(paramName)) {
                String suggestion = parameterFormatter.suggestParameterCorrection(
                    paramName, null, new ArrayList<>(allowedParams));
                result.addWarning(paramName, parameters.get(paramName), 
                    String.format("Unknown parameter '%s'. %s", paramName, suggestion));
            }
        }
    }

    private void performToolSpecificValidation(String toolName, Map<String, Object> parameters, ValidationResult result) {
        switch (toolName) {
            case "activity_create" -> validateActivityCreate(parameters, result);
            case "contact_create" -> validateContactCreate(parameters, result);
            case "note_create", "task_create" -> validateContactLinkedOperation(parameters, result);
            default -> {
                // Generic validation for list operations
                if (toolName.endsWith("_list")) {
                    validateListOperation(parameters, result);
                }
            }
        }
    }

    private void validateActivityCreate(Map<String, Object> parameters, ValidationResult result) {
        // Validate attendees using ParameterFormatter
        Object attendees = parameters.get("attendees");
        if (attendees != null) {
            try {
                parameterFormatter.normalizeAttendees(attendees);
            } catch (IllegalArgumentException e) {
                result.addError("attendees", attendees, e.getMessage());
            }
        }
        
        // Validate duration if present
        Object duration = parameters.get("duration");
        if (duration instanceof Number num) {
            if (num.intValue() < 0) {
                result.addError("duration", duration, "Duration cannot be negative");
            } else if (num.intValue() > 1440) { // 24 hours in minutes
                result.addWarning("duration", duration, "Duration exceeds 24 hours, please verify");
            }
        }
    }

    private void validateContactCreate(Map<String, Object> parameters, ValidationResult result) {
        // Validate name combination
        String firstName = (String) parameters.get("firstName");
        String lastName = (String) parameters.get("lastName");
        
        if (firstName != null && firstName.trim().isEmpty()) {
            result.addError("firstName", firstName, "First name cannot be empty");
        }
        
        if (lastName != null && lastName.trim().isEmpty()) {
            result.addError("lastName", lastName, "Last name cannot be empty");
        }
    }

    private void validateContactLinkedOperation(Map<String, Object> parameters, ValidationResult result) {
        // Ensure contactId is valid
        Object contactId = parameters.get("contactId");
        if (contactId instanceof Number num) {
            if (num.longValue() <= 0) {
                result.addError("contactId", contactId, "Contact ID must be positive");
            }
        }
    }

    private void validateListOperation(Map<String, Object> parameters, ValidationResult result) {
        // Validate pagination parameters
        Object page = parameters.get("page");
        if (page instanceof Number num && num.intValue() < 1) {
            result.addError("page", page, "Page number must be 1 or greater");
        }
        
        Object limit = parameters.get("limit");
        if (limit instanceof Number num) {
            if (num.intValue() < 1) {
                result.addError("limit", limit, "Limit must be 1 or greater");
            } else if (num.intValue() > 100) {
                result.addError("limit", limit, "Limit cannot exceed 100");
            }
        }
    }

    private void validateStringParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof String str)) {
            result.addError(name, value, "Must be a string");
            return;
        }
        
        if (str.trim().isEmpty()) {
            result.addError(name, value, "Cannot be empty");
        }
        
        if (str.length() > 1000) {
            result.addWarning(name, value, "Text is very long, consider shortening");
        }
    }

    private void validateNumberParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof Number)) {
            try {
                Long.parseLong(value.toString());
            } catch (NumberFormatException e) {
                result.addError(name, value, "Must be a number");
            }
        }
    }

    private void validateBooleanParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof Boolean)) {
            String str = value.toString().toLowerCase();
            if (!str.equals("true") && !str.equals("false")) {
                result.addError(name, value, "Must be true or false");
            }
        }
    }

    private void validateArrayParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof List)) {
            result.addError(name, value, "Must be an array");
        }
    }

    private void validateObjectParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof Map)) {
            result.addError(name, value, "Must be an object");
        }
    }

    private void validateEmailParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof String str)) {
            result.addError(name, value, "Email must be a string");
            return;
        }
        
        if (!EMAIL_PATTERN.matcher(str).matches()) {
            result.addError(name, value, "Invalid email format");
        }
    }

    private void validatePhoneParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof String str)) {
            result.addError(name, value, "Phone must be a string");
            return;
        }
        
        if (!PHONE_PATTERN.matcher(str.replaceAll("\\s|-|\\(|\\)", "")).matches()) {
            result.addError(name, value, "Invalid phone number format");
        }
    }

    private void validateDateParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof String str)) {
            result.addError(name, value, "Date must be a string in ISO 8601 format");
            return;
        }
        
        if (!ISO_DATE_PATTERN.matcher(str).matches()) {
            result.addError(name, value, "Date must be in ISO 8601 format (e.g., 2024-01-15T10:30:00Z)");
        }
    }

    private void validateAttendeesParameter(String name, Object value, ValidationResult result) {
        if (!(value instanceof List)) {
            result.addError(name, value, "Attendees must be an array");
            return;
        }
        
        @SuppressWarnings("unchecked")
        List<?> list = (List<?>) value;
        
        if (list.isEmpty()) {
            result.addError(name, value, "Attendees cannot be empty");
            return;
        }
        
        for (int i = 0; i < list.size(); i++) {
            Object attendee = list.get(i);
            String indexedName = name + "[" + i + "]";
            
            if (attendee instanceof String str) {
                if (str.trim().isEmpty()) {
                    result.addError(indexedName, attendee, "Attendee name cannot be empty");
                }
            } else if (attendee instanceof Map<?, ?> map) {
                if (!map.containsKey("contactId")) {
                    result.addError(indexedName, attendee, "Attendee object must contain 'contactId' field");
                }
            } else {
                result.addError(indexedName, attendee, 
                    "Attendee must be a string name or object with contactId");
            }
        }
    }

    /**
     * Holds validation results including errors and warnings
     */
    public static class ValidationResult {
        private final List<ValidationError> errors = new ArrayList<>();
        private final List<ValidationError> warnings = new ArrayList<>();
        
        public boolean isValid() {
            return errors.isEmpty();
        }
        
        public List<ValidationError> getErrors() {
            return errors;
        }
        
        public List<ValidationError> getWarnings() {
            return warnings;
        }
        
        public void addError(String parameter, Object value, String message) {
            errors.add(new ValidationError(parameter, value, message, false));
        }
        
        public void addWarning(String parameter, Object value, String message) {
            warnings.add(new ValidationError(parameter, value, message, true));
        }
        
        public String getErrorSummary() {
            if (errors.isEmpty()) return "No errors";
            
            return errors.stream()
                .map(ValidationError::toString)
                .reduce((a, b) -> a + "; " + b)
                .orElse("Unknown errors");
        }
    }

    /**
     * Represents a single validation error or warning
     */
    public static class ValidationError {
        private final String parameter;
        private final Object value;
        private final String message;
        private final boolean isWarning;
        
        public ValidationError(String parameter, Object value, String message, boolean isWarning) {
            this.parameter = parameter;
            this.value = value;
            this.message = message;
            this.isWarning = isWarning;
        }
        
        public String getParameter() { return parameter; }
        public Object getValue() { return value; }
        public String getMessage() { return message; }
        public boolean isWarning() { return isWarning; }
        
        @Override
        public String toString() {
            return String.format("%s '%s': %s", isWarning ? "Warning" : "Error", parameter, message);
        }
    }
}