package com.monicahq.mcp.util;

import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Utility class for formatting and normalizing MCP tool parameters
 * Provides consistent parameter handling across all MCP operations
 */
@Component
@Slf4j
public class ParameterFormatter {

    private static final boolean DEBUG_MODE = Boolean.parseBoolean(System.getenv().getOrDefault("MCP_DEBUG", "false"));

    /**
     * Formats a parameter value for display in error messages or debug output
     * @param name Parameter name
     * @param value Parameter value
     * @return Formatted string representation
     */
    public String formatParameter(String name, Object value) {
        if (value == null) {
            return String.format("%s: null", name);
        }
        
        if (value instanceof String str) {
            if (str.length() > 50) {
                return String.format("%s: \"%s...\" (length: %d)", name, str.substring(0, 47), str.length());
            }
            return String.format("%s: \"%s\"", name, str);
        }
        
        if (value instanceof List<?> list) {
            return String.format("%s: [%d items] %s", name, list.size(), 
                list.isEmpty() ? "[]" : formatListPreview(list));
        }
        
        if (value instanceof Map<?, ?> map) {
            return String.format("%s: {%d fields} %s", name, map.size(),
                map.isEmpty() ? "{}" : formatMapPreview(map));
        }
        
        if (value instanceof Number || value instanceof Boolean) {
            return String.format("%s: %s", name, value);
        }
        
        String strValue = value.toString();
        if (strValue.length() > 50) {
            return String.format("%s: %s... (length: %d)", name, strValue.substring(0, 47), strValue.length());
        }
        
        return String.format("%s: %s", name, strValue);
    }

    /**
     * Formats all parameters in a map for debug logging
     * @param parameters Map of parameter names to values
     * @return Formatted string showing all parameters
     */
    public String formatParameterMap(Map<String, Object> parameters) {
        if (parameters == null || parameters.isEmpty()) {
            return "{}";
        }
        
        return parameters.entrySet().stream()
            .map(entry -> formatParameter(entry.getKey(), entry.getValue()))
            .collect(Collectors.joining(", ", "{", "}"));
    }

    /**
     * Normalizes attendees parameter to consistent format
     * Handles both string arrays and object arrays with contactId
     * @param attendees The attendees parameter value
     * @return Normalized list of attendee objects
     */
    @SuppressWarnings("unchecked")
    public List<Map<String, Object>> normalizeAttendees(Object attendees) {
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Normalizing attendees: {}", formatParameter("attendees", attendees));
        }
        
        if (!(attendees instanceof List)) {
            throw new IllegalArgumentException("attendees must be an array");
        }
        
        List<?> attendeeList = (List<?>) attendees;
        if (attendeeList.isEmpty()) {
            throw new IllegalArgumentException("attendees cannot be empty");
        }
        
        List<Map<String, Object>> normalized = new ArrayList<>();
        
        for (Object attendee : attendeeList) {
            Map<String, Object> normalizedAttendee = new HashMap<>();
            
            if (attendee instanceof Map) {
                Map<String, Object> attendeeMap = (Map<String, Object>) attendee;
                if (attendeeMap.containsKey("contactId")) {
                    normalizedAttendee.put("contactId", attendeeMap.get("contactId"));
                    // Copy other properties
                    attendeeMap.forEach((k, v) -> {
                        if (!"contactId".equals(k)) {
                            normalizedAttendee.put(k, v);
                        }
                    });
                } else {
                    throw new IllegalArgumentException("Invalid attendees format: object must contain 'contactId' field");
                }
            } else if (attendee instanceof String str) {
                if (str.trim().isEmpty()) {
                    throw new IllegalArgumentException("Invalid attendees format: attendee name cannot be empty");
                }
                normalizedAttendee.put("name", str.trim());
            } else if (attendee instanceof Number || attendee instanceof Boolean) {
                normalizedAttendee.put("name", attendee.toString());
            } else {
                throw new IllegalArgumentException("Invalid attendees format: attendee must be a string or object with contactId, got: " + attendee.getClass().getSimpleName());
            }
            
            normalized.add(normalizedAttendee);
        }
        
        if (DEBUG_MODE) {
            log.debug("[MCP-DEBUG] Normalized {} attendees", normalized.size());
        }
        
        return normalized;
    }

    /**
     * Formats parameter validation errors with helpful context
     * @param paramName Parameter name
     * @param paramValue Parameter value
     * @param expectedType Expected type description
     * @param actualIssue Description of the actual issue
     * @return Formatted error message
     */
    public String formatValidationError(String paramName, Object paramValue, String expectedType, String actualIssue) {
        StringBuilder error = new StringBuilder();
        error.append("Invalid parameter '").append(paramName).append("': ");
        error.append(actualIssue);
        
        if (expectedType != null && !expectedType.isEmpty()) {
            error.append(" (expected: ").append(expectedType).append(")");
        }
        
        if (paramValue != null) {
            error.append(" (received: ").append(formatParameter("", paramValue).substring(2)).append(")");
        }
        
        return error.toString();
    }

    /**
     * Suggests parameter corrections based on common mistakes
     * @param paramName Parameter name
     * @param paramValue Parameter value
     * @param availableParams List of valid parameter names
     * @return Helpful suggestion string
     */
    public String suggestParameterCorrection(String paramName, Object paramValue, List<String> availableParams) {
        if (availableParams == null || availableParams.isEmpty()) {
            return "No parameter suggestions available";
        }
        
        // Find close matches using simple string distance
        List<String> suggestions = availableParams.stream()
            .filter(valid -> calculateSimilarity(paramName, valid) > 0.5)
            .sorted((a, b) -> Double.compare(calculateSimilarity(paramName, b), calculateSimilarity(paramName, a)))
            .limit(3)
            .toList();
        
        if (suggestions.isEmpty()) {
            return String.format("Valid parameters are: %s", String.join(", ", availableParams));
        }
        
        return String.format("Did you mean: %s? Valid parameters are: %s", 
            String.join(", ", suggestions), String.join(", ", availableParams));
    }

    private String formatListPreview(List<?> list) {
        if (list.size() <= 3) {
            return list.toString();
        }
        
        List<String> preview = list.stream()
            .limit(3)
            .map(Object::toString)
            .toList();
        
        return String.format("[%s, ... +%d more]", String.join(", ", preview), list.size() - 3);
    }

    private String formatMapPreview(Map<?, ?> map) {
        if (map.size() <= 3) {
            return map.toString();
        }
        
        List<String> preview = map.entrySet().stream()
            .limit(3)
            .map(entry -> entry.getKey() + "=" + entry.getValue())
            .toList();
        
        return String.format("{%s, ... +%d more}", String.join(", ", preview), map.size() - 3);
    }

    private double calculateSimilarity(String str1, String str2) {
        if (str1 == null || str2 == null) return 0.0;
        if (str1.equals(str2)) return 1.0;
        
        int maxLength = Math.max(str1.length(), str2.length());
        if (maxLength == 0) return 1.0;
        
        return 1.0 - (double) calculateLevenshteinDistance(str1, str2) / maxLength;
    }

    private int calculateLevenshteinDistance(String str1, String str2) {
        int[][] dp = new int[str1.length() + 1][str2.length() + 1];
        
        for (int i = 0; i <= str1.length(); i++) {
            dp[i][0] = i;
        }
        
        for (int j = 0; j <= str2.length(); j++) {
            dp[0][j] = j;
        }
        
        for (int i = 1; i <= str1.length(); i++) {
            for (int j = 1; j <= str2.length(); j++) {
                int cost = str1.charAt(i - 1) == str2.charAt(j - 1) ? 0 : 1;
                dp[i][j] = Math.min(Math.min(dp[i - 1][j] + 1, dp[i][j - 1] + 1), dp[i - 1][j - 1] + cost);
            }
        }
        
        return dp[str1.length()][str2.length()];
    }
}