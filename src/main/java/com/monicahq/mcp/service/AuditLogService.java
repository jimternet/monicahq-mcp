package com.monicahq.mcp.service;

import com.monicahq.mcp.client.MonicaHqClient;
import com.monicahq.mcp.util.ContentFormatter;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Mono;

import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class AuditLogService {

    private final MonicaHqClient monicaClient;
    private final ContentFormatter contentFormatter;

    public Mono<Map<String, Object>> getAuditLog(Map<String, Object> arguments) {
        log.info("Getting audit log with arguments: {}", arguments);
        
        try {
            Long auditLogId = extractAuditLogId(arguments);
            
            return monicaClient.get("/auditlogs/" + auditLogId, null)
                .map(this::formatAuditLogResponse)
                .doOnSuccess(result -> log.info("Audit log retrieved successfully: {}", auditLogId))
                .doOnError(error -> log.error("Failed to get audit log {}: {}", auditLogId, error.getMessage()));
                
        } catch (IllegalArgumentException e) {
            log.error("Invalid arguments for audit log retrieval: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> listAuditLogs(Map<String, Object> arguments) {
        log.info("Listing audit logs with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildListQueryParams(arguments);
            
            return monicaClient.get("/auditlogs", queryParams)
                .map(this::formatAuditLogsListResponse)
                .doOnSuccess(result -> log.info("Audit logs listed successfully"))
                .doOnError(error -> log.error("Failed to list audit logs: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error listing audit logs: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    public Mono<Map<String, Object>> searchAuditLogs(Map<String, Object> arguments) {
        log.info("Searching audit logs with arguments: {}", arguments);
        
        try {
            Map<String, String> queryParams = buildSearchQueryParams(arguments);
            
            return monicaClient.get("/auditlogs", queryParams)
                .map(this::formatAuditLogsListResponse)
                .doOnSuccess(result -> log.info("Audit logs searched successfully"))
                .doOnError(error -> log.error("Failed to search audit logs: {}", error.getMessage()));
                
        } catch (Exception e) {
            log.error("Error searching audit logs: {}", e.getMessage());
            return Mono.error(e);
        }
    }

    private Long extractAuditLogId(Map<String, Object> arguments) {
        Object idObj = arguments.get("id");
        if (idObj == null) {
            throw new IllegalArgumentException("id is required");
        }
        
        if (idObj instanceof Number) {
            return ((Number) idObj).longValue();
        }
        
        try {
            return Long.parseLong(idObj.toString());
        } catch (NumberFormatException e) {
            throw new IllegalArgumentException("id must be a valid number");
        }
    }

    private Map<String, String> buildListQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = new HashMap<>();

        // Handle null arguments by using defaults
        if (arguments == null) {
            queryParams.put("limit", "25");
            queryParams.put("page", "1");
            return queryParams;
        }

        if (arguments.containsKey("limit")) {
            queryParams.put("limit", arguments.get("limit").toString());
        } else {
            queryParams.put("limit", "25");
        }

        if (arguments.containsKey("page")) {
            queryParams.put("page", arguments.get("page").toString());
        } else {
            queryParams.put("page", "1");
        }

        return queryParams;
    }

    private Map<String, String> buildSearchQueryParams(Map<String, Object> arguments) {
        Map<String, String> queryParams = buildListQueryParams(arguments);

        // Handle null arguments - return only defaults from buildListQueryParams
        if (arguments == null) {
            return queryParams;
        }

        if (arguments.containsKey("action")) {
            queryParams.put("action", arguments.get("action").toString());
        }
        if (arguments.containsKey("auditableType")) {
            queryParams.put("auditable_type", arguments.get("auditableType").toString());
        }
        if (arguments.containsKey("userId")) {
            queryParams.put("user_id", arguments.get("userId").toString());
        }

        return queryParams;
    }

    private Map<String, Object> formatAuditLogResponse(Map<String, Object> apiResponse) {
        Map<String, Object> rawApiData;
        Map<String, Object> auditLogData;
        
        if (apiResponse.containsKey("data")) {
            @SuppressWarnings("unchecked")
            Map<String, Object> rawData = (Map<String, Object>) apiResponse.get("data");
            rawApiData = rawData;
            auditLogData = mapFromApiFormat(rawData);
        } else {
            rawApiData = apiResponse;
            auditLogData = mapFromApiFormat(apiResponse);
        }
        
        String formattedContent = contentFormatter.formatAsEscapedJson(rawApiData);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", auditLogData);
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> formatAuditLogsListResponse(Map<String, Object> apiResponse) {
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> auditLogs = (List<Map<String, Object>>) apiResponse.get("data");
        
        List<Map<String, Object>> formattedAuditLogs = auditLogs.stream()
            .map(this::mapFromApiFormat)
            .toList();
        
        String formattedContent = contentFormatter.formatListAsEscapedJson(apiResponse);
        
        Map<String, Object> result = new HashMap<>();
        result.put("data", formattedAuditLogs);
        
        @SuppressWarnings("unchecked")
        Map<String, Object> meta = (Map<String, Object>) apiResponse.get("meta");
        if (meta != null) {
            result.put("meta", meta);
        }
        
        List<Map<String, Object>> content = List.of(
            Map.of(
                "type", "text",
                "text", formattedContent
            )
        );
        result.put("content", content);
        
        return result;
    }

    private Map<String, Object> mapFromApiFormat(Map<String, Object> apiData) {
        Map<String, Object> result = new HashMap<>();
        
        apiData.forEach((key, value) -> {
            switch (key) {
                case "auditable_type" -> result.put("auditableType", value);
                case "auditable_id" -> result.put("auditableId", value);
                case "user_id" -> result.put("userId", value);
                case "user_name" -> result.put("userName", value);
                case "ip_address" -> result.put("ipAddress", value);
                case "user_agent" -> result.put("userAgent", value);
                case "old_values" -> result.put("oldValues", value);
                case "new_values" -> result.put("newValues", value);
                case "created_at" -> result.put("createdAt", value);
                default -> result.put(key, value);
            }
        });
        
        return result;
    }
}