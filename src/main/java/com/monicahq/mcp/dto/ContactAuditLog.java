package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.*;
import java.time.LocalDateTime;
import java.util.Map;

/**
 * DTO representing contact audit log entries.
 * Maps to Monica API contact audit logs endpoint.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContactAuditLog {
    
    @JsonProperty("id")
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotBlank(message = "Action is required")
    @Size(max = 100, message = "Action must not exceed 100 characters")
    @JsonProperty("action")
    private String action;
    
    @Size(max = 255, message = "Description must not exceed 255 characters")
    @JsonProperty("description")
    private String description;
    
    @JsonProperty("author_id")
    private Long authorId;
    
    @JsonProperty("author_name")
    private String authorName;
    
    @JsonProperty("object_type")
    private String objectType;
    
    @JsonProperty("object_id")
    private Long objectId;
    
    @JsonProperty("object_name")
    private String objectName;
    
    @JsonProperty("changes")
    private Map<String, Object> changes;
    
    @JsonProperty("old_values")
    private Map<String, Object> oldValues;
    
    @JsonProperty("new_values")
    private Map<String, Object> newValues;
    
    @JsonProperty("ip_address")
    private String ipAddress;
    
    @JsonProperty("user_agent")
    private String userAgent;
    
    @JsonProperty("created_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime createdAt;
}