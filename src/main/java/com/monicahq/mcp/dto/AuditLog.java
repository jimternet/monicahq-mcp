package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class AuditLog {
    
    private Long id;
    
    @NotBlank(message = "Action is required")
    @Size(max = 255, message = "Action must not exceed 255 characters")
    private String action;
    
    @JsonProperty("auditable_type")
    @Size(max = 255, message = "Auditable type must not exceed 255 characters")
    private String auditableType;
    
    @JsonProperty("auditable_id")
    private Long auditableId;
    
    @JsonProperty("user_id")
    private Long userId;
    
    @JsonProperty("user_name")
    @Size(max = 255, message = "User name must not exceed 255 characters")
    private String userName;
    
    @JsonProperty("ip_address")
    @Size(max = 45, message = "IP address must not exceed 45 characters")
    private String ipAddress;
    
    @JsonProperty("user_agent")
    @Size(max = 1000, message = "User agent must not exceed 1000 characters")
    private String userAgent;
    
    @JsonProperty("old_values")
    private String oldValues;
    
    @JsonProperty("new_values")
    private String newValues;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
}