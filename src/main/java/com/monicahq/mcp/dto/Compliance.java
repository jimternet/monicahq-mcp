package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

/**
 * DTO representing compliance information.
 * Maps to Monica API Compliance entity (if available).
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Compliance {
    
    @JsonProperty("id")
    private Long id;
    
    @JsonProperty("contact_id")
    private Long contactId;
    
    @Size(max = 100, message = "Compliance type must not exceed 100 characters")
    @JsonProperty("type")
    private String type;
    
    @Size(max = 500, message = "Description must not exceed 500 characters")
    @JsonProperty("description")
    private String description;
    
    @JsonProperty("is_active")
    private Boolean isActive;
    
    @JsonProperty("data_retention_days")
    private Integer dataRetentionDays;
    
    @JsonProperty("privacy_level")
    private String privacyLevel;
    
    @JsonProperty("consent_given")
    private Boolean consentGiven;
    
    @JsonProperty("consent_date")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime consentDate;
    
    @JsonProperty("audit_required")
    private Boolean auditRequired;
    
    @JsonProperty("created_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime updatedAt;
}