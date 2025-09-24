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
 * DTO representing a Monica user account.
 * Maps to Monica API Users entity.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class User {
    
    @JsonProperty("id")
    private Long id;
    
    @NotBlank(message = "First name is required")
    @Size(max = 255, message = "First name must not exceed 255 characters")
    @JsonProperty("first_name")
    private String firstName;
    
    @Size(max = 255, message = "Last name must not exceed 255 characters")
    @JsonProperty("last_name")
    private String lastName;
    
    @NotBlank(message = "Email is required")
    @Email(message = "Email must be valid")
    @Size(max = 255, message = "Email must not exceed 255 characters")
    @JsonProperty("email")
    private String email;
    
    @JsonProperty("timezone")
    private String timezone;
    
    @JsonProperty("locale")
    private String locale;
    
    @JsonProperty("currency")
    private String currency;
    
    @JsonProperty("is_administrator")
    private Boolean isAdministrator;
    
    @JsonProperty("profile_picture_url")
    private String profilePictureUrl;
    
    @JsonProperty("created_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime updatedAt;
}