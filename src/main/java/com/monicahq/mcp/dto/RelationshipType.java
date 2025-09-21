package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.*;
import java.time.LocalDateTime;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class RelationshipType {
    
    @JsonProperty("id")
    private Long id;
    
    @NotBlank(message = "Name is required")
    @Size(max = 255, message = "Name must not exceed 255 characters")
    @JsonProperty("name")
    private String name;
    
    @Size(max = 255, message = "Name reverse must not exceed 255 characters")
    @JsonProperty("name_reverse")
    private String nameReverse;
    
    @NotNull(message = "Relationship type group ID is required")
    @JsonProperty("relationship_type_group_id")
    private Long relationshipTypeGroupId;
    
    @JsonProperty("delible")
    private Boolean delible;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
    
    // Nested objects for expanded responses
    @JsonProperty("relationship_type_group")
    private RelationshipTypeGroup relationshipTypeGroup;
}