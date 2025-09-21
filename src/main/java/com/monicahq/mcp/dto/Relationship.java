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
public class Relationship {
    
    @JsonProperty("id")
    private Long id;
    
    @NotNull(message = "Contact A is required")
    @JsonProperty("contact_is")
    private Long contactIs;
    
    @NotNull(message = "Contact B (of contact) is required")
    @JsonProperty("of_contact")
    private Long ofContact;
    
    @NotNull(message = "Relationship type ID is required")
    @JsonProperty("relationship_type_id")
    private Long relationshipTypeId;
    
    @Size(max = 1000, message = "Notes must not exceed 1000 characters")
    @JsonProperty("notes")
    private String notes;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
    
    // Nested objects for expanded responses
    @JsonProperty("contact_a")
    private Contact contactA;
    
    @JsonProperty("contact_b")
    private Contact contactB;
    
    @JsonProperty("relationship_type")
    private RelationshipType relationshipType;
}