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
public class Pet {

    @JsonProperty("id")
    private Long id;

    @JsonProperty("uuid")
    private String uuid;

    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;

    @NotNull(message = "Pet category ID is required")
    @JsonProperty("pet_category_id")
    private Long petCategoryId;

    @Size(max = 255, message = "Name must not exceed 255 characters")
    @JsonProperty("name")
    private String name;

    @JsonProperty("created_at")
    private LocalDateTime createdAt;

    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;

    // Nested objects for expanded responses
    @JsonProperty("pet_category")
    private PetCategory petCategory;

    @JsonProperty("contact")
    private Contact contact;
}
