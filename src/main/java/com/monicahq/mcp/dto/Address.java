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
public class Address {
    
    private Long id;
    
    @JsonProperty("contact_id")
    private Long contactId;
    
    @Size(max = 255, message = "Name must not exceed 255 characters")
    private String name;
    
    @Size(max = 255, message = "Street must not exceed 255 characters")
    private String street;
    
    @Size(max = 255, message = "City must not exceed 255 characters")
    private String city;
    
    @Size(max = 255, message = "Province must not exceed 255 characters")
    private String province;
    
    @JsonProperty("postal_code")
    @Size(max = 255, message = "Postal code must not exceed 255 characters")
    private String postalCode;
    
    @Size(max = 3, message = "Country must not exceed 3 characters")
    private String country;
    
    private Double latitude;
    
    private Double longitude;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}