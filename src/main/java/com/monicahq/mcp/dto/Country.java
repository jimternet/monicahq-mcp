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
public class Country {
    
    private Long id;
    
    @NotBlank(message = "Country name is required")
    @Size(max = 255, message = "Country name must not exceed 255 characters")
    private String name;
    
    @JsonProperty("iso")
    @Size(max = 3, message = "ISO code must not exceed 3 characters")
    private String iso;
    
    @JsonProperty("country_code")
    @Size(max = 10, message = "Country code must not exceed 10 characters")
    private String countryCode;
    
    @Size(max = 50, message = "Timezone must not exceed 50 characters")
    private String timezone;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}