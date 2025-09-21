package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Currency {
    
    private Long id;
    
    @NotBlank(message = "Currency code is required")
    @Size(max = 3, message = "Currency code must not exceed 3 characters")
    private String code;
    
    @NotBlank(message = "Currency name is required")
    @Size(max = 255, message = "Currency name must not exceed 255 characters")
    private String name;
    
    @Size(max = 10, message = "Symbol must not exceed 10 characters")
    private String symbol;
    
    @JsonProperty("exchange_rate")
    private BigDecimal exchangeRate;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}