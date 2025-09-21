package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Debt {
    
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotNull(message = "Amount is required")
    private BigDecimal amount;
    
    @Size(max = 3, message = "Currency code must not exceed 3 characters")
    private String currency;
    
    @JsonProperty("in_debt")
    private String inDebt;
    
    @Size(max = 255, message = "Status must not exceed 255 characters")
    private String status;
    
    @Size(max = 1000, message = "Reason must not exceed 1000 characters")
    private String reason;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}