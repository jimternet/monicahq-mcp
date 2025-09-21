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
public class Gift {
    
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotBlank(message = "Gift name is required")
    @Size(max = 255, message = "Gift name must not exceed 255 characters")
    private String name;
    
    @Size(max = 1000, message = "Comment must not exceed 1000 characters")
    private String comment;
    
    @Size(max = 255, message = "URL must not exceed 255 characters")
    private String url;
    
    private BigDecimal value;
    
    @JsonProperty("value_in_base_currency")
    private BigDecimal valueInBaseCurrency;
    
    @Size(max = 255, message = "Status must not exceed 255 characters")
    private String status;
    
    @JsonProperty("date")
    private String date;
    
    @JsonProperty("is_for")
    private String isFor;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}