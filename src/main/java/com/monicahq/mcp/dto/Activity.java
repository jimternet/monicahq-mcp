package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
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
public class Activity {
    
    @JsonProperty("id")
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotBlank(message = "Type is required")
    @Pattern(regexp = "^(call|meeting|email)$", message = "Type must be one of: call, meeting, email")
    @JsonProperty("type")
    private String type;
    
    @NotBlank(message = "Summary is required")
    @Size(max = 500, message = "Summary must not exceed 500 characters")
    @JsonProperty("summary")
    private String summary;
    
    @Size(max = 5000, message = "Description must not exceed 5000 characters")
    @JsonProperty("description")
    private String description;
    
    @NotNull(message = "Date is required")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("date")
    private LocalDateTime date;
    
    @Min(value = 0, message = "Duration must be non-negative")
    @Max(value = 1440, message = "Duration cannot exceed 1440 minutes")
    @JsonProperty("duration")
    private Integer duration;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}
