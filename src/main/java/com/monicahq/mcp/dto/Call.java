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
public class Call {
    
    @JsonProperty("id")
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotNull(message = "Called at timestamp is required")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("called_at")
    private LocalDateTime calledAt;
    
    @Min(value = 0, message = "Duration must be non-negative")
    @Max(value = 1440, message = "Duration cannot exceed 1440 minutes")
    @JsonProperty("duration")
    private Integer duration;
    
    @NotBlank(message = "Type is required")
    @Pattern(regexp = "^(incoming|outgoing)$", message = "Type must be either 'incoming' or 'outgoing'")
    @JsonProperty("type")
    private String type;
    
    @Size(max = 5000, message = "Content must not exceed 5000 characters")
    @JsonProperty("content")
    private String content;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}
