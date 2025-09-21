package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;
import java.time.LocalDateTime;

@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class Photo {
    
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @NotBlank(message = "Filename is required")
    @Size(max = 255, message = "Filename must not exceed 255 characters")
    private String filename;
    
    @JsonProperty("original_filename")
    @Size(max = 255, message = "Original filename must not exceed 255 characters")
    private String originalFilename;
    
    private Integer width;
    
    private Integer height;
    
    private Long filesize;
    
    @JsonProperty("mime_type")
    @Size(max = 100, message = "MIME type must not exceed 100 characters")
    private String mimeType;
    
    @JsonProperty("link")
    private String link;
    
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}