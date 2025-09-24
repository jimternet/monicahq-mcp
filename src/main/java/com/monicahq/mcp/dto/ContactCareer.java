package com.monicahq.mcp.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import lombok.Builder;

import jakarta.validation.constraints.*;
import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * DTO representing contact career/work information.
 * Maps to Monica API contact work endpoint.
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class ContactCareer {
    
    @JsonProperty("id")
    private Long id;
    
    @NotNull(message = "Contact ID is required")
    @JsonProperty("contact_id")
    private Long contactId;
    
    @Size(max = 255, message = "Job title must not exceed 255 characters")
    @JsonProperty("job_title")
    private String jobTitle;
    
    @Size(max = 255, message = "Company name must not exceed 255 characters")
    @JsonProperty("company")
    private String company;
    
    @JsonProperty("company_id")
    private Long companyId;
    
    @Size(max = 500, message = "Description must not exceed 500 characters")
    @JsonProperty("description")
    private String description;
    
    @JsonProperty("salary")
    private String salary;
    
    @JsonProperty("salary_currency")
    private String salaryCurrency;
    
    @JsonProperty("start_date")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate startDate;
    
    @JsonProperty("end_date")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDate endDate;
    
    @JsonProperty("is_current")
    private Boolean isCurrent;
    
    @JsonProperty("location")
    private String location;
    
    @JsonProperty("employment_type")
    private String employmentType;
    
    @JsonProperty("created_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime createdAt;
    
    @JsonProperty("updated_at")
    @JsonFormat(pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    private LocalDateTime updatedAt;
}