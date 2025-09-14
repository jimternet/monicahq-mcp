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
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Contact {
    
    @JsonProperty("id")
    private Long id;
    
    @NotBlank(message = "First name is required")
    @Size(max = 255, message = "First name must not exceed 255 characters")
    @JsonProperty("first_name")
    private String firstName;
    
    @Size(max = 255, message = "Last name must not exceed 255 characters")
    @JsonProperty("last_name")
    private String lastName;
    
    @Size(max = 255, message = "Nickname must not exceed 255 characters")
    @JsonProperty("nickname")
    private String nickname;
    
    @NotNull(message = "Gender ID is required")
    @JsonProperty("gender_id")
    private Long genderId;
    
    @NotNull(message = "Birthdate known flag is required")
    @JsonProperty("is_birthdate_known")
    private Boolean isBirthdateKnown;
    
    @NotNull(message = "Deceased flag is required")
    @JsonProperty("is_deceased")
    private Boolean isDeceased;
    
    @NotNull(message = "Deceased date known flag is required")
    @JsonProperty("is_deceased_date_known")
    private Boolean isDeceasedDateKnown;
    
    @Email(message = "Email must be valid")
    @JsonProperty("email")
    private String email;
    
    @JsonProperty("phone")
    private String phone;
    
    @Past(message = "Birthdate must be in the past")
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd")
    @JsonProperty("birthdate")
    private LocalDate birthdate;
    
    @Size(max = 255, message = "Company must not exceed 255 characters")
    @JsonProperty("company")
    private String company;
    
    @Size(max = 255, message = "Job title must not exceed 255 characters")
    @JsonProperty("job_title")
    private String jobTitle;
    
    @JsonProperty("tags")
    private List<Tag> tags;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("created_at")
    private LocalDateTime createdAt;
    
    @JsonFormat(shape = JsonFormat.Shape.STRING, pattern = "yyyy-MM-dd'T'HH:mm:ss'Z'")
    @JsonProperty("updated_at")
    private LocalDateTime updatedAt;
}
