package com.unper.samper.model.dto;

import java.util.Date;
import java.util.Set;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RegisterAdminRequestDto {
    private String nip;

    private Set<Integer> previllages; 

    private String firstName;

    private String lastName;

    @JsonFormat(pattern = "dd-MM-yyyy")
    private Date dateOfBirth;
    
    private String username;

    private String email;

    private String phoneNumber;

    private String password;
}
