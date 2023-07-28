package com.unper.samper.model.dto;

import java.util.Date;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonFormat;
import com.unper.samper.model.Role;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class UserResponseDto {
    private Long id;

    private String firstName;

    private String lastName;

    @JsonFormat(pattern = "dd-MM-yyyy")
    private Date dateOfBirth;
    
    private String username;

    private String email;

    private String phoneNumber;

    private List<Role> roles;
}
