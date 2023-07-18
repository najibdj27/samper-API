package com.unper.samper.model.dto;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
public class AddUserRequestDto {
    private String firstName;

    private String lastName;

    private Date dateOfBirth;
    
    private String username;

    private String email;

    private Integer phoneNumber;

    private String password;
}
