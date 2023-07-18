package com.unper.samper.model.dto;

import java.util.Date;

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
public class EditUserRequestDto {

    private String firstName;
    
    private String lastName;
    
    private Date dateOfBirth;
}
