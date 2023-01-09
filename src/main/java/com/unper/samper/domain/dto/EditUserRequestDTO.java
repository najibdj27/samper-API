package com.unper.samper.domain.dto;

import java.util.Date;

import javax.validation.constraints.NotBlank;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class EditUserRequestDTO {

    @NotBlank
    private String firstName;

    private String lastName;

    @JsonFormat(pattern = "dd/MM/yyyy")
    private Date dateOfBirth;

    @NotBlank
    private String email;

    @NotBlank
    private String phoneNumber;

}
