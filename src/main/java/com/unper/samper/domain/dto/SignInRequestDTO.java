package com.unper.samper.domain.dto;

import javax.validation.constraints.*;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class SignInRequestDTO {
    @NotBlank
    @Size(min = 1, max = 20)
    @Pattern(regexp = "^(?=.{8,20}$)(?![_.])(?!.*[_.]{2})[a-zA-Z0-9._]+(?<![.])$")
    private String username;

    @Pattern(regexp = "^(?=.*[A-Z].*[A-Z])(?=.*[!@#$&*])(?=.*[0-9].*[0-9])(?=.*[a-z].*[a-z].*[a-z]).{8}$")
    private String password;
}
