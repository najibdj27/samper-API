package com.unper.samper.model.dto;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.Size;

import lombok.*;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder

public class SignInRequestDto {
    @NotBlank
    @Size(min = 3, max = 20)
    @Pattern(regexp = "(?![_.]).+", message = "can't contain '_' at the beginning")
    @Pattern(regexp = "(?!.*[_.]{2}).+", message = "can't contain double '_'")
    @Pattern(regexp = "[a-zA-Z0-9._]+(?<![.])", message = "can only contain letters, numbers and underscore")
    private String usernameOrEmail;
    
    @NotBlank
    private String password;
}
