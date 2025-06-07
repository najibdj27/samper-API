package com.unper.samper.model.dto;

import lombok.*;

import javax.validation.constraints.Email;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class ConfirmOTPRequestDto {
    @NotBlank
    @Size(max = 50)
    @Email
    private String key;

    @NotNull
    private int otp;
}
