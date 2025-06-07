package com.unper.samper.model.dto;

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
public class RegistrationEligibilityRequestDto {
    private String username;

    private String email;

    private String phoneNumber;

    private String nim;

    private String nip;
}
