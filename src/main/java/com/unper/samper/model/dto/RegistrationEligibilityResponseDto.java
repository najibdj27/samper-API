package com.unper.samper.model.dto;

import java.util.Map;

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
public class RegistrationEligibilityResponseDto {
    private char eligibilityStatus;

    private Map<String, Map<String, String>> field;
}
