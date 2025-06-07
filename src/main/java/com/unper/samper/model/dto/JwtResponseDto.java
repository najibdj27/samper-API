package com.unper.samper.model.dto;


import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.util.List;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class JwtResponseDto {
    private String accessToken;

    private String refreshToken;

    @Builder.Default
    private String type = "Bearer";
    
    private Long userId;

    private List<String> roles;

}
