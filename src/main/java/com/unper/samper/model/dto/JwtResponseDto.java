package com.unper.samper.model.dto;


import lombok.Getter;
import lombok.Setter;

import java.util.List;

@Getter
@Setter
public class JwtResponseDto {
    private String accessToken;
    private String refreshToken;
    private String type = "Bearer";
    private Long userId;
    private List<String> roles;
    public JwtResponseDto(String accessToken, String refreshToken, Long userId, List<String> roles) {
        this.accessToken = accessToken;
        this.refreshToken = refreshToken;
        this.userId = userId;
        this.roles = roles;
    }
}
