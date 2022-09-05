package com.unper.samper.domain.dto;

import java.util.List;

import lombok.*;

@Getter
@Setter
public class SignInResponseDTO {
    private String accessToken;

    private String type = "Bearer";

    private String username;
    
    private List<String> roles;
    
    public SignInResponseDTO(String accessToken, String username, List<String> roles) {
        this.accessToken = accessToken;
        this.username = username;
        this.roles = roles;
    }
}
