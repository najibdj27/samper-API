package com.unper.samper.model.dto;

import java.util.List;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AdminResponseDto {
    private Long id;

    private UserResponseDto user;

    private String NIP;

    private List<GetAllPrevillageByAdminResponseDto> previllages;
}
