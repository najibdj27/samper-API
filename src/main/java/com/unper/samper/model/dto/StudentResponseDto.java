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
public class StudentResponseDto {
    private Long id;
    
    private String NIM;

    private UserResponseDto user;

    private ClassResponseDto kelas;

    private Boolean isLeader;

    private Boolean isActive;
}
