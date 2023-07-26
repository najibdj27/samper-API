package com.unper.samper.model.dto;

import com.unper.samper.model.User;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import com.unper.samper.model.Class;

@AllArgsConstructor
@NoArgsConstructor
@Getter
@Setter
@Builder
public class AddStudentRequestDto {
    private String NIM;

    private User user;

    private Class kelas;
}
