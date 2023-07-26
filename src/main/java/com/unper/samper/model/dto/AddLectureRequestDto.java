package com.unper.samper.model.dto;

import com.unper.samper.model.User;

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
public class AddLectureRequestDto {
    private User user;

    private Long NIP;
}
