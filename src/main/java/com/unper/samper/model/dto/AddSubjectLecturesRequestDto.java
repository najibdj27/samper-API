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
public class AddSubjectLecturesRequestDto {
    private Long id;

    private List<Long> lectures;
}
