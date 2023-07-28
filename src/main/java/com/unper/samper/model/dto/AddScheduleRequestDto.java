package com.unper.samper.model.dto;

import java.util.Calendar;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AddScheduleRequestDto {
    private Long classId;

    private Long subjectId;

    @JsonFormat(pattern = "yyyy-MM-dd hh:mm")
    private Calendar timeStart;

    @JsonFormat(pattern = "yyyy-MM-dd hh:mm")
    private Calendar timeEnd;

}
