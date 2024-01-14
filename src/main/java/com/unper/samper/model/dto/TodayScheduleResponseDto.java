package com.unper.samper.model.dto;

import java.util.Date;

import com.fasterxml.jackson.annotation.JsonFormat;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class TodayScheduleResponseDto {
    private Long id;

    private String lectureName;

    private String subjectName;

    @JsonFormat(pattern = "DD MMMM YYYY")
    private Date date;

    @JsonFormat(pattern = "HH:mm")
    private Date timeStart;

    @JsonFormat(pattern = "HH:mm")
    private Date timeEnd;
    
    @JsonFormat(pattern = "HH:mm")
    private Date clockIn;
    
    @JsonFormat(pattern = "HH:mm")
    private Date clockOut;

    private Boolean isActive;
}
