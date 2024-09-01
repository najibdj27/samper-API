package com.unper.samper.model.dto;

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

    private Long lectureId;

    @JsonFormat(pattern = "YYYY-MM-DD")
    private String dateStart;
    
    @JsonFormat(pattern = "HH:mm")
    private String timeStart;
    
    private Short creditAmount;

    private Integer numberOfMeetings;

}
