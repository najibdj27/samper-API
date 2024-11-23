package com.unper.samper.model.dto;

import java.util.Date;
import java.util.Map;

import com.unper.samper.model.constant.ERequestType;
import com.unper.samper.model.constant.EStatus;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class RequestResponseDto {
    private Long id;

    private String reason;

    private Date requestTime;

    private Date approveTime;

    private Boolean isApproved;

    private ERequestType type;

    private EStatus status;

    private ScheduleResponseDto schedule;

    private UserResponseDto sender;

    private UserResponseDto receiver;

    private Map<String, Object> requsetData;
}
