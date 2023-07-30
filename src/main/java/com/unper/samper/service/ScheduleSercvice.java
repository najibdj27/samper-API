package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.ScheduleResponseDto;

public interface ScheduleSercvice {
    List<ScheduleResponseDto> getAll();

    Schedule getById(Long id);

    Schedule add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException;

    List<Schedule> addAll(List<AddScheduleRequestDto> requestDto) throws ResourceNotFoundException;
}
