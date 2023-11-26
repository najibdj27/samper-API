package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.RescheduleRequestDto;

public interface ScheduleSercvice {
    List<Schedule> getAll() throws ResourceNotFoundException;

    Schedule getById(Long id) throws ResourceNotFoundException;

    Schedule add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException;

    List<Schedule> addAll(List<AddScheduleRequestDto> requestDto) throws ResourceNotFoundException;

    Schedule activate(Long id) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException;

    Schedule deactivate(Long id) throws NoAccessException, ResourceNotFoundException;

    Schedule reschedule(RescheduleRequestDto requestDto) throws ResourceNotFoundException, ScheduleUnavailableException, NoAccessException;

    void delete(Long id) throws ResourceNotFoundException;
}
