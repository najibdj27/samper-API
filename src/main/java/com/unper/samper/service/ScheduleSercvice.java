package com.unper.samper.service;

import java.text.ParseException;
import java.util.List;

import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.RescheduleRequestDto;

public interface ScheduleSercvice {
    List<Schedule> getAllByLecture(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException;

    List<Schedule> getAllByStudent(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException;

    List<Schedule> getScheduleMonthly(String dateStr, Long userId) throws ParseException, ResourceNotFoundException;

    Schedule getById(Long id) throws ResourceNotFoundException;

    List<Schedule> add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException, ParseException;

    Schedule edit(Schedule schedule) throws ResourceNotFoundException;

    Schedule activate(Long id) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException;

    Schedule deactivate(Long id) throws NoAccessException, ResourceNotFoundException;

    Schedule reschedule(RescheduleRequestDto requestDto) throws ResourceNotFoundException, ScheduleUnavailableException, NoAccessException;

    void delete(Long id) throws ResourceNotFoundException;
}
