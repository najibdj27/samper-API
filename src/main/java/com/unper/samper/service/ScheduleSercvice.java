package com.unper.samper.service;

import java.text.ParseException;
import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.FaceNotMatchedException;
import com.unper.samper.exception.GeolocationException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.dto.ActionScheduleRequestDto;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.RescheduleRequestDto;

public interface ScheduleSercvice {
    List<Schedule> getAllByLecture(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException;

    List<Schedule> getAllByStudent(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException;

    List<Schedule> getScheduleMonthly(String dateStr, Long userId) throws ParseException, ResourceNotFoundException;

    Schedule getById(Long id) throws ResourceNotFoundException;

    List<Schedule> add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException, ParseException;

    Schedule edit(Schedule schedule) throws ResourceNotFoundException;

    Schedule activate(ActionScheduleRequestDto requestDto) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException;

    Schedule deactivate(ActionScheduleRequestDto requestDto) throws NoAccessException, ResourceNotFoundException, GeolocationException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException;

    Schedule reschedule(RescheduleRequestDto requestDto) throws ResourceNotFoundException, ScheduleUnavailableException, NoAccessException;

    void delete(Long id) throws ResourceNotFoundException;
}
