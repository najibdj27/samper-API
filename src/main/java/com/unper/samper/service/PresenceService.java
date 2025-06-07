package com.unper.samper.service;

import java.util.List;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.FaceNotMatchedException;
import com.unper.samper.exception.GeolocationException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.OutScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.dto.PresenceRecordRequestDto;

public interface PresenceService {
    List<Presence> getAllByLecture() throws ResourceNotFoundException;

    Presence getById(Long id) throws ResourceNotFoundException;

    Presence getByCurrentStudentAndScheduleAndType(Schedule shedule, Character Type) throws ResourceNotFoundException;
    
    List<Presence> findByStudent(Long studentId, Integer limit) throws ResourceNotFoundException;

    Presence checkIn(PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException, GeolocationException;

    Presence checkOut(PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException, DifferentClassException, OutScheduleException, ActivityNotAllowedException, FaceNotMatchedException, ExternalAPIException, JsonMappingException, JsonProcessingException, GeolocationException;

    void delete(Long id) throws ResourceNotFoundException;
}
