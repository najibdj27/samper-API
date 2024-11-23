package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.OutScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.model.Presence;
import com.unper.samper.model.dto.PresenceRecordRequestDto;

public interface PresenceService {
    List<Presence> getAllByLecture() throws ResourceNotFoundException;

    Presence getById(Long id) throws ResourceNotFoundException;

    List<Presence> findByStudent(Long studentId, Integer limit) throws ResourceNotFoundException;

    Presence checkIn(PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException;

    Presence checkOut(PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException, DifferentClassException, OutScheduleException, ActivityNotAllowedException;

    void delete(Long id) throws ResourceNotFoundException;
}
