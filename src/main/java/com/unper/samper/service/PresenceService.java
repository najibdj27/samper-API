package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.model.Presence;
import com.unper.samper.model.dto.PresenceCheckInRequestDto;
import com.unper.samper.model.dto.PresenceCheckOutRequestDto;

public interface PresenceService {
    List<Presence> getAllByLecture() throws ResourceNotFoundException;

    Presence getById(Long id) throws ResourceNotFoundException;

    Presence checkIn(PresenceCheckInRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException;

    Presence checkOut(PresenceCheckOutRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException;

    void delete(Long id) throws ResourceNotFoundException;
}
