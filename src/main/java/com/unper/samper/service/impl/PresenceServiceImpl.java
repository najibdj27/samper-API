package com.unper.samper.service.impl;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.PresenceCheckInRequestDto;
import com.unper.samper.model.dto.PresenceCheckOutRequestDto;
import com.unper.samper.repository.PresenceRepository;
import com.unper.samper.service.PresenceService;

public class PresenceServiceImpl implements PresenceService {
    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    PresenceRepository presenceRepository;

    @Override
    public List<Presence> getAll() throws ResourceNotFoundException {
        List<Presence> presenceList = presenceRepository.findAll();
        if (presenceList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return presenceList;
    }

    @Override
    public Presence getById(Long id) throws ResourceNotFoundException {
        Presence presence = presenceRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return presence;
    }

    @Override
    public Presence checkIn(PresenceCheckInRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException {
        User user = authenticationServiceImpl.getCurrentUser();
        Schedule schedule = scheduleServiceImpl.getById(requestDto.getScheduleId());
        Student student = studentServiceImpl.getByUser(user);

        if (schedule.getKelas() != student.getKelas()) {
            throw new DifferentClassException(EResponseMessage.PRESENCE_DIFFERENT_CLASS.getMessage());
        }
        Presence presence = Presence.builder()
            .schedule(schedule)
            .checkIn(LocalDateTime.now())
            .student(student)
            .checkInLocation(requestDto.getLocation())
            .build();
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }

    @Override
    public Presence checkOut(PresenceCheckOutRequestDto requestDto) throws ResourceNotFoundException {
        getById(requestDto.getId());
        Presence presence = Presence.builder()
            .id(requestDto.getId())
            .checkOut(LocalDateTime.now())
            .checkOutLocation(requestDto.getLocation())
            .build();
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }
    
}
