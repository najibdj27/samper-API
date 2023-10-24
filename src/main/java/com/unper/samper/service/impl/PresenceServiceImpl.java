package com.unper.samper.service.impl;

import java.time.LocalDateTime;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.PresenceCheckInRequestDto;
import com.unper.samper.model.dto.PresenceCheckOutRequestDto;
import com.unper.samper.repository.PresenceRepository;
import com.unper.samper.service.PresenceService;

@Service
public class PresenceServiceImpl implements PresenceService {
    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    PresenceRepository presenceRepository;

    @Override
    public List<Presence> getAllByLecture() throws ResourceNotFoundException {
        Lecture lecture = lectureServiceImpl.getCurrentLecture();
        List<Presence> presenceList = presenceRepository.findByLecture(lecture);
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
    public Presence checkIn(PresenceCheckInRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException {
        Schedule schedule = scheduleServiceImpl.getById(requestDto.getScheduleId());
        Student student = studentServiceImpl.getCurrentStudent();
        
        if (schedule.getKelas() != student.getKelas()) {
            throw new DifferentClassException(EResponseMessage.PRESENCE_DIFFERENT_CLASS.getMessage());
        }
        
        if (Boolean.FALSE.equals(schedule.getIsActive())) {
            throw new ScheduleNotActiveException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        if (Boolean.TRUE.equals(presenceRepository.isOnSchedule(student))) {
            throw new OnScheduleException(EResponseMessage.ON_SCHEDULE.getMessage());
        }
        Presence presence = Presence.builder()
            .student(student)
            .schedule(schedule)
            .checkIn(LocalDateTime.now())
            .student(student)
            .checkInLocation(requestDto.getCehckInLocation())
            .build();
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }

    @Override
    public Presence checkOut(PresenceCheckOutRequestDto requestDto) throws ScheduleNotActiveException, ResourceNotFoundException {
        Presence presence = getById(requestDto.getId());
        if (Boolean.FALSE.equals(presence.getSchedule().getIsActive())) {
            throw new ScheduleNotActiveException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        if (!presence.getCheckOut().equals(null) && !presence.getCheckOutLocation().equals(null)) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        presence.setCheckOut(LocalDateTime.now());
        presence.setCheckOutLocation(requestDto.getCheckOutLocation());
        
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        presenceRepository.deleteById(id);
    }
    
}
