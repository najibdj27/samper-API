package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.exception.IllegalAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.ScheduleResponseDto;
import com.unper.samper.repository.ScheduleRepository;
import com.unper.samper.service.ScheduleSercvice;

@Service
public class ScheduleServiceImpl implements ScheduleSercvice {
    @Autowired
    ScheduleRepository scheduleRepository;

    @Autowired
    ClassServiceImpl classServiceImpl;

    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Override
    public List<ScheduleResponseDto> getAll() {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'getAll'");
    }

    @Override
    public Schedule getById(Long id) {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'getById'");
    }

    @Override
    public Schedule add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        Class kelas = classServiceImpl.getById(requestDto.getClassId());
        Subject subject = subjectServiceImpl.getById(requestDto.getSubjectId());
        // check if schedule on the time exist
        if (Boolean.TRUE.equals(scheduleRepository.existsByTime(kelas ,requestDto.getTimeStart(), requestDto.getTimeEnd()))) {
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }
        Schedule schedule = Schedule.builder()
            .kelas(kelas)
            .subject(subject)
            .isActive(Boolean.FALSE)
            .timeStart(requestDto.getTimeStart())
            .timeEnd(requestDto.getTimeEnd())
            .build();
        
        Schedule newSchedule = scheduleRepository.save(schedule);
        return newSchedule;
    }

    @Override
    @Transactional(rollbackFor = {ResourceNotFoundException.class, ResourceAlreadyExistException.class})
    public List<Schedule> addAll(List<AddScheduleRequestDto> requestDtoList) throws ResourceNotFoundException {
        List<Schedule> scheduleList = new ArrayList<>();
        requestDtoList.forEach(requestDto -> {
            Schedule schedule =  new Schedule();
            try {
                schedule = add(requestDto);
            } catch (ResourceNotFoundException e) {

            } catch (ResourceAlreadyExistException e) {

            }
            scheduleList.add(schedule);
        });
        
        List<Schedule> newSchedules = scheduleRepository.saveAll(scheduleList);
        return newSchedules;       
    }

    @Override
    public Schedule activate(Long id) throws ResourceNotFoundException, IllegalAccessException {
        Lecture lecture = lectureServiceImpl.getCurrentLecture();
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        schedule.setIsActive(Boolean.TRUE);
        if (schedule.getKelas().getLecture() != lecture) {
            throw new IllegalAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.TRUE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        Schedule activatedSchedule = scheduleRepository.save(schedule);
        return activatedSchedule;
    }

    @Override
    public Schedule deactivate(Long id) throws IllegalAccessException, ResourceNotFoundException {
        Lecture lecture = lectureServiceImpl.getCurrentLecture();
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        schedule.setIsActive(Boolean.FALSE);
        if (schedule.getKelas().getLecture() != lecture) {
            throw new IllegalAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.TRUE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        Schedule deactivatedSchedule = scheduleRepository.save(schedule);
        return deactivatedSchedule;
    }
}
