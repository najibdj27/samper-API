package com.unper.samper.service.impl;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;
import com.unper.samper.model.Subject;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.RescheduleRequestDto;
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

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    UserServiceImpl userServiceImpl;

    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Override
    public List<Schedule> getAll(LocalDate filterDateFrom, LocalDate filterDateTo, Long classId) throws ResourceNotFoundException {
        String dateFrom = filterDateFrom.toString();
        String dateTo = filterDateTo.toString();
        List<Schedule> scheduleList = scheduleRepository.findAllWithFilter(dateFrom, dateTo, classId);
        if (scheduleList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return scheduleList;
    }

    @Override
    public List<Schedule> getAllByUserClass(String filterDateFrom, String filterDateTo, Long userId) throws ResourceNotFoundException {
        User user = userServiceImpl.getById(userId);
        Student student = studentServiceImpl.getByUser(user);
        Long classId = student.getKelas().getId();
        List<Schedule> scheduleList = scheduleRepository.findAllWithFilter(filterDateFrom, filterDateTo, classId);
        if (scheduleList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return scheduleList;
    }

    @Override
    public Schedule getById(Long id) throws ResourceNotFoundException {
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return schedule;
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
    public Schedule activate(Long id) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException {
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        Lecture lecture = schedule.getKelas().getLecture();
        Lecture currentLecture = lectureServiceImpl.getCurrentLecture();
        if (!currentLecture.equals(lecture)) {
            throw new NoAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.FALSE == scheduleRepository.isAvailable(id)) {
            throw new ScheduleUnavailableException(EResponseMessage.SCHEDULE_UNAVAILABLE.getMessage());
        }
        if (Boolean.TRUE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        schedule.setIsActive(Boolean.TRUE);
        Schedule activatedSchedule = scheduleRepository.save(schedule);
        
        return activatedSchedule;
    }

    @Override
    public Schedule deactivate(Long id) throws NoAccessException, ResourceNotFoundException {
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        Lecture lecture = schedule.getKelas().getLecture();
        Lecture currentLecture = lectureServiceImpl.getCurrentLecture();
        if (!currentLecture.equals(lecture)) {
            throw new NoAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.FALSE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        schedule.setIsActive(Boolean.FALSE);
        Schedule deactivatedSchedule = scheduleRepository.save(schedule);
        return deactivatedSchedule;
    }

    @Override
    public Schedule reschedule(RescheduleRequestDto requestDto) throws ResourceNotFoundException, ScheduleUnavailableException, NoAccessException {
        Schedule schedule = getById(requestDto.getId());
        Lecture lecture = schedule.getKelas().getLecture();
        Lecture currentLecture = lectureServiceImpl.getCurrentLecture();

        if (Boolean.TRUE.equals(schedule.getIsActive())) {
            throw new ScheduleUnavailableException(EResponseMessage.SCHEDULE_UNAVAILABLE.getMessage());
        }
        if (!currentLecture.equals(lecture)) {
            throw new NoAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }

        schedule.setTimeStart(requestDto.getTimeStart());
        schedule.setTimeEnd(requestDto.getTimeEnd());

        Schedule newSchedule = scheduleRepository.save(schedule);
        return newSchedule;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        scheduleRepository.deleteById(id);
    }

}
