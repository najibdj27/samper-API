package com.unper.samper.service.impl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;
import com.unper.samper.model.Subject;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Role;
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

    @Autowired
    LectureSubjectServiceImpl lectureSubjectServiceImpl;

    @Value("${com.unper.samper.credithour}")
    Short creditHour;

    @Override
    public List<Schedule> getAll(LocalDate filterDateFrom, LocalDate filterDateTo, Long classId) throws ResourceNotFoundException {
        String dateFrom = filterDateFrom.toString();
        String dateTo = filterDateTo.toString();
        List<Schedule> scheduleList = scheduleRepository.findAllByStudent(dateFrom, dateTo, classId);
        if (scheduleList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return scheduleList;
    }

    @Override
    public List<Schedule> getAllByCurrentUserClass(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException {
        User user = authenticationServiceImpl.getCurrentUser();
        Student student = studentServiceImpl.getByUser(user);
        Long classId = student.getKelas().getId();
        List<Schedule> scheduleList = scheduleRepository.findAllByLecture(filterDateFrom, filterDateTo, classId);
        if (scheduleList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return scheduleList;
    }

    @Override
    public List<Schedule> getScheduleMonthly(String dateStr, Long userId) throws ParseException, ResourceNotFoundException {
       SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
       Calendar firtDayOfMonth = Calendar.getInstance();
       Calendar lastDayOfMonth = Calendar.getInstance();
       firtDayOfMonth.setTime(sdf.parse(dateStr));
       lastDayOfMonth.setTime(sdf.parse(dateStr));
       int year = lastDayOfMonth.get(Calendar.YEAR);
       int month = lastDayOfMonth.get(Calendar.MONTH);
       YearMonth yearMonth = YearMonth.of(year, month+1);
       int monthLength = yearMonth.lengthOfMonth();
       firtDayOfMonth.set(Calendar.DAY_OF_MONTH, 1);
       lastDayOfMonth.set(Calendar.DAY_OF_MONTH, monthLength);
       User user = userServiceImpl.getById(userId);
       List<Schedule> scheduleList = new ArrayList<>();
       List<ERole> roles = new ArrayList<>();
       for (Role role : user.getRoles()) {
            roles.add(role.getName());
       }
       if (roles.contains(ERole.valueOf("STUDENT"))) {
            Student student = studentServiceImpl.getByUser(user);
            Long classId = student.getKelas().getId();
           scheduleList = scheduleRepository.findAllByStudent(sdf.format(firtDayOfMonth.getTime()), sdf.format(lastDayOfMonth.getTime()), classId);
        } else {
            Lecture lecture = lectureServiceImpl.getByUser(user);
            scheduleList = scheduleRepository.findAllByLecture(sdf.format(firtDayOfMonth.getTime()), sdf.format(lastDayOfMonth.getTime()), lecture.getId());
        }

       return scheduleList;
    }

    @Override
    public Schedule getById(Long id) throws ResourceNotFoundException {
        Schedule schedule = scheduleRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return schedule;
    }

    @Transactional(rollbackFor = {ResourceNotFoundException.class, ResourceAlreadyExistException.class, ParseException.class})
    @Override
    public List<Schedule> add(AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException, ParseException {
        Class kelas = classServiceImpl.getById(requestDto.getClassId());
        Subject subject = subjectServiceImpl.getById(requestDto.getSubjectId());
        Lecture lecture = lectureServiceImpl.getById(requestDto.getLectureId());
        SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm");

        LectureSubject lectureSubject = lectureSubjectServiceImpl.getByLectureAndSubject(subject, lecture);

        if (Boolean.FALSE.equals(lectureSubjectServiceImpl.checkLectureSubjectClass(lectureSubject, kelas))) {
            lectureSubjectServiceImpl.addClass(lectureSubject.getId(), kelas.getId());
        }

        List<Schedule> scheduleList = new ArrayList<>();
        for (int i = 0; i < requestDto.getNumberOfMeetings(); i++) {
            Calendar timeStart = Calendar.getInstance();
            Calendar timeEnd = Calendar.getInstance();
            timeStart.setTime(simpleDateFormat.parse(requestDto.getDateStart() + " " + requestDto.getTimeStart()));
            timeEnd.setTime(simpleDateFormat.parse(requestDto.getDateStart() + " " + requestDto.getTimeStart()));
            timeStart.add(Calendar.DAY_OF_MONTH, 7*i); 
            timeEnd.add(Calendar.DAY_OF_MONTH, 7*i);
            timeEnd.add(Calendar.MINUTE, requestDto.getCreditAmount()*creditHour);
             
            Schedule schedule = Schedule.builder()
                .kelas(kelas)
                .subject(subject)
                .meetingOrder(String.valueOf(i+1))
                .timeStart(timeStart)
                .timeEnd(timeEnd)
                .creditAmount(requestDto.getCreditAmount())
                .isActive(false)
                .build();
            scheduleList.add(schedule);
        }
        
        return scheduleRepository.saveAll(scheduleList);
    }

    @Override
    public Schedule edit(Schedule schedule) throws ResourceNotFoundException {
        getById(schedule.getId());
        return scheduleRepository.save(schedule);
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
