package com.unper.samper.service.impl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.YearMonth;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.ScheduleHistory;
import com.unper.samper.model.Student;
import com.unper.samper.model.Subject;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.FaceNotMatchedException;
import com.unper.samper.exception.GeolocationException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Role;
import com.unper.samper.model.dto.ActionScheduleRequestDto;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.RescheduleRequestDto;
import com.unper.samper.model.dto.ScheduleHistoryReqeustDto;
import com.unper.samper.repository.ScheduleRepository;
import com.unper.samper.service.ScheduleSercvice;
import com.unper.samper.util.GeoUtils;

@Service
public class ScheduleServiceImpl implements ScheduleSercvice {
    @Autowired
    ScheduleRepository scheduleRepository;

    @Autowired
    ScheduleHistoryServiceImpl scheduleHistoryServiceImpl;

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

    @Autowired
    ExternalAPIServiceImpl externalAPIServiceImpl;

    @Value("${com.unper.samper.credit-minutes}")
    Short creditMinutes;

    @Override
    public List<Schedule> getAllByLecture(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException {
        User user = authenticationServiceImpl.getCurrentUser();
        Lecture lecture = lectureServiceImpl.getByUser(user);
        Long lectureId = lecture.getId();
        List<Schedule> scheduleList = scheduleRepository.findAllByLecture(filterDateFrom, filterDateTo, lectureId);
        if (scheduleList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return scheduleList;
    }

    @Override
    public List<Schedule> getAllByStudent(String filterDateFrom, String filterDateTo) throws ResourceNotFoundException {
        User user = authenticationServiceImpl.getCurrentUser();
        Student student = studentServiceImpl.getByUser(user);
        Long classId = student.getKelas().getId();
        List<Schedule> scheduleList = scheduleRepository.findAllByStudent(filterDateFrom, filterDateTo, classId);
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
            double minutes = (requestDto.getCreditAmount()*creditMinutes)/requestDto.getNumberOfMeetings();
            int roundedUpMinutes = (int) Math.ceil(minutes);
            timeEnd.add(Calendar.MINUTE, roundedUpMinutes);
             
            Schedule schedule = Schedule.builder()
                .kelas(kelas)
                .subject(subject)
                .meetingOrder(String.valueOf(i+1))
                .timeStart(timeStart)
                .timeEnd(timeEnd)
                .creditAmount(requestDto.getCreditAmount())
                .isActive(false)
                .geolocationFlag(null)
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
    public Schedule activate(ActionScheduleRequestDto requestDto) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException {
        Schedule schedule = scheduleRepository.findById(requestDto.getScheduleId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        Lecture lecture = schedule.getKelas().getLecture();
        Lecture currentLecture = lectureServiceImpl.getCurrentLecture();
        if (!currentLecture.equals(lecture)) {
            throw new NoAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.FALSE == scheduleRepository.isAvailable(requestDto.getScheduleId())) {
            throw new ScheduleUnavailableException(EResponseMessage.SCHEDULE_UNAVAILABLE.getMessage());
        }
        if (Boolean.TRUE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(Date.from(LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant()));

        String userFaceToken = lecture.getUser().getFaceToken();
      
        Map<?,?> faceCompareRespone =  externalAPIServiceImpl.faceplusplusFaceCompare(userFaceToken, requestDto.getImageBase64());

        Double faceCompareScore = (Double) faceCompareRespone.get("confidence");

        if (faceCompareScore < 80) {
            throw new FaceNotMatchedException(EResponseMessage.FACE_NOT_MATCH.getMessage());
        }

        ScheduleHistoryReqeustDto scheduleHistoryReqeustDto = ScheduleHistoryReqeustDto.builder()
            .scheduleId(schedule.getId())
            .time(calendar)
            .longitude(requestDto.getLongitude())
            .latitude(requestDto.getLatitude())
            .build();

        scheduleHistoryServiceImpl.create(scheduleHistoryReqeustDto);

        schedule.setIsActive(Boolean.TRUE);
        schedule.setGeolocationFlag(requestDto.getGeolocationFlag());
        Schedule activatedSchedule = scheduleRepository.save(schedule);
        
        return activatedSchedule;
    }

    @Override
    public Schedule deactivate(ActionScheduleRequestDto requestDto) throws NoAccessException, ResourceNotFoundException, GeolocationException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException {
        Schedule schedule = scheduleRepository.findById(requestDto.getScheduleId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        Lecture lecture = schedule.getKelas().getLecture();
        Lecture currentLecture = lectureServiceImpl.getCurrentLecture();
        if (!currentLecture.equals(lecture)) {
            throw new NoAccessException(EResponseMessage.ILLEGAL_ACCESS.getMessage());
        }
        if (Boolean.FALSE.equals(schedule.getIsActive())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(Date.from(LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant()));

        ScheduleHistory scheduleHistory = scheduleHistoryServiceImpl.getByScheduleId(requestDto.getScheduleId());
        if (Boolean.TRUE.equals(schedule.getGeolocationFlag()) && Boolean.FALSE.equals(GeoUtils.isWithinRadius(scheduleHistory.getOpenLatitude(), scheduleHistory.getOpenLongitude(), requestDto.getLatitude(), requestDto.getLongitude(), Double.valueOf(0.1)))) {
            throw new GeolocationException("You are out of the class location");
        }

        String userFaceToken = lecture.getUser().getFaceToken();
        
        Map<?,?> faceCompareRespone =  externalAPIServiceImpl.faceplusplusFaceCompare(userFaceToken, requestDto.getImageBase64());

        Double faceCompareScore = (Double) faceCompareRespone.get("confidence");

        if (faceCompareScore < 80) {
            throw new FaceNotMatchedException(EResponseMessage.FACE_NOT_MATCH.getMessage());
        }

        ScheduleHistoryReqeustDto scheduleHistoryReqeustDto = ScheduleHistoryReqeustDto.builder()
            .scheduleId(schedule.getId())
            .time(calendar)
            .longitude(requestDto.getLongitude())
            .latitude(requestDto.getLatitude())
            .build();

        scheduleHistoryServiceImpl.update(scheduleHistoryReqeustDto);
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
