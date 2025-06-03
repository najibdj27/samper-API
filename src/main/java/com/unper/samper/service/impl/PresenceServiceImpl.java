package com.unper.samper.service.impl;

import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.DifferentClassException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.FaceNotMatchedException;
import com.unper.samper.exception.GeolocationException;
import com.unper.samper.exception.OnScheduleException;
import com.unper.samper.exception.OutScheduleException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleNotActiveException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.ScheduleHistory;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.PresenceRecordRequestDto;
import com.unper.samper.repository.PresenceRepository;
import com.unper.samper.service.PresenceService;
import com.unper.samper.util.GeoUtils;

@Service
public class PresenceServiceImpl implements PresenceService {
    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Autowired
    ScheduleHistoryServiceImpl scheduleHistoryServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    PresenceRepository presenceRepository;

    @Autowired
    ExternalAPIServiceImpl externalAPIServiceImpl;

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
    public Presence getByCurrentStudentAndScheduleAndType(Schedule schedule, Character type) throws ResourceNotFoundException {
        User user = authenticationServiceImpl.getCurrentUser();
        Student student = studentServiceImpl.getByUser(user);
        Presence presenceList = presenceRepository.findByStudentAndScheduleAndType(student, schedule, type);

        return presenceList;
    }

    @Override
    public List<Presence> findByStudent(Long studentId, Integer limit) throws ResourceNotFoundException {
        List<Presence> presenceList = presenceRepository.findByStudent(studentId, limit);
        if (presenceList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return presenceList;
    }

    @Override
    public Presence checkIn(PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException, ExternalAPIException, JsonMappingException, JsonProcessingException, FaceNotMatchedException, GeolocationException {
        Schedule schedule = scheduleServiceImpl.getById(requestDto.getScheduleId());
        Student student = studentServiceImpl.getCurrentStudent();
        
        //check if student record a presence from a different class
        if (schedule.getKelas() != student.getKelas()) {
            throw new DifferentClassException(EResponseMessage.PRESENCE_DIFFERENT_CLASS.getMessage());
        }
        
        // check if schedule is inactive
        if (Boolean.FALSE.equals(schedule.getIsActive())) {
            throw new ScheduleNotActiveException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        // check if student is on schedule
        if (Boolean.TRUE.equals(presenceRepository.inOnCurrentSchedule(student, schedule))) {
            throw new OnScheduleException(EResponseMessage.ON_SCHEDULE.getMessage());
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(Date.from(LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant()));

        ScheduleHistory scheduleHistory = scheduleHistoryServiceImpl.getByScheduleId(requestDto.getScheduleId());
        if (Boolean.TRUE.equals(schedule.getGeolocationFlag()) && Boolean.FALSE.equals(GeoUtils.isWithinRadius(scheduleHistory.getOpenLatitude(), scheduleHistory.getOpenLongitude(), requestDto.getLatitude(), requestDto.getLongitude(), Double.valueOf(0.1)))) {
            throw new GeolocationException("You are out of the class location");
        }

        String userFaceToken = student.getUser().getFaceToken();
      
        ResponseEntity<String> faceCompareRespone =  externalAPIServiceImpl.faceplusplusFaceCompare(userFaceToken, requestDto.getImageBase64());
        ObjectMapper faceCompareMapper = new ObjectMapper();
        JsonNode faceCommpareRoot =  faceCompareMapper.readTree(faceCompareRespone.getBody());

        double faceCompareScore = faceCommpareRoot.path("confidence").asDouble();

        if (faceCompareScore < 80) {
            throw new FaceNotMatchedException(EResponseMessage.FACE_NOT_MATCH.getMessage());
        }

        Presence presence = Presence.builder()
            .student(student)
            .schedule(schedule)
            .time(calendar)
            .type('I')
            .longitude(requestDto.getLongitude())
            .latitude(requestDto.getLatitude())
            .build();
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }

    @Override
    public Presence checkOut(PresenceRecordRequestDto requestDto) throws ScheduleNotActiveException, ResourceNotFoundException, DifferentClassException, OutScheduleException, ActivityNotAllowedException, FaceNotMatchedException, ExternalAPIException, JsonMappingException, JsonProcessingException, GeolocationException {
        Schedule schedule = scheduleServiceImpl.getById(requestDto.getScheduleId());
        Student student = studentServiceImpl.getCurrentStudent();
        
        //check if student record a presence from a different class
        if (schedule.getKelas() != student.getKelas()) {
            throw new DifferentClassException(EResponseMessage.PRESENCE_DIFFERENT_CLASS.getMessage());
        }
        
        // check if schedule is inactive
        if (Boolean.FALSE.equals(schedule.getIsActive())) {
            throw new ScheduleNotActiveException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        // check if student is on schedule
        if (Boolean.FALSE.equals(presenceRepository.inOnCurrentSchedule(student, schedule))) {
            throw new OutScheduleException(EResponseMessage.OUT_SCHEDULE.getMessage());
        }

        if (Boolean.TRUE.equals(presenceRepository.isCheckOut(student, schedule))) {
            throw new ActivityNotAllowedException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
        }

        Calendar calendar = Calendar.getInstance();
        calendar.setTime(Date.from(LocalDateTime.now().atZone(ZoneId.systemDefault()).toInstant()));

        ScheduleHistory scheduleHistory = scheduleHistoryServiceImpl.getByScheduleId(requestDto.getScheduleId());
        if (Boolean.FALSE.equals(Boolean.TRUE.equals(schedule.getGeolocationFlag()) && GeoUtils.isWithinRadius(scheduleHistory.getCloseLatitude(), scheduleHistory.getCloseLongitude(), requestDto.getLatitude(), requestDto.getLongitude(), Double.valueOf(0.1)))) {
            throw new GeolocationException("You are out of the class location");
        }

        String userFaceToken = student.getUser().getFaceToken();
      
        ResponseEntity<String> faceCompareRespone =  externalAPIServiceImpl.faceplusplusFaceCompare(userFaceToken, requestDto.getImageBase64());
        ObjectMapper faceCompareMapper = new ObjectMapper();
        JsonNode faceCommpareRoot =  faceCompareMapper.readTree(faceCompareRespone.getBody());

        double faceCompareScore = faceCommpareRoot.path("confidence").asDouble();

        if (faceCompareScore < 80) {
            throw new FaceNotMatchedException(EResponseMessage.FACE_NOT_MATCH.getMessage());
        } 

        Presence presence = Presence.builder()
            .student(student)
            .schedule(schedule)
            .time(calendar)
            .type('O')
            .longitude(requestDto.getLongitude())
            .latitude(requestDto.getLatitude())
            .build();
        Presence newPresence = presenceRepository.save(presence);

        return newPresence;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        presenceRepository.deleteById(id);
    }
}
