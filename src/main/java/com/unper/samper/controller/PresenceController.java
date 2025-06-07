package com.unper.samper.controller;

import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

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
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Class;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.PresenceRecordRequestDto;
import com.unper.samper.model.dto.PresenceResponseDto;
import com.unper.samper.model.dto.ScheduleResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.ClassServiceImpl;
import com.unper.samper.service.impl.LectureSubjectServiceImpl;
import com.unper.samper.service.impl.PresenceServiceImpl;
import com.unper.samper.service.impl.SubjectServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Presence")
@RequestMapping("/presence")
@RestController
public class PresenceController {
    @Autowired
    PresenceServiceImpl presenceServiceImpl;

    @Autowired
    ClassServiceImpl classServiceImpl;

    @Autowired
    LectureSubjectServiceImpl lectureSubjectServiceImpl;

    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @PreAuthorize("hasAuthority('LECTURE')")
    @Operation(summary = "Get all presence data by current lecture")
    @GetMapping("/getallbylecture")
    public ResponseEntity<?> getAllByLecture() throws ResourceNotFoundException {
        List<Presence> presenceList = presenceServiceImpl.getAllByLecture();
        List<PresenceResponseDto> responseDtoList = new ArrayList<>();
        presenceList.forEach(presence -> {
            
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(presence.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(presence.getSchedule().getSubject(), presence.getSchedule().getKelas());
            } catch (ResourceNotFoundException e) {}

            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(lectureSubject.getLecture().getUser().getId())
                .firstName(lectureSubject.getLecture().getUser().getFirstName())
                .lastName(lectureSubject.getLecture().getUser().getLastName())
                .dateOfBirth(lectureSubject.getLecture().getUser().getDateOfBirth())
                .username(lectureSubject.getLecture().getUser().getUsername())
                .email(lectureSubject.getLecture().getUser().getEmail())
                .phoneNumber(lectureSubject.getLecture().getUser().getPhoneNumber())
                .roles(null)
                .build();

            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(lectureSubject.getLecture().getId())
                .NIP(lectureSubject.getLecture().getNIP())
                .user(userResponseDto)
                .build();
            
            Subject subject = new Subject();
            try {
                subject = subjectServiceImpl.getById(presence.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(presence.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(presence.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(presence.getSchedule().getTimeEnd().getTime()))
                .isActive(presence.getSchedule().getIsActive())
                .build();
                
            PresenceResponseDto responseDto = PresenceResponseDto.builder()
                .id(presence.getId())
                .student(null)
                .schedule(scheduleResponseDto)
                .time(presence.getTime())
                .type(presence.getType())
                .longitude(presence.getLongitude())
                .latitude(presence.getLatitude())
                .build();
            responseDtoList.add(responseDto);
        });
        Map<String, Integer> metaData = new HashMap<>();
        metaData.put("_total", presenceList.size());   
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaData);
    }

    @PreAuthorize("hasAuthority('STUDENT')")
    @Operation(summary = "Get all presence data by student")
    @GetMapping("/getallbystudent")
    public ResponseEntity<?> getAllByStudent(
        @RequestParam(value = "studentId", required = true) Long studentId,
        @RequestParam(value = "limit", required = false) Integer limit
    ) throws ResourceNotFoundException {
        List<Presence> presenceList = presenceServiceImpl.findByStudent(studentId, limit);
        List<PresenceResponseDto> responseDtoList = new ArrayList<>();
        presenceList.forEach(presence -> {
            
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(presence.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(presence.getSchedule().getSubject(), presence.getSchedule().getKelas());
            } catch (ResourceNotFoundException e) {}

            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(lectureSubject.getLecture().getUser().getId())
                .firstName(lectureSubject.getLecture().getUser().getFirstName())
                .lastName(lectureSubject.getLecture().getUser().getLastName())
                .dateOfBirth(lectureSubject.getLecture().getUser().getDateOfBirth())
                .username(lectureSubject.getLecture().getUser().getUsername())
                .email(lectureSubject.getLecture().getUser().getEmail())
                .phoneNumber(lectureSubject.getLecture().getUser().getPhoneNumber())
                .roles(null)
                .build();

            LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
                .id(lectureSubject.getLecture().getId())
                .NIP(lectureSubject.getLecture().getNIP())
                .user(userResponseDto)
                .build();
            
            Subject subject = new Subject();
            try {
                subject = subjectServiceImpl.getById(presence.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(presence.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(presence.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(presence.getSchedule().getTimeEnd().getTime()))
                .isActive(presence.getSchedule().getIsActive())
                .build();
                
            PresenceResponseDto responseDto = PresenceResponseDto.builder()
                .id(presence.getId())
                .student(null)
                .schedule(scheduleResponseDto)
                .time(presence.getTime())
                .type(presence.getType())
                .longitude(presence.getLongitude())
                .latitude(presence.getLatitude())
                .build();
            responseDtoList.add(responseDto);
        });
        Map<String, Integer> metaData = new HashMap<>();
        metaData.put("_total", presenceList.size());
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaData);
    }

    @PreAuthorize("hasAuthority('STUDENT')")
    @Operation(summary = "Record check in presence")
    @PostMapping("/checkin")
    public ResponseEntity<?> checkIn(@RequestBody PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, DifferentClassException, ScheduleNotActiveException, OnScheduleException, JsonMappingException, JsonProcessingException, ExternalAPIException, FaceNotMatchedException, GeolocationException{
        Presence presence = presenceServiceImpl.checkIn(requestDto);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(presence.getSchedule().getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .id(kelas.getId())
            .lecture(null)
            .name(kelas.getName())
            .build(); 
        
        LectureSubject lectureSubject = new LectureSubject();
        try {
            lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(presence.getSchedule().getSubject(), presence.getSchedule().getKelas());
        } catch (ResourceNotFoundException e) {}

        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(lectureSubject.getLecture().getUser().getId())
            .firstName(lectureSubject.getLecture().getUser().getFirstName())
            .lastName(lectureSubject.getLecture().getUser().getLastName())
            .dateOfBirth(lectureSubject.getLecture().getUser().getDateOfBirth())
            .username(lectureSubject.getLecture().getUser().getUsername())
            .email(lectureSubject.getLecture().getUser().getEmail())
            .phoneNumber(lectureSubject.getLecture().getUser().getPhoneNumber())
            .roles(null)
            .build();

        LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
            .id(lectureSubject.getLecture().getId())
            .NIP(lectureSubject.getLecture().getNIP())
            .user(userResponseDto)
            .build();
        
        Subject subject = new Subject();
        try {
            subject = subjectServiceImpl.getById(presence.getSchedule().getSubject().getId());
        } catch (ResourceNotFoundException e) {}
        SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .lecture(null)
            .name(subject.getName())
            .build();

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
            .id(presence.getSchedule().getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .lecture(lectureResponseDto)
            .timeStart(dateFormat.format(presence.getSchedule().getTimeStart().getTime()))
            .timeEnd(dateFormat.format(presence.getSchedule().getTimeEnd().getTime()))
            .isActive(presence.getSchedule().getIsActive())
            .build();

        PresenceResponseDto responseDto = PresenceResponseDto.builder()
            .id(presence.getId())
            .student(null)
            .schedule(scheduleResponseDto)
            .time(presence.getTime())
            .type(presence.getType())
            .longitude(presence.getLongitude())
            .latitude(presence.getLatitude())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), responseDto);
    }

    @PreAuthorize("hasAuthority('STUDENT')")
    @Operation(summary = "Record check out presence")
    @PostMapping("/checkout")
    public ResponseEntity<?> checkOut(@RequestBody PresenceRecordRequestDto requestDto) throws ResourceNotFoundException, ScheduleNotActiveException, DifferentClassException, OutScheduleException, ActivityNotAllowedException, JsonMappingException, JsonProcessingException, FaceNotMatchedException, ExternalAPIException, GeolocationException {
        Presence presence = presenceServiceImpl.checkOut(requestDto);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(presence.getSchedule().getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .id(kelas.getId())
            .lecture(null)
            .name(kelas.getName())
            .build(); 
        
        LectureSubject lectureSubject = new LectureSubject();
        try {
            lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(presence.getSchedule().getSubject(), presence.getSchedule().getKelas());
        } catch (ResourceNotFoundException e) {}

        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(lectureSubject.getLecture().getUser().getId())
            .firstName(lectureSubject.getLecture().getUser().getFirstName())
            .lastName(lectureSubject.getLecture().getUser().getLastName())
            .dateOfBirth(lectureSubject.getLecture().getUser().getDateOfBirth())
            .username(lectureSubject.getLecture().getUser().getUsername())
            .email(lectureSubject.getLecture().getUser().getEmail())
            .phoneNumber(lectureSubject.getLecture().getUser().getPhoneNumber())
            .roles(null)
            .build();

        LectureResponseDto lectureResponseDto = LectureResponseDto.builder()
            .id(lectureSubject.getLecture().getId())
            .NIP(lectureSubject.getLecture().getNIP())
            .user(userResponseDto)
            .build();
        
        Subject subject = new Subject();
        try {
            subject = subjectServiceImpl.getById(presence.getSchedule().getSubject().getId());
        } catch (ResourceNotFoundException e) {}
        SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .lecture(null)
            .name(subject.getName())
            .build();

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
            .id(presence.getSchedule().getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .lecture(lectureResponseDto)
            .timeStart(dateFormat.format(presence.getSchedule().getTimeStart().getTime()))
            .timeEnd(dateFormat.format(presence.getSchedule().getTimeEnd().getTime()))
            .isActive(presence.getSchedule().getIsActive())
            .build();

        PresenceResponseDto responseDto = PresenceResponseDto.builder()
            .id(presence.getId())
            .student(null)
            .schedule(scheduleResponseDto)
            .time(presence.getTime())
            .type(presence.getType())
            .longitude(presence.getLongitude())
            .latitude(presence.getLatitude())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.PRESENCE_SUCCESS.getMessage(), responseDto);
    }

    @PreAuthorize("hasAuthority('ADMMIN') or hasAuthority('LECTURE')")
    @Operation(summary = "Record check out presence")
    @PostMapping("/delete")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {
        presenceServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
