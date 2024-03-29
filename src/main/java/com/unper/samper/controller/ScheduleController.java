package com.unper.samper.controller;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Subject;
import com.unper.samper.model.Class;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.RescheduleRequestDto;
import com.unper.samper.model.dto.ScheduleResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.ClassServiceImpl;
import com.unper.samper.service.impl.LectureServiceImpl;
import com.unper.samper.service.impl.LectureSubjectServiceImpl;
import com.unper.samper.service.impl.ScheduleServiceImpl;
import com.unper.samper.service.impl.SubjectServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Schedule")
@RestController
@RequestMapping("/schedule")
public class ScheduleController {
    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Autowired
    ClassServiceImpl classServiceImpl;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Autowired
    SubjectServiceImpl subjectServiceImpl;

    @Autowired
    LectureSubjectServiceImpl lectureSubjectServiceImpl;

    @Operation(summary = "Get all data of schedules")
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll(
        @RequestParam(value = "dateFrom", required = false) LocalDate filterDateFrom, 
        @RequestParam(value = "dateTo", required = false) LocalDate filterDateTo, 
        @RequestParam(value = "classId", required = false) Long classId) throws ResourceNotFoundException {
        List<Schedule> scheduleList = scheduleServiceImpl.getAll(filterDateFrom, filterDateTo, classId);
        List<ScheduleResponseDto> responseDtoList = new ArrayList<>();
        scheduleList.forEach(schedule -> {
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(schedule.getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .Id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(schedule.getSubject(), schedule.getKelas());
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
                subject = subjectServiceImpl.getById(schedule.getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();
            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(schedule.getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(schedule.getTimeStart())
                .timeEnd(schedule.getTimeEnd())
                .isActive(schedule.getIsActive())
                .build();
            responseDtoList.add(scheduleResponseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList);
    }

    @Operation(summary = "Get all data of schedules")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('STUDENT')")
    @GetMapping("/allbycurrentuserclass")
    public ResponseEntity<?> getAllByCurrentUserClass(
        @RequestParam(value = "dateFrom", required = false) String filterDateFrom, 
        @RequestParam(value = "dateTo", required = false) String filterDateTo,
        @RequestParam(value = "userId", required = false) Long userId) throws ResourceNotFoundException {
        List<Schedule> scheduleList = scheduleServiceImpl.getAllByUserClass(filterDateFrom, filterDateTo, userId);
        List<ScheduleResponseDto> responseDtoList = new ArrayList<>();
        scheduleList.forEach(schedule -> {
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(schedule.getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .Id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 

            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(schedule.getSubject(), schedule.getKelas());
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
                subject = subjectServiceImpl.getById(schedule.getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(schedule.getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(schedule.getTimeStart())
                .timeEnd(schedule.getTimeEnd())
                .isActive(schedule.getIsActive())
                .build();
            responseDtoList.add(scheduleResponseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList); 
    }

    @Operation(summary = "Add new schedule")
    @PreAuthorize("hasAuthority('LECTURE')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        scheduleServiceImpl.add(requestDto);
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), null);
    }

    @Operation(summary = "Activate schedule")
    @PreAuthorize("hasAuthority('LECTURE')")
    @PatchMapping("/activate")
    public ResponseEntity<?> activate(Long id) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException {
        Schedule schedule = scheduleServiceImpl.activate(id);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(schedule.getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .Id(kelas.getId())
            .lecture(null)
            .name(kelas.getName())
            .build(); 
        Subject subject = new Subject();
        try {
            subject = subjectServiceImpl.getById(schedule.getSubject().getId());
        } catch (ResourceNotFoundException e) {}
        SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .lecture(null)
            .name(subject.getName())
            .build();
        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(schedule.getTimeStart())
            .timeEnd(schedule.getTimeEnd())
            .isActive(schedule.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.ACTIVATE_SCHEDULE_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Deactivate shedule")
    @PreAuthorize("hasAuthority('LECTURE')")
    @PatchMapping("/deactivate")
    public ResponseEntity<?> deactivate(Long id) throws ResourceNotFoundException, NoAccessException, ScheduleUnavailableException {
        Schedule schedule = scheduleServiceImpl.deactivate(id);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(schedule.getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .Id(kelas.getId())
            .lecture(null)
            .name(kelas.getName())
            .build(); 
        Subject subject = new Subject();
        try {
            subject = subjectServiceImpl.getById(schedule.getSubject().getId());
        } catch (ResourceNotFoundException e) {}
        SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .lecture(null)
            .name(subject.getName())
            .build();
        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(schedule.getTimeStart())
            .timeEnd(schedule.getTimeEnd())
            .isActive(schedule.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DEACTIVATE_SCHEDULE.getMessage(), responseDto);
    }

    @Operation(summary = "Change the schedule start and end time")
    @PreAuthorize("hasAuthority('LECTURE')")
    @PatchMapping("/reschedule")
    public ResponseEntity<?> reschedule(@RequestBody RescheduleRequestDto requestDto) throws ResourceNotFoundException, ScheduleUnavailableException, NoAccessException {
        Schedule schedule = scheduleServiceImpl.reschedule(requestDto);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(schedule.getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .Id(kelas.getId())
            .lecture(null)
            .name(kelas.getName())
            .build(); 
        Subject subject = new Subject();
        try {
            subject = subjectServiceImpl.getById(schedule.getSubject().getId());
        } catch (ResourceNotFoundException e) {}
        SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
            .id(subject.getId())
            .lecture(null)
            .name(subject.getName())
            .build();
        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(schedule.getTimeStart())
            .timeEnd(schedule.getTimeEnd())
            .isActive(schedule.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Soft delete a schedule")
    @PreAuthorize("hasAthority('ADMIN') or hasAuthority('LECTURE')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {  
        scheduleServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
