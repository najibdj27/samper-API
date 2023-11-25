package com.unper.samper.controller;

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
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.IllegalAccessException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.ScheduleUnavailableException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Subject;
import com.unper.samper.model.Class;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddScheduleRequestDto;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.ScheduleResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.service.impl.ClassServiceImpl;
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
    SubjectServiceImpl subjectServiceImpl;

    @Operation(summary = "Get all data of schedules")
    @PreAuthorize("hasAuthority('ADMIN')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<Schedule> scheduleList = scheduleServiceImpl.getAll();
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
                .timeStart(schedule.getTimeStart())
                .timeEnd(schedule.getTimeEnd())
                .isActive(schedule.getIsActive())
                .build();
            responseDtoList.add(scheduleResponseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), scheduleServiceImpl);
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
    public ResponseEntity<?> activate(Long id) throws ResourceNotFoundException, IllegalAccessException, ScheduleUnavailableException {
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
    public ResponseEntity<?> deactivate(Long id) throws ResourceNotFoundException, IllegalAccessException, ScheduleUnavailableException {
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

    @Operation(summary = "Soft delete a schedule")
    @PreAuthorize("hasAthority('ADMIN') or hasAuthority('LECTURE')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException {  
        scheduleServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
