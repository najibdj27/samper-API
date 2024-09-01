package com.unper.samper.controller;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.YearMonth;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

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
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('STUDENT')")
    @GetMapping("/allbystudent")
    public ResponseEntity<?> getAll(
        @RequestParam(value = "dateFrom", required = false) String filterDateFrom, 
        @RequestParam(value = "dateTo", required = false) String filterDateTo) throws ResourceNotFoundException {
        List<Schedule> scheduleList = scheduleServiceImpl.getAllByCurrentUserClass(filterDateFrom, filterDateTo);
        List<ScheduleResponseDto> responseDtoList = new LinkedList<>();
        scheduleList.forEach(schedule -> {
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(schedule.getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
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

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(schedule.getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
                .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
                .creditAmount(schedule.getCreditAmount())
                .meetingOrder(schedule.getMeetingOrder())
                .isActive(schedule.getIsActive())
                .build();
            responseDtoList.add(scheduleResponseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList);
    }

    @Operation(summary = "Get all data of schedules")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @GetMapping("/allbylecture")
    public ResponseEntity<?> getAllByCurrentUserClass(
        @RequestParam(value = "dateFrom", required = false) String filterDateFrom, 
        @RequestParam(value = "dateTo", required = false) String filterDateTo) throws ResourceNotFoundException {
        List<Schedule> scheduleList = scheduleServiceImpl.getAll(filterDateFrom, filterDateTo);
        List<ScheduleResponseDto> responseDtoList = new ArrayList<>();
        scheduleList.forEach(schedule -> {
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(schedule.getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
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

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(schedule.getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .meetingOrder(schedule.getMeetingOrder())
                .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
                .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
                .creditAmount(schedule.getCreditAmount())
                .meetingOrder(schedule.getMeetingOrder())
                .isActive(schedule.getIsActive())
                .build();
            responseDtoList.add(scheduleResponseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList); 
    }

    @Operation(summary = "Get data of schedules for a month by the given date and user's class")
    @PreAuthorize("hasAuthority('STUDENT') or hasAuthority('LECTURE')")
    @GetMapping("/getmonthlyschedule")
    public ResponseEntity<?> getScheduleMonthly(
        @RequestParam(value = "date", required = true) String dateStr,
        @RequestParam(value = "userId", required = true) Long userId
    ) throws ParseException, ResourceNotFoundException {
        Map<String, List<ScheduleResponseDto>> responseMap = new LinkedHashMap<>();
        List<Schedule> scheduleList = scheduleServiceImpl.getScheduleMonthly(dateStr, userId);
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd");
        Calendar firtDayOfMonth = Calendar.getInstance();
        Calendar dayOfMonth = Calendar.getInstance();
        dayOfMonth.setTime(sdf.parse(dateStr));
        firtDayOfMonth.setTime(sdf.parse(dateStr));
        int year = dayOfMonth.get(Calendar.YEAR);
        int month = dayOfMonth.get(Calendar.MONTH);
        YearMonth yearMonth = YearMonth.of(year, month+1);
        int monthLength = yearMonth.lengthOfMonth();
        
        for (int i = 0; i < monthLength; i++) {
            firtDayOfMonth.set(Calendar.DAY_OF_MONTH, i+1);
            List<ScheduleResponseDto> scheduleOfTheDay = new LinkedList<>();
            for (Schedule schedule : scheduleList) {
                String scheduleDate = sdf.format(schedule.getTimeStart().getTime());
                if (sdf.format(firtDayOfMonth.getTime()).equals(scheduleDate)) {
                    Class kelas = new Class();
                    try {
                        kelas = classServiceImpl.getById(schedule.getKelas().getId());
                    } catch (ResourceNotFoundException e) {}
                    ClassResponseDto classResponseDto = ClassResponseDto.builder()
                        .id(kelas.getId())
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
                        
                    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                    ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                        .id(schedule.getId())
                        .kelas(classResponseDto)
                        .subject(subjectResponseDto)
                        .lecture(lectureResponseDto)
                        .meetingOrder(schedule.getMeetingOrder())
                        .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
                        .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
                        .creditAmount(schedule.getCreditAmount())
                        .meetingOrder(schedule.getMeetingOrder())
                        .isActive(schedule.getIsActive())
                        .build();
                    scheduleOfTheDay.add(scheduleResponseDto);
                }
            }
            Collections.sort(scheduleOfTheDay, new Comparator<ScheduleResponseDto>() {
                @Override
                public int compare(ScheduleResponseDto o1, ScheduleResponseDto o2){
                    SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
                    Date o1Date = new Date();
                    Date o2Date = new Date();
                    try {
                        o1Date = sdf.parse(o1.getTimeStart());
                        o2Date = sdf.parse(o2.getTimeStart());
                    } catch (ParseException e) {
                        // TODO: handle exception
                    }
                    return o1Date.compareTo(o2Date);
                }
            });
            responseMap.put(sdf.format(firtDayOfMonth.getTime()), scheduleOfTheDay);
        }

        Map<String, Integer> metaMap = new LinkedHashMap<>();
        metaMap.put("__total", scheduleList.size());
        metaMap.put("__dateTotal", responseMap.size());
        metaMap.put("__monthIndex", month+1);
        metaMap.put("__monthLength", monthLength);
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseMap, metaMap);
    }

    @Operation(summary = "Get Schedule detail by id")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {
        Schedule schedule = scheduleServiceImpl.getById(id);
        Class kelas = new Class();
        try {
            kelas = classServiceImpl.getById(schedule.getKelas().getId());
        } catch (ResourceNotFoundException e) {}
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .id(kelas.getId())
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

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .lecture(lectureResponseDto)
            .meetingOrder(schedule.getMeetingOrder())
            .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
            .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
            .creditAmount(schedule.getCreditAmount())
            .meetingOrder(schedule.getMeetingOrder())
            .isActive(schedule.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), scheduleResponseDto);
    }

    @Operation(summary = "Add new schedule")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddScheduleRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException, ParseException {
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
            .id(kelas.getId())
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

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
            .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
            .creditAmount(schedule.getCreditAmount())
            .meetingOrder(schedule.getMeetingOrder())
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
            .id(kelas.getId())
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

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
        
        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
            .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
            .creditAmount(schedule.getCreditAmount())
            .meetingOrder(schedule.getMeetingOrder())
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
            .id(kelas.getId())
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

        SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

        ScheduleResponseDto responseDto = ScheduleResponseDto.builder()
            .id(schedule.getId())
            .kelas(classResponseDto)
            .subject(subjectResponseDto)
            .timeStart(dateFormat.format(schedule.getTimeStart().getTime()))
            .timeEnd(dateFormat.format(schedule.getTimeEnd().getTime()))
            .creditAmount(schedule.getCreditAmount())
            .meetingOrder(schedule.getMeetingOrder())
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
