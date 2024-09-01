package com.unper.samper.controller;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.cloudinary.json.JSONObject;
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

import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Class;
import com.unper.samper.model.Request;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.ERequestType;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.EStatus;
import com.unper.samper.model.dto.AddRequestRequestDto;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.LectureResponseDto;
import com.unper.samper.model.dto.RequestResponseDto;
import com.unper.samper.model.dto.ScheduleResponseDto;
import com.unper.samper.model.dto.SubjectResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.ClassServiceImpl;
import com.unper.samper.service.impl.LectureServiceImpl;
import com.unper.samper.service.impl.LectureSubjectServiceImpl;
import com.unper.samper.service.impl.RequestServiceImpl;
import com.unper.samper.service.impl.ScheduleServiceImpl;
import com.unper.samper.service.impl.SubjectServiceImpl;
import com.unper.samper.service.impl.UserServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Request")
@RestController
@RequestMapping("/request")
public class RequestController {
    @Autowired
    RequestServiceImpl requestServiceImpl;

    @Autowired
    UserServiceImpl userServiceImpl;

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

    @Operation(summary = "Get all request with filter")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll(
        @RequestParam(value = "receiverId", required = false) Long receiverId,
        @RequestParam(value = "senderId", required = false) Long senderId,
        @RequestParam(value = "requestTimeFrom", required = false) String requestTimeFrom,
        @RequestParam(value = "requestTimeTo", required = false) String requestTimeTo
    ) throws ResourceNotFoundException {
        List<Request> requestList = requestServiceImpl.getAll(senderId, receiverId, requestTimeFrom, requestTimeTo);
        List<RequestResponseDto> responseDtoList = new LinkedList<>();
        Integer pendingCounter = 0;
        Integer approvedCounter = 0;
        Integer rejectCounter = 0;
        Integer rescheduleCounter = 0;
        Integer lateCounter = 0;
        Integer permitCounter = 0;
        for (Request request : requestList) {
            Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(request.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(request.getSchedule().getSubject(), request.getSchedule().getKelas());
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
                subject = subjectServiceImpl.getById(request.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(request.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(request.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(request.getSchedule().getTimeEnd().getTime()))
                .isActive(request.getSchedule().getIsActive())
                .build();

            UserResponseDto senderResponseDto = UserResponseDto.builder()
                .id(request.getSender().getId())
                .firstName(request.getSender().getFirstName())
                .lastName(request.getSender().getLastName())
                .dateOfBirth(request.getSender().getDateOfBirth())
                .username(request.getSender().getUsername())
                .email(request.getSender().getEmail())
                .phoneNumber(request.getSender().getPhoneNumber())
                .roles(null)
                .build();

            UserResponseDto receiverResponseDto = UserResponseDto.builder()
                .id(request.getReceiver().getId())
                .firstName(request.getReceiver().getFirstName())
                .lastName(request.getReceiver().getLastName())
                .dateOfBirth(request.getReceiver().getDateOfBirth())
                .username(request.getReceiver().getUsername())
                .email(request.getReceiver().getEmail())
                .phoneNumber(request.getReceiver().getPhoneNumber())
                .roles(null)
                .build();

            Map<String, Object> requestDataMap = new LinkedHashMap<>();
            if (Objects.nonNull(request.getRequestData())) {
                JSONObject requestData = new JSONObject(request.getRequestData());
                for (String key : requestData.keySet()) {
                    requestDataMap.put(key, requestData.get(key));
                }
            }
            RequestResponseDto responseDto = RequestResponseDto.builder()
                .id(request.getId())
                .reason(request.getReason())
                .requestTime(request.getRequestTime())
                .approveTime(request.getApproveTime())
                .isApproved(request.getIsApproved())
                .type(request.getType())
                .status(request.getStatus())
                .schedule(scheduleResponseDto)
                .sender(senderResponseDto)
                .receiver(receiverResponseDto)
                .requsetData(requestDataMap)
                .build();
            if (responseDto.getType().equals(ERequestType.LATE_RECORD)) {
                lateCounter++;
            } else if (responseDto.getType().equals(ERequestType.PERMIT)) {
                permitCounter++;
            } else if (responseDto.getType().equals(ERequestType.RESCHEDULE)) {
                rescheduleCounter++;
            }

            if (responseDto.getStatus().equals(EStatus.PENDING)) {
                pendingCounter++;  
            } else if (responseDto.getStatus().equals(EStatus.APPROVED)) {
                approvedCounter++;
            } else if (responseDto.getStatus().equals(EStatus.REJECTED)) {
                rejectCounter++;
            }
            
            responseDtoList.add(responseDto);
        }
        Collections.sort(responseDtoList, new Comparator<RequestResponseDto>() {
            @Override
            public int compare(RequestResponseDto o1, RequestResponseDto o2){
                Date o1Date = o1.getRequestTime();
                Date o2Date = o2.getRequestTime();
                return o2Date.compareTo(o1Date);
            }
        });

        Map<String, Integer> metaMap = new LinkedHashMap<>();
        metaMap.put("__total", responseDtoList.size());
        metaMap.put("__statusPending", pendingCounter);
        metaMap.put("__statusApproved", approvedCounter);
        metaMap.put("__statusRejected", rejectCounter);
        metaMap.put("__typeLateRecord", lateCounter);
        metaMap.put("__typePermit", permitCounter);
        metaMap.put("__typeReschedule", rescheduleCounter);

        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaMap);
    }

    @Operation(summary = "Get request by id")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @GetMapping("/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {
        Request request = requestServiceImpl.getById(id);
        Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(request.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(request.getSchedule().getSubject(), request.getSchedule().getKelas());
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
                subject = subjectServiceImpl.getById(request.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            
            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(request.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(request.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(request.getSchedule().getTimeEnd().getTime()))
                .isActive(request.getSchedule().getIsActive())
                .meetingOrder(request.getSchedule().getMeetingOrder())
                .build();

            UserResponseDto senderResponseDto = UserResponseDto.builder()
                .id(request.getSender().getId())
                .firstName(request.getSender().getFirstName())
                .lastName(request.getSender().getLastName())
                .dateOfBirth(request.getSender().getDateOfBirth())
                .username(request.getSender().getUsername())
                .email(request.getSender().getEmail())
                .phoneNumber(request.getSender().getPhoneNumber())
                .roles(null)
                .build();

            UserResponseDto receiverResponseDto = UserResponseDto.builder()
                .id(request.getReceiver().getId())
                .firstName(request.getReceiver().getFirstName())
                .lastName(request.getReceiver().getLastName())
                .dateOfBirth(request.getReceiver().getDateOfBirth())
                .username(request.getReceiver().getUsername())
                .email(request.getReceiver().getEmail())
                .phoneNumber(request.getReceiver().getPhoneNumber())
                .roles(null)
                .build();

            Map<String, Object> requestDataMap = new LinkedHashMap<>();
            if (Objects.nonNull(request.getRequestData())) {
                JSONObject requestData = new JSONObject(request.getRequestData());
                for (String key : requestData.keySet()) {
                    requestDataMap.put(key, requestData.get(key));
                }
            }
            RequestResponseDto responseDto = RequestResponseDto.builder()
                .id(request.getId())
                .reason(request.getReason())
                .requestTime(request.getRequestTime())
                .approveTime(request.getApproveTime())
                .isApproved(request.getIsApproved())
                .type(request.getType())
                .status(request.getStatus())
                .schedule(scheduleResponseDto)
                .sender(senderResponseDto)
                .receiver(receiverResponseDto)
                .requsetData(requestDataMap)
                .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Add new request data")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @PostMapping("/add")
    public ResponseEntity<?> add(@RequestBody AddRequestRequestDto requestDto) throws ResourceNotFoundException, ActivityNotAllowedException{
        Request request = requestServiceImpl.add(requestDto);
        Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(request.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(request.getSchedule().getSubject(), request.getSchedule().getKelas());
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
                subject = subjectServiceImpl.getById(request.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
            
            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(request.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(request.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(request.getSchedule().getTimeEnd().getTime()))
                .isActive(request.getSchedule().getIsActive())
                .build();

            UserResponseDto senderResponseDto = UserResponseDto.builder()
                .id(request.getSender().getId())
                .firstName(request.getSender().getFirstName())
                .lastName(request.getSender().getLastName())
                .dateOfBirth(request.getSender().getDateOfBirth())
                .username(request.getSender().getUsername())
                .email(request.getSender().getEmail())
                .phoneNumber(request.getSender().getPhoneNumber())
                .roles(null)
                .build();

            UserResponseDto receiverResponseDto = UserResponseDto.builder()
                .id(request.getReceiver().getId())
                .firstName(request.getReceiver().getFirstName())
                .lastName(request.getReceiver().getLastName())
                .dateOfBirth(request.getReceiver().getDateOfBirth())
                .username(request.getReceiver().getUsername())
                .email(request.getReceiver().getEmail())
                .phoneNumber(request.getReceiver().getPhoneNumber())
                .roles(null)
                .build();

            Map<String, Object> requestDataMap = new LinkedHashMap<>();
            if (Objects.nonNull(request.getRequestData())) {
                JSONObject requestData = new JSONObject(request.getRequestData());
                for (String key : requestData.keySet()) {
                    requestDataMap.put(key, requestData.get(key));
                }
            }
            RequestResponseDto responseDto = RequestResponseDto.builder()
                .id(request.getId())
                .reason(request.getReason())
                .requestTime(request.getRequestTime())
                .approveTime(request.getApproveTime())
                .isApproved(request.getIsApproved())
                .type(request.getType())
                .status(request.getStatus())
                .schedule(scheduleResponseDto)
                .sender(senderResponseDto)
                .receiver(receiverResponseDto)
                .requsetData(requestDataMap)
                .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.CREATED, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Approve a request")
    @PreAuthorize("hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @PatchMapping("/approve")
    public ResponseEntity<?> approve(@RequestParam(value = "requestId", required = true) Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException, ParseException {
        Request request = requestServiceImpl.approve(id);
        Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(request.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(request.getSchedule().getSubject(), request.getSchedule().getKelas());
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
                subject = subjectServiceImpl.getById(request.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();

            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(request.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(request.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(request.getSchedule().getTimeEnd().getTime()))
                .isActive(request.getSchedule().getIsActive())
                .build();

            UserResponseDto senderResponseDto = UserResponseDto.builder()
                .id(request.getSender().getId())
                .firstName(request.getSender().getFirstName())
                .lastName(request.getSender().getLastName())
                .dateOfBirth(request.getSender().getDateOfBirth())
                .username(request.getSender().getUsername())
                .email(request.getSender().getEmail())
                .phoneNumber(request.getSender().getPhoneNumber())
                .roles(null)
                .build();

            UserResponseDto receiverResponseDto = UserResponseDto.builder()
                .id(request.getReceiver().getId())
                .firstName(request.getReceiver().getFirstName())
                .lastName(request.getReceiver().getLastName())
                .dateOfBirth(request.getReceiver().getDateOfBirth())
                .username(request.getReceiver().getUsername())
                .email(request.getReceiver().getEmail())
                .phoneNumber(request.getReceiver().getPhoneNumber())
                .roles(null)
                .build();

            RequestResponseDto responseDto = RequestResponseDto.builder()
                .id(request.getId())
                .reason(request.getReason())
                .requestTime(request.getRequestTime())
                .approveTime(request.getApproveTime())
                .isApproved(request.getIsApproved())
                .type(request.getType())
                .status(request.getStatus())
                .schedule(scheduleResponseDto)
                .sender(senderResponseDto)
                .receiver(receiverResponseDto)
                .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Approve a request")
    @PreAuthorize("hasAuthority('LECTURE') or hasAuthority('STUDENT')")
    @PatchMapping("/reject")
    public ResponseEntity<?> reject(@RequestParam(value = "requestId", required = true) Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException {
        Request request = requestServiceImpl.reject(id);
        Class kelas = new Class();
            try {
                kelas = classServiceImpl.getById(request.getSchedule().getKelas().getId());
            } catch (ResourceNotFoundException e) {}
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(kelas.getId())
                .lecture(null)
                .name(kelas.getName())
                .build(); 
            
            LectureSubject lectureSubject = new LectureSubject();
            try {
                lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(request.getSchedule().getSubject(), request.getSchedule().getKelas());
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
                subject = subjectServiceImpl.getById(request.getSchedule().getSubject().getId());
            } catch (ResourceNotFoundException e) {}
            SubjectResponseDto subjectResponseDto = SubjectResponseDto.builder()
                .id(subject.getId())
                .lecture(null)
                .name(subject.getName())
                .build();
            
            SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");

            ScheduleResponseDto scheduleResponseDto = ScheduleResponseDto.builder()
                .id(request.getSchedule().getId())
                .kelas(classResponseDto)
                .subject(subjectResponseDto)
                .lecture(lectureResponseDto)
                .timeStart(dateFormat.format(request.getSchedule().getTimeStart().getTime()))
                .timeEnd(dateFormat.format(request.getSchedule().getTimeEnd().getTime()))
                .isActive(request.getSchedule().getIsActive())
                .build();

            UserResponseDto senderResponseDto = UserResponseDto.builder()
                .id(request.getSender().getId())
                .firstName(request.getSender().getFirstName())
                .lastName(request.getSender().getLastName())
                .dateOfBirth(request.getSender().getDateOfBirth())
                .username(request.getSender().getUsername())
                .email(request.getSender().getEmail())
                .phoneNumber(request.getSender().getPhoneNumber())
                .roles(null)
                .build();

            UserResponseDto receiverResponseDto = UserResponseDto.builder()
                .id(request.getReceiver().getId())
                .firstName(request.getReceiver().getFirstName())
                .lastName(request.getReceiver().getLastName())
                .dateOfBirth(request.getReceiver().getDateOfBirth())
                .username(request.getReceiver().getUsername())
                .email(request.getReceiver().getEmail())
                .phoneNumber(request.getReceiver().getPhoneNumber())
                .roles(null)
                .build();

            RequestResponseDto responseDto = RequestResponseDto.builder()
                .id(request.getId())
                .reason(request.getReason())
                .requestTime(request.getRequestTime())
                .approveTime(request.getApproveTime())
                .isApproved(request.getIsApproved())
                .type(request.getType())
                .status(request.getStatus())
                .schedule(scheduleResponseDto)
                .sender(senderResponseDto)
                .receiver(receiverResponseDto)
                .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }
}
