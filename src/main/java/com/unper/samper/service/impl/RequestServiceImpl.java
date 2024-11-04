package com.unper.samper.service.impl;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.util.List;
import java.util.Objects;

import org.cloudinary.json.JSONObject;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Request;
import com.unper.samper.model.Role;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.ERequestType;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.model.constant.EStatus;
import com.unper.samper.model.dto.AddRequestRequestDto;
import com.unper.samper.repository.RequestRepository;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.service.RequestService;

@Service
public class RequestServiceImpl implements RequestService {

    @Autowired
    RequestRepository requestRepository;

    @Autowired
    ScheduleServiceImpl scheduleServiceImpl;

    @Autowired
    UserServiceImpl userServiceImpl;
    
    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    LectureSubjectServiceImpl lectureSubjectServiceImpl;

    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    RoleRepository roleRepository;

    @Override
    public List<Request> getAll(Long senderId, Long receiverId, String requestTimeFrom, String requestTimeTo) throws ResourceNotFoundException {
        if (Objects.nonNull(senderId) && Objects.nonNull(receiverId)){
            if (receiverId.equals(senderId)) {
                throw new IllegalArgumentException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
            }
        }
        if (Objects.nonNull(requestTimeFrom) && Objects.nonNull(requestTimeTo)) {
            if (requestTimeFrom.equals(requestTimeTo)) {
                throw new IllegalArgumentException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
            }
        }
        if (Objects.isNull(requestTimeFrom)) {
            if (Objects.nonNull(requestTimeTo)) {
                throw new IllegalArgumentException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
            }
        } else {
            if (Objects.isNull(requestTimeTo)) {
                throw new IllegalArgumentException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
            }
        }
        List<Request> requestList = requestRepository.findAllWithFilter(senderId, receiverId, requestTimeFrom, requestTimeTo);
        if (requestList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return requestList;
    }

    @Override
    public Request getById(Long id) throws ResourceNotFoundException {
        return requestRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        Request request = getById(id);
        request.setIsDeleted(Boolean.TRUE);
        requestRepository.save(request);
    }

    @SuppressWarnings("deprecation")
    @Override
    public Request add(AddRequestRequestDto requestDto) throws ResourceNotFoundException, ActivityNotAllowedException {
        Date date = new Date();
        Schedule schedule = scheduleServiceImpl.getById(requestDto.getScheduleId());
        User sender = authenticationServiceImpl.getCurrentUser();
        LectureSubject lectureSubject = lectureSubjectServiceImpl.getLectureSubjectBySubjectAndClass(schedule.getSubject(), schedule.getKelas());
        User receiver = userServiceImpl.getById(lectureSubject.getLecture().getUser().getId());
        ERequestType requestType = ERequestType.valueOf(requestDto.getType());
        
        if (requestType.equals(ERequestType.RESCHEDULE)) {
            JSONObject requestDataJson = new JSONObject(requestDto.getRequestData());
            requestDataJson.get("timeStart");
            requestDataJson.get("timeEnd");
            if (Objects.isNull(requestDto.getRequestData())) {
                throw new HttpMessageNotReadableException(EResponseMessage.MISSING_PARAM.getMessage());
            } 
            Role senderRole = roleRepository.findByName(ERole.LECTURE).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
            if (sender.getRoles().contains(senderRole)) {
                Student student = studentServiceImpl.getStudentLeaderByClass(schedule.getKelas().getId());
                receiver = userServiceImpl.getById(student.getUser().getId());
            } else {
                Student student = studentServiceImpl.getByUser(sender);
                if (Boolean.FALSE.equals(student.getIsLeader())) {
                    throw new ActivityNotAllowedException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
                }
            }
        } else {
            if (Objects.nonNull(requestDto.getRequestData())) {
                throw new HttpMessageNotReadableException(EResponseMessage.MISSING_PARAM.getMessage());
            } 
        }

        Request request = Request.builder()
            .reason(requestDto.getReason())
            .requestTime(date)
            .approveTime(null)
            .isApproved(false)
            .requestData(requestDto.getRequestData())
            .status(EStatus.PENDING)
            .type(requestType)
            .schedule(schedule)
            .sender(sender)
            .receiver(receiver)
            .build();
            return requestRepository.save(request);
    }

    @Transactional(rollbackFor = {ResourceNotFoundException.class, NoAccessException.class, ActivityNotAllowedException.class, ParseException.class})
    @Override
    public Request approve(Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException, ParseException {
        User user = authenticationServiceImpl.getCurrentUser();
        Request request = getById(id);
        if (!request.getReceiver().equals(user)) {
            throw new NoAccessException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
        }
        if (!request.getStatus().equals(EStatus.PENDING)) {
            throw new ActivityNotAllowedException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
        }
        if (request.getType().equals(ERequestType.RESCHEDULE)) {
            SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.sss");
            Schedule schedule = request.getSchedule();
            JSONObject requestDataJson = new JSONObject(request.getRequestData());
            String startTimeStr = (String) requestDataJson.get("timeStart");
            String endTimeStr = (String) requestDataJson.get("timeEnd");
            Date newStartTime = sdf.parse(startTimeStr);
            Date newEndTime = sdf.parse(endTimeStr);
            Calendar startTime = Calendar.getInstance();
            Calendar endTime = Calendar.getInstance();
            startTime.setTime(newStartTime);
            endTime.setTime(newEndTime);
            schedule.setTimeStart(startTime);
            schedule.setTimeEnd(endTime);
            scheduleServiceImpl.edit(schedule);
        }
        request.setApproveTime(new Date());
        request.setStatus(EStatus.APPROVED);
        request.setIsApproved(true);

        return requestRepository.save(request);
    }

    @Override
    public Request reject(Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException {
        User user = authenticationServiceImpl.getCurrentUser();
        Request request = getById(id);
        if (!request.getReceiver().equals(user)) {
            throw new NoAccessException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
        }
        if (!request.getStatus().equals(EStatus.PENDING)) {
            throw new ActivityNotAllowedException(EResponseMessage.ACTIVITY_NOT_ALLOWED.getMessage());
        }
        request.setStatus(EStatus.REJECTED);

        return requestRepository.save(request);
    }
    
}
