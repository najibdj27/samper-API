package com.unper.samper.service.impl;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.MissingFormatArgumentException;
import java.util.UUID;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.User;
import com.unper.samper.model.Class;
import com.unper.samper.model.Token;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.model.constant.EStatus;
import com.unper.samper.model.constant.EType;
import com.unper.samper.model.dto.AddAdminRequestDto;
import com.unper.samper.model.dto.AddLectureRequestDto;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.RegisterAdminRequestDto;
import com.unper.samper.model.dto.RegisterLectureRequestDto;
import com.unper.samper.model.dto.RegisterStudentRequestDto;
import com.unper.samper.model.dto.RegistrationEligibilityRequestDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;
import com.unper.samper.model.dto.RegisterUserRequestDto;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.service.AdminService;
import com.unper.samper.service.AuthenticationService;
import com.unper.samper.service.ClassService;
import com.unper.samper.service.LectureService;
import com.unper.samper.service.OTPService;
import com.unper.samper.service.RegistrationService;
import com.unper.samper.service.StudentService;
import com.unper.samper.service.TokenService;
import com.unper.samper.service.UserService;
import com.unper.samper.util.EmailSender;

@Service
public class RegistrationServiceImpl implements RegistrationService {

    @Autowired
    AuthenticationService authenticationService;

    @Autowired
    RoleRepository roleRepository;

    @Autowired
    UserService userService;

    @Autowired
    OTPService otpService;

    @Autowired
    EmailSender emailSender;

    @Autowired
    StudentService studentService;

    @Autowired
    LectureService lectureService;

    @Autowired
    AdminService adminService;

    @Autowired
    ClassService classService;

    @Autowired
    TokenService tokenService;

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public void registerStudent(UUID requestToken, RegisterStudentRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, InvalidTokenException, IOException {
        if (requestDto.getFaceData().isEmpty()) {
            throw new MissingFormatArgumentException("Face data is null");
        }
        
        Token token = tokenService.getByKeyAndType(requestDto.getEmail(), EType.REGISTRATION);
        if (!token.getToken().equals(requestToken)) {
            throw new InvalidTokenException(EResponseMessage.TOKEN_INVALID.getMessage());
        }

        if (studentService.existsByNIM(requestDto.getNim())){
            throw new ResourceAlreadyExistException("NIM is already exists");
        }

        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.STUDENT);
        RegisterUserRequestDto signUpRequestDto = RegisterUserRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .faceData(requestDto.getFaceData())
            .roles(eRoleList)
            .build();
        User newUser = authenticationService.registerUser(signUpRequestDto);

        Class kelas = classService.getById(requestDto.getClassId());  
        AddStudentRequestDto addStudentRequestDto = AddStudentRequestDto.builder()
            .user(newUser)
            .kelas(kelas)
            .NIM(requestDto.getNim())
            .isLeader(false)
            .build();
        studentService.add(addStudentRequestDto);
    }

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class})
    public void registerLecture(UUID requestToken, RegisterLectureRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, InvalidTokenException, IOException {
        if (requestDto.getFaceData().isEmpty()) {
            throw new MissingFormatArgumentException("Face data is null");
        }

        Token token = tokenService.getByKeyAndType(requestDto.getEmail(), EType.REGISTRATION);
        if (!token.getToken().equals(requestToken)) {
            throw new InvalidTokenException(EResponseMessage.TOKEN_INVALID.getMessage());
        }

        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.LECTURE);
        RegisterUserRequestDto signUpRequestDto = RegisterUserRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .faceData(requestDto.getFaceData())
            .roles(eRoleList)
            .build();
        User newUser = authenticationService.registerUser(signUpRequestDto);

        AddLectureRequestDto addLectureRequestDto = AddLectureRequestDto.builder()
            .NIP(requestDto.getNIP())
            .user(newUser)
            .build();
        lectureService.add(addLectureRequestDto);
    }

    @Override
    @Transactional(rollbackFor = {ResourceAlreadyExistException.class, ResourceNotFoundException.class, ExternalAPIException.class, IOException.class})
    public void registerAdmin(RegisterAdminRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, IOException {
        List<ERole> eRoleList = new ArrayList<>();
        eRoleList.add(ERole.ADMIN);
        RegisterUserRequestDto signUpRequestDto = RegisterUserRequestDto.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .faceData(null)
            .roles(eRoleList)
            .build();
        User newUser = authenticationService.registerUser(signUpRequestDto);

        AddAdminRequestDto addAdminRequestDto = AddAdminRequestDto.builder()
            .user(newUser)
            .NIP(requestDto.getNIP())
            .previllagesId(requestDto.getPrevillages())
            .build();
        adminService.add(addAdminRequestDto);
    }

    @Override
    public void sendRegistrationOTP(SendEmailOTPRequestDto requestDto) throws MessagingException, ResourceAlreadyExistException {
        if (userService.existsByEmail(requestDto.getEmailAddress())) {
            throw new ResourceAlreadyExistException("User with email " + requestDto.getEmailAddress() + " have been registered!");
        }
        String emailAddress = requestDto.getEmailAddress();
        int otp = otpService.generateOTP(emailAddress);
        emailSender.sendOtpMessage(emailAddress, "SAMPER Registration", String.valueOf(otp));
    }

    @Override
    public ConfirmOTPResponseDto confirmRegistrationOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException {
        return otpService.confirmOTP(requestDto.getKey(), requestDto.getOtp(), EType.REGISTRATION);
    }

    @Override
    public Map<String, Map<String, String>> registrationEligibilityCheck(RegistrationEligibilityRequestDto requestDto) {
        Map<String, Map<String, String>> eligbilityMap = new HashMap<>();
        if (requestDto.getUsername() != null && userService.existsUsername(requestDto.getUsername())) {
            Map<String, String> fieldMap = new HashMap<>();
            fieldMap.put("status", String.valueOf(EStatus.NOT_ELIGIBLE.getCode()));
            fieldMap.put("message", EResponseMessage.USERNAME_ALREADY_TAKEN.getMessage());
            eligbilityMap.put("username", fieldMap);
        }
        if (requestDto.getEmail() != null && userService.existsByEmail(requestDto.getEmail())) {
            Map<String, String> fieldMap = new HashMap<>();
            fieldMap.put("status", String.valueOf(EStatus.NOT_ELIGIBLE.getCode()));
            fieldMap.put("message", EResponseMessage.EMAIL_ALREADY_EXIST.getMessage());
            eligbilityMap.put("email", fieldMap);
        }
        if (requestDto.getPhoneNumber() != null && userService.existByPhoneNumber(requestDto.getPhoneNumber())) {
            Map<String, String> fieldMap = new HashMap<>();
            fieldMap.put("status", String.valueOf(EStatus.NOT_ELIGIBLE.getCode()));
            fieldMap.put("message", EResponseMessage.PHONE_NUMBER_ALREADY_EXIST.getMessage());
            eligbilityMap.put("phoneNumber", fieldMap);
        }
        if (requestDto.getNim() != null && studentService.existsByNIM(requestDto.getNim())) {
            Map<String, String> fieldMap = new HashMap<>();
            fieldMap.put("status", String.valueOf(EStatus.NOT_ELIGIBLE.getCode()));
            fieldMap.put("message", EResponseMessage.NIP_ALREADY_EXIST.getMessage());
            eligbilityMap.put("phoneNumber", fieldMap);
        }
        if (requestDto.getNip() != null && lectureService.existsByNIP(requestDto.getNip())) {
            Map<String, String> fieldMap = new HashMap<>();
            fieldMap.put("status", String.valueOf(EStatus.NOT_ELIGIBLE.getCode()));
            fieldMap.put("message", EResponseMessage.NIM_ALREADY_EXIST.getMessage());
            eligbilityMap.put("phoneNumber", fieldMap);
        }
        return eligbilityMap;
    }
    
}
