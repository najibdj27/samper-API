package com.unper.samper.service.impl;

import java.io.IOException;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import com.unper.samper.config.JwtUtils;
import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.TemplateNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.EmailTemplate;
import com.unper.samper.model.Role;
import com.unper.samper.model.Token;
import com.unper.samper.model.User;
import com.unper.samper.model.common.UserDetailsImpl;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.model.constant.EType;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.RegisterUserRequestDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.AuthenticationService;
import com.unper.samper.service.EmailTemplateService;
import com.unper.samper.service.ExternalAPIService;
import com.unper.samper.util.EmailSender;


@Service
public class AuthenticationServiceImpl implements AuthenticationService {
    @Autowired
    AuthenticationManager authenticationManager;

    @Autowired
    UserRepository userRepository;

    @Autowired
    RoleRepository roleRepository;

    @Autowired
    PasswordEncoder encoder;

    @Autowired
    TokenServiceImpl tokenServiceImpl;

    @Autowired
    JwtUtils jwtUtils;

    @Autowired
    OTPServiceImpl otpService;

    @Autowired
    EmailSender emailSender;

    @Autowired
    ExternalAPIService externalAPIService;

    @Autowired
    EmailTemplateService emailTemplateService;

    @Value("${com.unper.samper.domain}")
    String domain;

    @Override
    public JwtResponseDto authenticateUser(SignInRequestDto requestDto) throws SignInFailException, ResourceNotFoundException {
        User user = userRepository.findByUsername(requestDto.getUsername()).orElseThrow(() -> new SignInFailException("Username or password is wrong!"));
        Boolean isPasswordCorrect = encoder.matches(requestDto.getPassword(), user.getPassword());
        if (Boolean.FALSE.equals(userRepository.existsByUsername(requestDto.getUsername()))) {
            throw new SignInFailException("Username or password is wrong!");
        }
        if (Boolean.FALSE.equals(isPasswordCorrect)) {
            throw new SignInFailException("Username or password is wrong!");
        }
        Authentication authentication = authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(requestDto.getUsername(), requestDto.getPassword()));
        SecurityContextHolder.getContext().setAuthentication(authentication);
        Map<String,String> jwt = jwtUtils.generateJwtToken(authentication);
        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        List<String> roles = userDetails.getAuthorities().stream().map(item -> item.getAuthority()).collect(Collectors.toList());
        JwtResponseDto jwtResponseDto = JwtResponseDto.builder()
            .accessToken(jwt.get("accessToken"))
            .refreshToken(jwt.get("refreshToken"))
            .userId(userDetails.getId())
            .roles(roles)
            .build();
        return jwtResponseDto;
    }

    @Override
    public String refreshAuthToken(String refreshToken) throws ExpiredTokenException {
        
        return jwtUtils.refreshAccessToken(refreshToken);
    }

    @Override
    public void sendChangePasswordOTP(SendEmailOTPRequestDto requestDto) throws ResourceNotFoundException, MessagingException, TemplateNotFoundException {
        if (!userRepository.existsByEmail(requestDto.getEmailAddress())) {
            throw new ResourceNotFoundException("User with email " + requestDto.getEmailAddress() + " does not exist!");
        }
        String emailAddress = requestDto.getEmailAddress();
        int otpCode = otpService.generateOTP(emailAddress);
        EmailTemplate emailTemplate = emailTemplateService.getByName("RESET_PASSWORD_OTP");
        Map<String, String> emailTemplateParams = new HashMap<>();
        emailTemplateParams.put("otp_code", String.valueOf(otpCode));
        emailSender.sendEmailWithTemplate(emailAddress, emailTemplate, emailTemplateParams);
    }

    
    @Override
    public ConfirmOTPResponseDto confirmResetPasswordOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException {
        return otpService.confirmOTP(requestDto.getKey(), requestDto.getOtp(), EType.RESET_PASSWORD);
    }

    @Override
    public void resetPassword(UUID token , ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException {
        Date now = Calendar.getInstance().getTime();
        Token resetPasswordToken = tokenServiceImpl.getByKey(token.toString());
        if (Boolean.TRUE.equals(tokenServiceImpl.isExpired(resetPasswordToken.getId(), now))) {
            throw new ExpiredTokenException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        Optional<User> optionalUser = userRepository.findByEmail(resetPasswordToken.getKey());
        User user = optionalUser.get();
        user.setPassword(encoder.encode(requestDto.getNewPassword()));
        userRepository.save(user);
        tokenServiceImpl.deleteByKey(resetPasswordToken.getKey());
    }

    @Override
    public User getCurrentUser() throws ResourceNotFoundException {
        UserDetailsImpl userDetails = (UserDetailsImpl) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        User user = userRepository.findById(userDetails.getId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return user;
    }

    @Override
    public User registerUser(RegisterUserRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, IOException {
        if (userRepository.existsByUsername(requestDto.getUsername())) {
            throw new ResourceAlreadyExistException(EResponseMessage.USERNAME_ALREADY_TAKEN.getMessage());
        }
        if (userRepository.existsByEmail(requestDto.getEmail())) {
            throw new ResourceAlreadyExistException(EResponseMessage.EMAIL_ALREADY_EXIST.getMessage());
        }
        if (userRepository.existsByPhoneNumber(requestDto.getPhoneNumber())) {
            throw new ResourceAlreadyExistException(EResponseMessage.PHONE_NUMBER_ALREADY_EXIST.getMessage());
        }

        User user = User.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(encoder.encode(requestDto.getPassword()))
            .faceToken(null)
            .registeredFaceUrl(null)
            .build();
        Set<Role> roleSet = new HashSet<>();
        requestDto.getRoles().forEach(role -> {
            Role roles = new Role();
            try {
                roles = roleRepository.findByName(role).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.ROLE_NOT_FOUND.getMessage()));
            } catch (ResourceNotFoundException e) {
                e.printStackTrace();
            }
            roleSet.add(roles);
        });
        user.setRoles(roleSet);
        
        if (requestDto.getFaceData() != null && !requestDto.getRoles().contains(ERole.ADMIN)) {
            Map<?,?> faceDetectResponse = externalAPIService.faceplusplusDetect(requestDto.getFaceData());
            
            @SuppressWarnings("unchecked")
            List<Map<?,?>> faceList = (List<Map<?,?>>) faceDetectResponse.get("faces");
            
            if (faceList.size() == 1) {
                String faceToken = (String) faceList.get(0).get("face_token");
                externalAPIService.faceplusplusSetUserId(faceToken, user.getUsername());
                user.setFaceToken(faceToken);
                
                Map<?,?> uploadBase64Image = externalAPIService.cloudinaryUploadBase64Image(requestDto.getFaceData(), "user/registration");
                String registeredFaceUrl = uploadBase64Image.get("secure_url").toString();
                user.setRegisteredFaceUrl(registeredFaceUrl);
            }
        }


        userRepository.save(user);
        
        return user;
    }

    @Override
    public Boolean checkTokenExpiration(String token) {
        return jwtUtils.validateJwtToken(token);
    }
}

