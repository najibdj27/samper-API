package com.unper.samper.service.impl;

import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;

import javax.mail.MessagingException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.unper.samper.config.JwtUtils;
import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.ExternalAPIException;
import com.unper.samper.exception.InvalidTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.RefreshToken;
import com.unper.samper.model.Role;
import com.unper.samper.model.Token;
import com.unper.samper.model.User;
import com.unper.samper.model.common.UserDetailsImpl;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.EType;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.RefreshTokenRequestDto;
import com.unper.samper.model.dto.RefreshTokenResponseDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SendEmailOTPRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.AuthenticationService;
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
    RefreshTokenServiceImpl refreshTokenServiceImpl;

    @Autowired
    TokenServiceImpl tokenServiceImpl;

    @Autowired
    JwtUtils jwtUtils;

    @Autowired
    OTPServiceImpl otpService;

    @Autowired
    EmailSender emailSender;

    @Autowired
    ExternalAPIServiceImpl externalAPIServiceImpl;

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
        String jwt = jwtUtils.generateJwtToken(authentication);
        RefreshToken refreshToken = refreshTokenServiceImpl.createRefreshToken(user.getId());
        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        List<String> roles = userDetails.getAuthorities().stream().map(item -> item.getAuthority()).collect(Collectors.toList());
        return new JwtResponseDto(jwt, refreshToken.getToken(), userDetails.getId(), roles);
    }

    @Override
    public RefreshTokenResponseDto refreshAuthToken(RefreshTokenRequestDto requestDto) throws ResourceNotFoundException, InvalidTokenException {
        RefreshToken token = refreshTokenServiceImpl.verifyTokenExpiration(requestDto.getRefreshToken());
        String jwt = jwtUtils.refreshJwtToken(token);
        // UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        // List<String> roles = userDetails.getAuthorities().stream().map(item -> item.getAuthority()).collect(Collectors.toList());
        return new RefreshTokenResponseDto(jwt);
    }

    @Override
    public void sendChangePasswordOTP(SendEmailOTPRequestDto requestDto) throws ResourceNotFoundException, MessagingException {
        if (!userRepository.existsByEmail(requestDto.getEmailAddress())) {
            throw new ResourceNotFoundException("User with email " + requestDto.getEmailAddress() + " does not exist!");
        }
        String emailAddress = requestDto.getEmailAddress();
        int otp = otpService.generateOTP(emailAddress);
        emailSender.sendOtpMessage(emailAddress, "SAMPER Reset Password Request", String.valueOf(otp));
    }

    
    @Override
    public ConfirmOTPResponseDto confirmRegistrationOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException {
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
    public User registerUser(SignUpRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException, ExternalAPIException, JsonMappingException, JsonProcessingException {
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
            .facesetToken(null)
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
        
        if (!requestDto.getFaceData().isEmpty()) {
            ResponseEntity<String> faceDetectResponse = externalAPIServiceImpl.faceplusplusDetect(requestDto.getFaceData());
            ObjectMapper faceDetectMapper = new ObjectMapper();
            JsonNode faceDetectRoot =  faceDetectMapper.readTree(faceDetectResponse.getBody());
            JsonNode facesNode  = faceDetectRoot.path("faces");
            
            if (facesNode.isArray() && facesNode.size() == 1) {
                JsonNode faceData = facesNode.get(0);
                String faceToken = faceData.path("face_token").toString();
                ResponseEntity<String> facesetCreateResponse = externalAPIServiceImpl.faceplusplusCreateFaceSet(user.getId(), user.getUsername(), faceToken);
                ObjectMapper facesetCreateMapper = new ObjectMapper();
                JsonNode facesetCreateRoot = facesetCreateMapper.readTree(facesetCreateResponse.getBody());
                String faceSetToken = facesetCreateRoot.path("faceset_token").asText();
                user.setFacesetToken(faceSetToken);
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

