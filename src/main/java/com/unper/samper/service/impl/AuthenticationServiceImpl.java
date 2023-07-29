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

import org.apache.commons.lang3.time.DateUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import com.unper.samper.config.JwtUtils;
import com.unper.samper.exception.ExpiredTokenException;
import com.unper.samper.exception.PasswordNotMatchException;
import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.SignInFailException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.ResetPasswordToken;
import com.unper.samper.model.Role;
import com.unper.samper.model.User;
import com.unper.samper.model.common.UserDetailsImpl;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.ERole;
import com.unper.samper.model.dto.ConfirmOTPRequestDto;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;
import com.unper.samper.model.dto.ForgetPasswordRequestDto;
import com.unper.samper.model.dto.JwtResponseDto;
import com.unper.samper.model.dto.ResetPasswordRequestDto;
import com.unper.samper.model.dto.SignInRequestDto;
import com.unper.samper.model.dto.SignUpRequestDto;
import com.unper.samper.repository.ResetPasswordTokenRepository;
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
    ResetPasswordTokenRepository resetPasswordTokenRepository;

    @Autowired
    PasswordEncoder encoder;

    @Autowired
    JwtUtils jwtUtils;

    @Autowired
    OTPServiceImpl otpService;

    @Autowired
    EmailSender emailSender;

    @Value("${com.unper.samper.domain}")
    String domain;

    @Override
    public ResponseEntity<?> authenticateUser(SignInRequestDto requestDto) throws SignInFailException {
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
        UserDetailsImpl userDetails = (UserDetailsImpl) authentication.getPrincipal();
        List<String> roles = userDetails.getAuthorities().stream().map(item -> item.getAuthority()).collect(Collectors.toList());
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Successfully login!", new JwtResponseDto(jwt, userDetails.getUsername(), roles));
    }

    @Override
    public ResponseEntity<?> changePassword(ForgetPasswordRequestDto requestDto) throws ResourceNotFoundException, MessagingException {
        if (!userRepository.existsByEmail(requestDto.getEmailAddress())) {
            throw new ResourceNotFoundException("User with email " + requestDto.getEmailAddress() + " does not exist!");
        }
        String emailAddress = requestDto.getEmailAddress();
        int otp = otpService.generateOTP(emailAddress);
        emailSender.sendOtpMessage(emailAddress, "SAMPER Reset Password Request", String.valueOf(otp));
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "OTP has been sent to your email!", null);
    }

    @Override
    public ResponseEntity<?> confirmOTP(ConfirmOTPRequestDto requestDto) throws WrongOTPException, ResourceNotFoundException{
        if (otpService.getOTP(requestDto.getEmailAddress()) == 0) {
            throw new ResourceNotFoundException("You have not generated OTP!");
        }else if (otpService.getOTP(requestDto.getEmailAddress()) != requestDto.getOtp()) {
            throw new WrongOTPException("Wrong OTP!");
        }
        resetPasswordTokenRepository.deleteByEmailAddress(requestDto.getEmailAddress());
        Date now = Calendar.getInstance().getTime();
        ResetPasswordToken resetPasswordToken = resetPasswordTokenRepository.save(ResetPasswordToken.builder()
            .emailAddress(requestDto.getEmailAddress())
            .expiredDate(DateUtils.addMinutes(now, 5))
            .build());
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "OTP has been confirmed!", new ConfirmOTPResponseDto(resetPasswordToken.getToken().toString()));
    }

    @Override
    public ResponseEntity<?> resetPassword(UUID token , ResetPasswordRequestDto requestDto) throws PasswordNotMatchException, ResourceNotFoundException, ExpiredTokenException {
        Date now = Calendar.getInstance().getTime();
        Optional<ResetPasswordToken> resetPasswordToken = resetPasswordTokenRepository.findByToken(token);
        if (resetPasswordToken.isEmpty()) {
            throw new ResourceNotFoundException("Token is not valid!");
        }
        if (Boolean.TRUE.equals(resetPasswordTokenRepository.isExpired(resetPasswordToken.get().getId(), now))) {
            throw new ExpiredTokenException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        Optional<User> optionalUser = userRepository.findByEmail(resetPasswordToken.get().getEmailAddress());
        User user = optionalUser.get();
        user.setPassword(encoder.encode(requestDto.getNewPassword()));
        userRepository.save(user);
        resetPasswordTokenRepository.deleteByEmailAddress(resetPasswordToken.get().getEmailAddress());
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, "Password has been reset successfully!", null);
    }

    @Override
    public User getCurrentUser() {
        UserDetailsImpl userDetails = (UserDetailsImpl) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        User user = userRepository.getReferenceById(userDetails.getId());
        return user;
    }

    @Override
    public User registerUser(SignUpRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException {
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
            .build();
        Set<Role> roles = new HashSet<>();
        Role customer = roleRepository.findByName(ERole.ROLE_LECTURE).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.ROLE_NOT_FOUND.getMessage()));
        roles.add(customer);
        user.setRoles(roles);
        userRepository.save(user);
        return user;
    }

    @Override
    @Scheduled(cron = "0 15 12 1/1 * *")
    public void deleteExpiredToken() throws ResourceNotFoundException {
        List<ResetPasswordToken> resetPasswordTokenList = resetPasswordTokenRepository.findExpiredToken();
        if (resetPasswordTokenList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        resetPasswordTokenRepository.deleteAll(resetPasswordTokenList);
        System.out.println(String.valueOf(resetPasswordTokenList.size()) + " Token successfully deleted");
    }
}
