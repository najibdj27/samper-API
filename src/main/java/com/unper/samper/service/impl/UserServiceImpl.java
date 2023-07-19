package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddUserRequestDto;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.UserService;

@Service
public class UserServiceImpl implements UserService {
    @Autowired
    UserRepository userRepository;

    @Override
    public ResponseEntity<?> getAll() throws ResourceNotFoundException {
        List<User> users = userRepository.findAll();

        // check if list of User is empty
        if (users.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        List<UserResponseDto> responseDtoList = new ArrayList<>();
        users.forEach(user -> {
            UserResponseDto responseDto = UserResponseDto.builder()
                .id(user.getId())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .dateOfBirth(user.getDateOfBirth())
                .username(user.getUsername())
                .email(user.getEmail())
                .phoneNumber(user.getPhoneNumber())
                .build();
            responseDtoList.add(responseDto);
        });
        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), metaData, responseDtoList);
    }

    @Override
    public ResponseEntity<?> getById(Long id) throws ResourceNotFoundException {
        Optional<User> user = userRepository.findById(id);

        // check if user with that ID not exist in DB
        if (user.isEmpty()){
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        UserResponseDto responseDto = UserResponseDto.builder()
            .id(user.get().getId())
            .firstName(user.get().getFirstName())
            .lastName(user.get().getLastName())
            .dateOfBirth(user.get().getDateOfBirth())
            .username(user.get().getUsername())
            .email(user.get().getEmail())
            .phoneNumber(user.get().getPhoneNumber())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Override
    public ResponseEntity<?> add(AddUserRequestDto requestDto) throws ResourceAlreadyExistException {
        User user = User.builder()
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .username(requestDto.getUsername())
            .email(requestDto.getEmail())
            .phoneNumber(requestDto.getPhoneNumber())
            .password(requestDto.getPassword())
            .build();

        // check if username or email already exist in DB
        if(Boolean.TRUE == userRepository.existsByUsernameIgnoreCaseOrEmailIgnoreCaseOrPhoneNumber(user.getUsername(), user.getEmail(), user.getPhoneNumber())) {
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }

        User newUser = userRepository.save(user);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.INSERT_DATA_SUCCESS.getMessage(), newUser);
    }

    @Override
    public ResponseEntity<?> edit(Long id, EditUserRequestDto requestDto) throws ResourceNotFoundException {
        User user = User.builder()
            .id(id)
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .build();

        // check if user exists in DB
        if (Boolean.FALSE == userRepository.existsById(id)) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        User editedUser = userRepository.save(user);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), editedUser);
    }

    @Override
    public ResponseEntity<?> changePassword(Long id, String password, String passwordValidation) throws ResourceNotFoundException, Exception {
        User user = User.builder()
            .id(id)
            .password(password)
            .build();
        
        // check if user exists in DB
        if (Boolean.FALSE == userRepository.existsById(id)) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        // check if password validation not match
        if (password != passwordValidation) {
            throw new Exception(EResponseMessage.PASSWORD_NOT_MATCH.getMessage());
        }

        userRepository.save(user);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), null);
    }
    
}
