package com.unper.samper.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.dto.AddUserRequestDto;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;

public interface UserService {
    ResponseEntity<?> getAll() throws ResourceNotFoundException;

    UserResponseDto getById(Long id) throws ResourceNotFoundException;

    ResponseEntity<?> add(AddUserRequestDto requestDto) throws ResourceAlreadyExistException;

    ResponseEntity<?> edit(Long id, EditUserRequestDto requestDto) throws ResourceNotFoundException;

    ResponseEntity<?> changePassword(Long id, String password, String passwordValidation) throws ResourceNotFoundException, Exception;
}
