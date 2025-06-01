package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;

public interface UserService {
    List<User> getAll() throws ResourceNotFoundException;

    User getById(Long id) throws ResourceNotFoundException;

    User getByEmail(String emailAddress) throws ResourceNotFoundException;

    boolean existsUsername(String username);

    boolean existsByEmail(String emailAddress);

    boolean existByPhoneNumber(String phoneNumber);

    UserResponseDto edit(Long id, EditUserRequestDto requestDto) throws ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
