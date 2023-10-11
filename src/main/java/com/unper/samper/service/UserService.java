package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;

public interface UserService {
    List<User> getAll() throws ResourceNotFoundException;

    User getById(Long id) throws ResourceNotFoundException;

    UserResponseDto edit(Long id, EditUserRequestDto requestDto) throws ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
