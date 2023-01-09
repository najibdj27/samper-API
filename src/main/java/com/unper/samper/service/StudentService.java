package com.unper.samper.service;

import com.unper.samper.domain.dao.User;
import com.unper.samper.domain.dto.EditUserRequestDTO;
import com.unper.samper.exception.ResourceNotFoundException;

public interface StudentService {
    User get(Long userId) throws ResourceNotFoundException;

    // List<User> getAll() throws ResourceNotFoundException;

    User edit(Long userId, EditUserRequestDTO requestDTO) throws ResourceNotFoundException;

    void delete(Long userId);
}
