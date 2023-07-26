package com.unper.samper.service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.dto.AddClassRequestDto;

public interface ClassService {
    Class getById(Long id) throws ResourceNotFoundException;

    Class addClass(AddClassRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;
}
