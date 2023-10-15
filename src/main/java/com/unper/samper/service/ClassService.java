package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.dto.AddClassRequestDto;

public interface ClassService {
    List<Class> getAll() throws ResourceNotFoundException;

    Class getById(Long id) throws ResourceNotFoundException;

    Class addClass(AddClassRequestDto requestDto) throws ResourceAlreadyExistException, ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
