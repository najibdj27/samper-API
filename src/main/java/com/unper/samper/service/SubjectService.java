package com.unper.samper.service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Subject;
import com.unper.samper.model.dto.AddSubjectrequestDto;

public interface SubjectService {
    Subject getById(Long id) throws ResourceNotFoundException;

    Subject addSubject(AddSubjectrequestDto requestDto) throws ResourceAlreadyExistException;
}
