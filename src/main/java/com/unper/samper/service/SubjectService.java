package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Subject;
import com.unper.samper.model.dto.AddSubjectLecturesRequestDto;
import com.unper.samper.model.dto.AddSubjectrequestDto;

public interface SubjectService {
    List<Subject> getAll() throws ResourceNotFoundException;

    Subject getById(Long id) throws ResourceNotFoundException;

    List<Subject> getByMajor(Long majorId) throws ResourceNotFoundException;

    Subject addSubject(AddSubjectrequestDto requestDto) throws ResourceAlreadyExistException;

    Subject addSubjectLecture(AddSubjectLecturesRequestDto requestDto) throws ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
