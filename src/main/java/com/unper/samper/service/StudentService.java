package com.unper.samper.service;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.dto.AddStudentRequestDto;

public interface StudentService {
    ResponseEntity<?> getAll() throws ResourceNotFoundException;

    ResponseEntity<?> getById(Long id);

    ResponseEntity<?> add(AddStudentRequestDto requestDto);

    ResponseEntity<?> delete(Long id);
}
