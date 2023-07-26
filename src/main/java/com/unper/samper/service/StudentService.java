package com.unper.samper.service;

import java.util.List;

import org.springframework.http.ResponseEntity;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Student;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.StudentResponseDto;

public interface StudentService {
    List<StudentResponseDto> getAll() throws ResourceNotFoundException;

    ResponseEntity<?> getById(Long id);

    Student add(AddStudentRequestDto requestDto);

    ResponseEntity<?> delete(Long id);
}
