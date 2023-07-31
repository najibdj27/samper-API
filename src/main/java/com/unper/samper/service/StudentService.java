package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.model.dto.StudentResponseDto;

public interface StudentService {
    List<StudentResponseDto> getAll() throws ResourceNotFoundException;

    Student getById(Long id) throws ResourceNotFoundException;

    Student getByUser(User user) throws ResourceNotFoundException;

    Student getCurrentStudent() throws ResourceNotFoundException;

    Student add(AddStudentRequestDto requestDto);

    void delete(Long id);
}
