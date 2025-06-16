package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.dto.AddStudentRequestDto;

public interface StudentService {
    List<Student> getAll() throws ResourceNotFoundException;

    List<Student> getAllByClass(Long classId) throws ResourceNotFoundException;

    List<Student> getAllByLectureAndClass(Long lectureId, Long classId) throws ResourceNotFoundException;

    Student getById(Long id) throws ResourceNotFoundException;

    Student getByUser(User user) throws ResourceNotFoundException;

    Student getCurrentStudent() throws ResourceNotFoundException;

    Student getStudentLeaderByClass(Long classId) throws ResourceNotFoundException;

    Boolean existsByNIM(String nim);

    Student add(AddStudentRequestDto requestDto);

    void delete(Long id) throws ResourceNotFoundException;
}
