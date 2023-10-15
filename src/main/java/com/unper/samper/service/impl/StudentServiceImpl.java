package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.repository.StudentRepository;
import com.unper.samper.service.StudentService;

@Service
public class StudentServiceImpl implements StudentService {
    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    UserServiceImpl userServiceImpl;

    @Autowired
    StudentRepository studentRepository;

    @Override
    public List<Student> getAll() throws ResourceNotFoundException {
        List<Student> studentList = studentRepository.findAll();

        // check if data available
        if (studentList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        

        return studentList;
    }

    @Override
    public Student getById(Long id) throws ResourceNotFoundException {
        Student student = studentRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return student;
    }

    @Override
    public Student getByUser(User user) throws ResourceNotFoundException {
        Student student = studentRepository.findByUser(user).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return student;
    }

    @Override
    public Student getCurrentStudent() throws ResourceNotFoundException {
        User user = authenticationServiceImpl.getCurrentUser();
        Student student = getByUser(user);
        return student;
    }

    @Override
    public Student add(AddStudentRequestDto requestDto) {
        Student student = Student.builder()
            .NIM(requestDto.getNIM())
            .kelas(requestDto.getKelas())
            .user(requestDto.getUser())
            .build();
        Student newStudent = studentRepository.save(student);
        return newStudent;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        Student student = getById(id);
        userServiceImpl.delete(student.getUser().getId());
    }
}
