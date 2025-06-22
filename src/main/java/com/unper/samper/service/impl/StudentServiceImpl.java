package com.unper.samper.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Student;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddStudentRequestDto;
import com.unper.samper.repository.StudentRepository;
import com.unper.samper.service.ClassService;
import com.unper.samper.service.LectureService;
import com.unper.samper.service.StudentService;

@Service
public class StudentServiceImpl implements StudentService {
    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Autowired
    UserServiceImpl userServiceImpl;

    @Autowired
    ClassService classService;

    @Autowired
    LectureService lectureService;

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
    public List<Student> getAllByClass(Long classId) throws ResourceNotFoundException {
        Class kelas = classService.getById(classId); 
        return studentRepository.findAllByClass(kelas);
    }

    
    @Override
    public List<Student> getAllByLectureAndClass(Long lectureId, Long classId) throws ResourceNotFoundException {
        Lecture lecture = lectureService.getById(lectureId);
        Class kelas = classService.getById(classId);
        return studentRepository.findAllByLectureAndClass(lecture, kelas);
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
    public Boolean existsByNIM(String nim){
        return studentRepository.existsByNIM(nim);
    }

    @Override
    public Student add(AddStudentRequestDto requestDto) {
        Student student = Student.builder()
            .NIM(requestDto.getNIM())
            .kelas(requestDto.getKelas())
            .user(requestDto.getUser())
            .isLeader(requestDto.getIsLeader())
            .isActive(requestDto.getIsActive())
            .build();
        Student newStudent = studentRepository.save(student);
        return newStudent;
    }

    @Override
    public Student setAsLeader(Long studentId) throws ResourceNotFoundException {
        Student student = getById(studentId);
        List<Student> studentList = getAllByClass(student.getKelas().getId());
        Optional<Student> currentLeader = Optional.ofNullable(studentList.stream().filter(s -> Boolean.TRUE.equals(s.getIsLeader())).findFirst().orElse(null));
        if (currentLeader.isPresent()) {
            currentLeader.get().setIsLeader(false);
            studentRepository.save(currentLeader.get());
        }
        student.setIsLeader(true);
        
        return studentRepository.save(student);
    }   

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        Student student = getById(id);
        userServiceImpl.delete(student.getUser().getId());
    }

    @Override
    public Student getStudentLeaderByClass(Long classId) throws ResourceNotFoundException {
        return studentRepository.findStudentLeaderByClass(classId).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
    }
}
