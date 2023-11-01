package com.unper.samper.service.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddSubjectLecturesRequestDto;
import com.unper.samper.model.dto.AddSubjectrequestDto;
import com.unper.samper.repository.SubjectRepository;
import com.unper.samper.service.SubjectService;

@Service
public class SubjectServiceImpl implements SubjectService {
    @Autowired
    SubjectRepository subjectRepository;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Override
    public List<Subject> getAll() throws ResourceNotFoundException {
        List<Subject> subjectList = subjectRepository.findAll();
        if (subjectList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return subjectList;
    }

    @Override
    public Subject getById(Long id) throws ResourceNotFoundException {
        Optional<Subject> subject = subjectRepository.findById(id); 

        if (Boolean.FALSE.equals(subject.isPresent())) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        return subject.get();
    }

    @Override
    public List<Subject> getByMajor(Long majorId) throws ResourceNotFoundException {
        List<Subject> subjectList = subjectRepository.findAllByMajor(majorId);
        if(subjectList.isEmpty()){
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        return subjectList;
    }

    @Override
    public Subject addSubject(AddSubjectrequestDto requestDto) throws ResourceAlreadyExistException {
        
        if (Boolean.TRUE.equals(subjectRepository.existsByName(requestDto.getName()))) {
            throw new ResourceAlreadyExistException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        Subject subject = Subject.builder()
            .name(requestDto.getName())
            .majors(null)   
            .lectures(null)
            .build();
        
        Subject newSubject = subjectRepository.save(subject);

        return newSubject;
    }

    @Override
    public Subject addSubjectLecture(AddSubjectLecturesRequestDto requestDto) throws ResourceNotFoundException {
        Subject oldSubject = subjectRepository.findById(requestDto.getId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        List<Lecture> lectureList = lectureServiceImpl.getAllById(requestDto.getLectures());
        Set<Lecture> lectureSet = new HashSet<Lecture>(lectureList);
        Subject subject = Subject.builder()
            .id(oldSubject.getId())
            .name(oldSubject.getName())
            .majors(oldSubject.getMajors())
            .lectures(lectureSet)
            .build();
        
        Subject newSubject = subjectRepository.save(subject);
        return newSubject;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        subjectRepository.deleteById(id);
    }

}
