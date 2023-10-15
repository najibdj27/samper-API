package com.unper.samper.service.impl;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddSubjectrequestDto;
import com.unper.samper.repository.SubjectRepository;
import com.unper.samper.service.SubjectService;

@Service
public class SubjectServiceImpl implements SubjectService {
    @Autowired
    SubjectRepository subjectRepository;

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
    public Subject addSubject(AddSubjectrequestDto requestDto) throws ResourceAlreadyExistException {
        
        if (Boolean.TRUE.equals(subjectRepository.existsByName(requestDto.getName()))) {
            throw new ResourceAlreadyExistException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        Subject subject = Subject.builder()
            .name(requestDto.getName())
            .build();
        
        Subject newSubject = subjectRepository.save(subject);

        return newSubject;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        Subject subject = getById(id);
        subjectRepository.delete(subject);
    }

}
