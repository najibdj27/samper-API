package com.unper.samper.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.LectureSubjectRepository;
import com.unper.samper.service.LectureSubjectService;

@Service
public class LectureSubjectServiceImpl implements LectureSubjectService {

    @Autowired
    LectureSubjectRepository lectureSubjectRepository;

    @Override
    public LectureSubject getLectureSubjectBySubjectAndClass(Subject subject, Class kelas) throws ResourceNotFoundException {
        LectureSubject lectureSubject = lectureSubjectRepository.findBySubjectAndClasses(subject, kelas).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return lectureSubject;
    }
    
}
