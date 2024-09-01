package com.unper.samper.service.impl;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.LectureSubjectRepository;
import com.unper.samper.service.LectureSubjectService;

@Service
public class LectureSubjectServiceImpl implements LectureSubjectService {

    @Autowired
    LectureSubjectRepository lectureSubjectRepository;

    @Lazy
    @Autowired
    ClassServiceImpl classServiceImpl;

    @Override
    public LectureSubject getLectureSubjectBySubjectAndClass(Subject subject, Class kelas) throws ResourceNotFoundException {
        LectureSubject lectureSubject = lectureSubjectRepository.findBySubjectAndClasses(subject, kelas).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return lectureSubject;
    }

    @Override
    public LectureSubject getByLectureAndSubject(Subject subject, Lecture lecture) throws ResourceNotFoundException {
        return lectureSubjectRepository.findBySubjectAndLecture(subject, lecture).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
    }

    @Override
    public LectureSubject getById(Long id) throws ResourceNotFoundException {
        return lectureSubjectRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
    }

    @Override
    public LectureSubject add(Lecture lecture, Subject subject) throws ResourceAlreadyExistException, ResourceNotFoundException {
        LectureSubject lectureSubject = LectureSubject.builder()
            .lecture(lecture)
            .subject(subject)
            .build();
        return lectureSubjectRepository.save(lectureSubject);
    }

    @Override
    public void addClass(Long lectureSubjectId, Long classId) throws ResourceNotFoundException {
        getById(lectureSubjectId);
        classServiceImpl.getById(classId);
        lectureSubjectRepository.addLectureSubjectClass(lectureSubjectId, classId);
    }

    @Override
    public Boolean checkLectureSubjectClass(LectureSubject lectureSubject, Class kelas) {
        return lectureSubjectRepository.existsByLectureAndSubjectAndClasses(lectureSubject.getId(), kelas.getId());
    }

}
