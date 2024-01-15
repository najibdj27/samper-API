package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;

import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.model.Class;

public interface LectureSubjectRepository extends JpaRepository<LectureSubject, Long> {
    Optional<LectureSubject> findBySubjectAndClasses(Subject subject, Class kelas);
}
