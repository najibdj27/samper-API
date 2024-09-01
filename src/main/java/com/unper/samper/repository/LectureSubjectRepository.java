package com.unper.samper.repository;

import java.util.Optional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.LectureSubject;
import com.unper.samper.model.Subject;
import com.unper.samper.model.Class;
import com.unper.samper.model.Lecture;

public interface LectureSubjectRepository extends JpaRepository<LectureSubject, Long> {
    Optional<LectureSubject> findBySubjectAndClasses(Subject subject, Class kelas);

    Optional<LectureSubject> findBySubjectAndLecture(Subject subject, Lecture lecture);

    @Query(value = "SELECT count(*)>0  FROM lecture_subject_class lsc WHERE lsc.lecture_subject_id = :lectureSubjectId AND lsc.class_id = :classId", nativeQuery = true)
    Boolean existsByLectureAndSubjectAndClasses(@Param("lectureSubjectId") Long lectureSubject, @Param("classId") Long classId);

    @Modifying
    @Query(value = "INSERT INTO public.lecture_subject_class (lecture_subject_id, class_id) VALUES(:lectureSubjectId, :classId)", nativeQuery = true)
    void addLectureSubjectClass(@Param("lectureSubjectId") Long lectureSubjectId, @Param("classId") Long classId);

}
