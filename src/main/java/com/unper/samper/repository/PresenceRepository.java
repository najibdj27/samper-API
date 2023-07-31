package com.unper.samper.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Lecture;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Student;

public interface PresenceRepository extends JpaRepository<Presence, Long> {
    @Query("SELECT COUNT(p)>0 FROM Presence p WHERE p.student = :student AND p.checkOut IS NULL")
    Boolean isOnSchedule(@Param("student") Student student);

    @Query("SELECT p FROM Presence p WHERE p.schedule.kelas.lecture = :lecture")
    List<Presence> findByLecture(@Param("lecture") Lecture lecture);
}
