package com.unper.samper.repository;

import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Lecture;
import com.unper.samper.model.Presence;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.Student;

public interface PresenceRepository extends JpaRepository<Presence, Long> {
    @Query("SELECT count(p)>0 FROM Presence p WHERE p.student = :student AND p.schedule = :schedule AND type ='O'")
    Boolean isCheckOut(@Param("student") Student student, @Param("schedule") Schedule schedule);

    @Query("SELECT count(p)>0 FROM Presence p WHERE p.student = :student AND p.schedule = :schedule AND type ='I'")
    Boolean inOnCurrentSchedule(@Param("student") Student student, @Param("schedule") Schedule schedule);

    @Query("SELECT p FROM Presence p WHERE p.schedule.kelas.lecture = :lecture")
    List<Presence> findByLecture(@Param("lecture") Lecture lecture);

    @Query(value = "SELECT * FROM schedule.presence p WHERE p.student_id = :studentId LIMIT :limit", nativeQuery = true)
    List<Presence> findByStudent(@Param("studentId") Long studentId, @Param("limit") Integer limit);

    Presence findByStudentAndScheduleAndType(Student student, Schedule schedule, Character Type);

    @Override
    @Query(value = "SELECT * FROM schedule.presence WHERE is_deleted = false", nativeQuery = true)
    List<Presence> findAll();

    @Override
    @Query(value = "SELECT * FROM schedule.presence WHERE is_deleted = false AND id = :id", nativeQuery = true)
    Optional<Presence> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE schedule.presence SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
