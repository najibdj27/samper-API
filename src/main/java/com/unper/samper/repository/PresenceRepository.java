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
import com.unper.samper.model.Student;

public interface PresenceRepository extends JpaRepository<Presence, Long> {
    @Query("SELECT COUNT(p)>0 FROM Presence p WHERE p.student = :student AND p.checkOut IS NULL")
    Boolean isOnSchedule(@Param("student") Student student);

    @Query("SELECT p FROM Presence p WHERE p.schedule.kelas.lecture = :lecture")
    List<Presence> findByLecture(@Param("lecture") Lecture lecture);

    @Override
    @Query(value = "SELECT * FROM public.presence WHERE is_deleted = false", nativeQuery = true)
    List<Presence> findAll();

    @Override
    @Query(value = "SELECT * FROM public.presence WHERE is_deleted = true AND id = :id")
    Optional<Presence> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE public.presence SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
