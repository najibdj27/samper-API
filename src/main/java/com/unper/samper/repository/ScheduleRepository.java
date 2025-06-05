package com.unper.samper.repository;

import java.util.Calendar;
import java.util.List;
import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Class;
import com.unper.samper.model.Schedule;

public interface ScheduleRepository extends JpaRepository<Schedule, Long>{
    
    @Query(
        value = "SELECT * FROM schedule.schedule s " + "\n" + 
        "WHERE s.class_id = :classId" + "\n" + 
        "AND (:filterDateFrom is null or s.time_start >= to_date(:filterDateFrom, 'YYYY-MM-DD')) " + "\n" + 
        "AND (:filterDateTo is null or s.time_start <= to_date(:filterDateTo, 'YYYY-MM-DD'))", 
        nativeQuery= true
    )
    List<Schedule> findAllByStudent(@Param("filterDateFrom") String filterDateFrom, @Param("filterDateTo") String filterDateTo, @Param("classId") Long classId);

    @Query(
        value = "SELECT s.* FROM schedule.schedule s" + "\n" +
        "JOIN schedule.lecture_subject ls ON ls.subject_id = s.subject_id" + "\n" +
        "JOIN schedule.lecture_subject_class lsc ON lsc.lecture_subject_id = ls.id " + "\n" +
        "WHERE lsc.lecture_subject_id IN (SELECT id FROM lecture_subject WHERE lecture_id = :lectureId)" + "\n" +
        "AND s.class_id = lsc.class_id" + "\n" +
        "AND (:filterDateFrom IS null OR s.time_start >= to_date(:filterDateFrom, 'YYYY-MM-DD'))" + "\n" +
        "AND (:filterDateTo IS null OR s.time_end <= to_date(:filterDateTo, 'YYYY-MM-DD'))",
        nativeQuery = true
    )
    List<Schedule> findAllByLecture(@Param("filterDateFrom") String filterDateFrom, @Param("filterDateTo") String filterDateTo, @Param("lectureId") Long lectureId);

    @Query("SELECT count(s)>0 FROM Schedule s" + "\n" + 
        "WHERE s.kelas = :kelas" + "\n" + 
        "AND ((s.timeStart BETWEEN :timeStart AND :timeEnd) or (s.timeEnd BETWEEN :timeStart AND :timeEnd))"
    )
    Boolean existsByTime(@Param("kelas") Class kelas, @Param("timeStart") Calendar timeStart, @Param("timeEnd") Calendar timeEnd);

    @Query("SELECT count(s)>0 FROM Schedule s" + "\n" + 
        "WHERE s.id = :id AND (CURRENT_TIMESTAMP BETWEEN s.timeStart AND s.timeEnd)")
    Boolean isAvailable(@Param("id") Long id);

    @Override
    @Query(value = "SELECT * FROM schedule.schedule" + "\n" + 
        "WHERE is_deleted = false", 
        nativeQuery = true
    )
    List<Schedule> findAll();

    @Override
    @Query(value = "SELECT * FROM schedule.schedule"+ "\n" + 
        "WHERE is_deleted = false AND id = :id", 
        nativeQuery = true
    )
    Optional<Schedule> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE schedule.schedule SET is_deleted = true" + "\n" + 
        "WHERE id = :id", 
        nativeQuery = true
    )
    void deleteById(@Param("id") Long id);
}
