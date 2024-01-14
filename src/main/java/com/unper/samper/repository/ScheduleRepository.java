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
    
    @Query(value = "SELECT * FROM public.schedule s WHERE s.time_start BETWEEN to_date(:filterDateFrom, 'YYYY-MM-DD') AND to_date(:filterDateTo, 'YYYY-MM-DD') AND s.class_id = :classId", nativeQuery= true)
    List<Schedule> findAllWithFilter(@Param("filterDateFrom") String filterDateFrom, @Param("filterDateTo") String filterDateTo, @Param("classId") Long classId);

    @Query("SELECT count(s)>0 FROM Schedule s WHERE s.kelas = :kelas AND ((s.timeStart BETWEEN :timeStart AND :timeEnd) or (s.timeEnd BETWEEN :timeStart AND :timeEnd))")
    Boolean existsByTime(@Param("kelas") Class kelas, @Param("timeStart") Calendar timeStart, @Param("timeEnd") Calendar timeEnd);

    @Query("SELECT count(s)>0 FROM Schedule s WHERE s.id = :id AND (CURRENT_TIMESTAMP BETWEEN s.timeStart AND s.timeEnd)")
    Boolean isAvailable(@Param("id") Long id);

    @Override
    @Query(value = "SELECT * FROM public.schedule WHERE is_deleted = false", nativeQuery = true)
    List<Schedule> findAll();

    @Override
    @Query(value = "SELECT * FROM public.schedule WHERE is_deleted = false AND id = :id", nativeQuery = true)
    Optional<Schedule> findById(@Param("id") Long id);

    @Override
    @Modifying
    @Transactional
    @Query(value = "UPDATE public.schedule SET is_deleted = true WHERE id = :id", nativeQuery = true)
    void deleteById(@Param("id") Long id);
}
