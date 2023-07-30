package com.unper.samper.repository;

import java.util.Calendar;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Class;
import com.unper.samper.model.Schedule;

public interface ScheduleRepository extends JpaRepository<Schedule, Long>{
    @Query("SELECT count(s)>0 FROM Schedule s WHERE s.kelas = :kelas AND ((s.timeStart BETWEEN :timeStart AND :timeEnd) or (s.timeEnd BETWEEN :timeStart AND :timeEnd))")
    Boolean existsByTime(@Param("kelas") Class kelas, @Param("timeStart") Calendar timeStart, @Param("timeEnd") Calendar timeEnd);

    @Query("SELECT count(s)>0 FROM Schedule s WHERE s.id = :id AND (CURRENT_DATE BETWEEN s.timeStart AND s.timeEnd)")
    Boolean isAvailable(@Param("id") Long id);
}
