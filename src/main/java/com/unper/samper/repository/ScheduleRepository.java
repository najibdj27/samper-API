package com.unper.samper.repository;

import java.util.Calendar;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import com.unper.samper.model.Class;
import com.unper.samper.model.Schedule;

public interface ScheduleRepository extends JpaRepository<Schedule, Long>{
    @Query("select count(s)>0 from Schedule s where s.kelas = :kelas and ((s.timeStart between :timeStart and :timeEnd) or (s.timeEnd between :timeStart and :timeEnd))")
    Boolean existsByTime(@Param("kelas") Class kelas, @Param("timeStart") Calendar timeStart, @Param("timeEnd") Calendar timeEnd);
}
