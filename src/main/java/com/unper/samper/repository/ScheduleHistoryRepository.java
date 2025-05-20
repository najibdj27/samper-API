package com.unper.samper.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.unper.samper.model.Schedule;
import com.unper.samper.model.ScheduleHistory;

@Repository
public interface ScheduleHistoryRepository extends JpaRepository<ScheduleHistory, Long>{
    public ScheduleHistory getBySchedule(Schedule scheedule);
}
