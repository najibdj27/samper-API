package com.unper.samper.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.unper.samper.model.Schedule;
import com.unper.samper.model.ScheduleHistory;

@Repository
public interface ScheduleHistoryRepository extends JpaRepository<ScheduleHistory, Long>{
    public ScheduleHistory getBySchedule(Schedule scheedule);

    @Query(
        nativeQuery = true, 
        value = "select sh.* from schedule.schedule_history sh" + "\n" +  
                "join schedule.schedule s on sh.schedule_id = s.id" + "\n" +  
                "join common.lecture_subject ls on ls.subject_id = s.subject_id" + "\n" +  
                "join common.lecture_subject_class lsc on lsc.class_id = s.class_id" + "\n" + 
                "where ls.lecture_id = :lectureId"
    )
    public List<ScheduleHistory> getByLecture(@Param("lectureId") Long lectureId);
}
