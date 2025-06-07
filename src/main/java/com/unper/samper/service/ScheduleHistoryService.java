package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.ScheduleHistory;
import com.unper.samper.model.dto.ScheduleHistoryReqeustDto;

public interface ScheduleHistoryService {
    public List<ScheduleHistory> getAll() throws ResourceNotFoundException;
    
    public ScheduleHistory getById(Long id) throws ResourceNotFoundException;

    public ScheduleHistory getByScheduleId(Long scheduleId) throws ResourceNotFoundException;

    public ScheduleHistory create(ScheduleHistoryReqeustDto scheduleHistoryReqeustDto) throws ResourceNotFoundException;

    public ScheduleHistory update(ScheduleHistoryReqeustDto scheduleHistoryReqeustDto) throws ResourceNotFoundException;
}
