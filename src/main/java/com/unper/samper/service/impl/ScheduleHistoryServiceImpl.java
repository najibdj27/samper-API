package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Schedule;
import com.unper.samper.model.ScheduleHistory;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.ScheduleHistoryReqeustDto;
import com.unper.samper.repository.ScheduleHistoryRepository;
import com.unper.samper.repository.ScheduleRepository;
import com.unper.samper.service.ScheduleHistoryService;

@Service
public class ScheduleHistoryServiceImpl implements ScheduleHistoryService {

    @Autowired 
    private ScheduleHistoryRepository scheduleHistoryRepository;
    
    @Autowired 
    private ScheduleRepository scheduleRepository;

    @Override
    public List<ScheduleHistory> getAll() throws ResourceNotFoundException {
        List<ScheduleHistory> scheduleHistoryList = scheduleHistoryRepository.findAll();
        if (scheduleHistoryList.isEmpty()) {
            throw new ResourceNotFoundException("Schedule history is empty");
        }
        return scheduleHistoryList;
    }

    @Override
    public ScheduleHistory getById(Long id) throws ResourceNotFoundException {
        ScheduleHistory scheduleHistory = scheduleHistoryRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException("null"));
        return scheduleHistory;
    }

    @Override
    public ScheduleHistory getByScheduleId(Long scheduleId) throws ResourceNotFoundException {
        Schedule schedule = scheduleRepository.findById(scheduleId).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return scheduleHistoryRepository.getBySchedule(schedule);
    }
    
    @Override
    public ScheduleHistory create(ScheduleHistoryReqeustDto scheduleHistoryReqeustDto) throws ResourceNotFoundException {
        Schedule schedule = scheduleRepository.findById(scheduleHistoryReqeustDto.getScheduleId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        ScheduleHistory scheduleHistory = ScheduleHistory.builder()
            .schedule(schedule)
            .openTime(scheduleHistoryReqeustDto.getTime())
            .openLongitude(scheduleHistoryReqeustDto.getLongitude())
            .openLatitude(scheduleHistoryReqeustDto.getLatitude())
            .build();
        return scheduleHistoryRepository.save(scheduleHistory);
    }

    @Override
    public ScheduleHistory update(ScheduleHistoryReqeustDto scheduleHistoryReqeustDto) throws ResourceNotFoundException {
        ScheduleHistory scheduleHistory = getByScheduleId(scheduleHistoryReqeustDto.getScheduleId());
        scheduleHistory.setCloseTime(scheduleHistoryReqeustDto.getTime());
        scheduleHistory.setCloseLongitude(scheduleHistoryReqeustDto.getLongitude());
        scheduleHistory.setCloseLatitude(scheduleHistoryReqeustDto.getLatitude());
        
        return scheduleHistoryRepository.save(scheduleHistory);
    }

}
