package com.unper.samper.service;

import java.text.ParseException;
import java.util.List;

import com.unper.samper.exception.ActivityNotAllowedException;
import com.unper.samper.exception.NoAccessException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Request;
import com.unper.samper.model.dto.AddRequestRequestDto;

public interface RequestService {

    List<Request> getAll(Long senderId, Long receiverId, String requestTimeFrom, String requestTimeTo) throws ResourceNotFoundException;

    Request getById(Long id) throws ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
    
    Request add(AddRequestRequestDto requestDto) throws ResourceNotFoundException, ActivityNotAllowedException;
    
    Request approve(Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException, ParseException;

    Request reject(Long id) throws ResourceNotFoundException, NoAccessException, ActivityNotAllowedException;
}
