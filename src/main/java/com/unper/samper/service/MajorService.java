package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Major;
import com.unper.samper.model.dto.AddMajorRequestDto;
import com.unper.samper.model.dto.UpdateMajorRequestDto;

public interface MajorService {
    List<Major> getAll() throws ResourceNotFoundException;

    Major getById(Long id) throws ResourceNotFoundException;

    Major add(AddMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException;
    
    Major update(UpdateMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException;
    
    void delete(Long id) throws ResourceNotFoundException;
}
