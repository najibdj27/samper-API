package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Privilage;

public interface AdminPrevillageService {
    List<Privilage> getAllByCurrentAdmin(String name) throws ResourceNotFoundException;

    List<Privilage> getAllByAdmin(Long adminId) throws ResourceNotFoundException;

}
