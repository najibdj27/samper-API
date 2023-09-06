package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Previllage;

public interface AdminPrevillageService {
    List<Previllage> getAllByCurrentAdmin(String name) throws ResourceNotFoundException;
}
