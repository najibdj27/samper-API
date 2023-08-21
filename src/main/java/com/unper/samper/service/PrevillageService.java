package com.unper.samper.service;

import java.util.List;
import java.util.Set;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Previllage;

public interface PrevillageService {
    List<Previllage> getAll();

    Set<Previllage> getAllById(Set<Integer> previllagesId) throws ResourceNotFoundException;
}
