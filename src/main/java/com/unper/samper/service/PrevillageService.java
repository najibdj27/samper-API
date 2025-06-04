package com.unper.samper.service;

import java.util.List;
import java.util.Set;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Privilage;

public interface PrevillageService {
    List<Privilage> getAll();

    Set<Privilage> getAllById(Set<Integer> previllagesId) throws ResourceNotFoundException;
}
