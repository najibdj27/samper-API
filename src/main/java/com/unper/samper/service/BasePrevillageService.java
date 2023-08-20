package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.BasePrevillage;

public interface BasePrevillageService {
    List<BasePrevillage> getAll() throws ResourceNotFoundException;

    List<BasePrevillage> getAllById(List<Integer> idList) throws ResourceNotFoundException;
}
