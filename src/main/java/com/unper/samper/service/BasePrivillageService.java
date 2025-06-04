package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.BasePrivilage;

public interface BasePrivillageService {
    List<BasePrivilage> getAll() throws ResourceNotFoundException;

    List<BasePrivilage> getAllById(List<Integer> idList) throws ResourceNotFoundException;
}
