package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.BasePrevillage;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.BasePrevillageRepository;
import com.unper.samper.service.BasePrevillageService;

@Service
public class BasePrevillageServiceImpl implements BasePrevillageService {
    @Autowired
    BasePrevillageRepository basePrevillageRepository;

    @Override
    public List<BasePrevillage> getAll() throws ResourceNotFoundException {
        List<BasePrevillage> basePrevillageList = basePrevillageRepository.findAll();
        if (basePrevillageList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        return basePrevillageList;
    }
    
    @Override
    public List<BasePrevillage> getAllById(List<Integer> idList) throws ResourceNotFoundException {
        List<BasePrevillage> basePrevillageList = basePrevillageRepository.findAllById(idList);
        if (basePrevillageList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        return basePrevillageList;
    }    
}
