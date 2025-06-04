package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.BasePrivilage;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.BasePrivilageRepository;
import com.unper.samper.service.BasePrivillageService;

@Service
public class BasePrivillageServiceImpl implements BasePrivillageService {
    @Autowired
    BasePrivilageRepository basePrevillageRepository;

    @Override
    public List<BasePrivilage> getAll() throws ResourceNotFoundException {
        List<BasePrivilage> basePrevillageList = basePrevillageRepository.findAll();
        if (basePrevillageList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        return basePrevillageList;
    }
    
    @Override
    public List<BasePrivilage> getAllById(List<Integer> idList) throws ResourceNotFoundException {
        List<BasePrivilage> basePrevillageList = basePrevillageRepository.findAllById(idList);
        if (basePrevillageList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        return basePrevillageList;
    }    
}
