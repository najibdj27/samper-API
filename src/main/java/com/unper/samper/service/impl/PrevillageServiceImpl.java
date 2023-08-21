package com.unper.samper.service.impl;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Previllage;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.repository.PrevillageRepository;
import com.unper.samper.service.PrevillageService;

@Service
public class PrevillageServiceImpl implements PrevillageService {
    @Autowired
    PrevillageRepository previllageRepository;

    @Override
    public List<Previllage> getAll() {
        // TODO Auto-generated method stub
        throw new UnsupportedOperationException("Unimplemented method 'getAll'");
    }

    @Override
    public Set<Previllage> getAllById(Set<Integer> previllagesId) throws ResourceNotFoundException {
        List<Previllage> previllageList = previllageRepository.findAllById(previllagesId);
        if (previllageList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        Set<Previllage> previllageSet = new HashSet<>(previllageList);
        return previllageSet;
        
    }
    
}
