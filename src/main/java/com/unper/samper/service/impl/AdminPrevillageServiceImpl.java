package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Admin;
import com.unper.samper.model.Previllage;
import com.unper.samper.service.AdminPrevillageService;

@Service
public class AdminPrevillageServiceImpl implements AdminPrevillageService {
    @Autowired
    AdminServiceImpl adminServiceImpl;

    @Override
    public List<Previllage> getAllByCurrentAdmin() throws ResourceNotFoundException {
        List<Previllage> previllageList = new ArrayList<>();
        Admin currentAdmin = adminServiceImpl.getCurrentAdmin();
        currentAdmin.getPrevillages().forEach((previllage) -> {
            previllageList.add(previllage);
        });

        return previllageList;
    }
    
}
