package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

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
    public List<Previllage> getAllByCurrentAdmin(String name) throws ResourceNotFoundException {
        List<Previllage> previllageList = new ArrayList<>();
        Admin currentAdmin = adminServiceImpl.getCurrentAdmin();
        currentAdmin.getPrevillages().forEach((previllage) -> {
            previllageList.add(previllage);
        });

        if (name != null) {
            List<Previllage> previllageListFiltered = previllageList.stream().filter(p -> p.getBasePrevillage().getNameDb().equals(name)).collect(Collectors.toList());
            return previllageListFiltered;
        }else{
            return previllageList;
        }

    }

    @Override
    public List<Previllage> getAllByAdmin(Long adminId) throws ResourceNotFoundException {
        List<Previllage> previllageList = new ArrayList<>();
        Admin admin = adminServiceImpl.getById(adminId);
        admin.getPrevillages().forEach((previllage) -> {
            previllageList.add(previllage);
        });

        return previllageList;
    }
    
}
