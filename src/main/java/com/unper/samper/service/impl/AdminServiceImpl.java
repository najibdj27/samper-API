package com.unper.samper.service.impl;

import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Admin;
import com.unper.samper.model.Previllage;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddAdminRequestDto;
import com.unper.samper.repository.AdminRepository;
import com.unper.samper.service.AdminService;

@Service
public class AdminServiceImpl implements AdminService {
    @Autowired
    AdminRepository adminRepository;

    @Autowired
    PrevillageServiceImpl previllageServiceImpl;

    @Autowired
    AuthenticationServiceImpl authenticationServiceImpl;

    @Override
    public List<Admin> getAll() throws ResourceNotFoundException {
        List<Admin> adminList = adminRepository.findAll();
        if (adminList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        return adminList;
    }

    @Override
    public Admin getCurrentAdmin() throws ResourceNotFoundException {
        User currentUser = authenticationServiceImpl.getCurrentUser();
        Admin admin = adminRepository.findById(currentUser.getId()).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));

        return admin;
    }

    @Override
    public Admin add(AddAdminRequestDto requestDto) throws ResourceNotFoundException {
        Set<Previllage> previllageSet = previllageServiceImpl.getAllById(requestDto.getPrevillagesId());
        Admin admin = Admin.builder()
            .user(requestDto.getUser())
            .NIP(requestDto.getNIP())
            .previllages(previllageSet)
            .build();
        Admin newAdmin = adminRepository.save(admin);
        return newAdmin;
    }
}
