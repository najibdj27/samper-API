package com.unper.samper.service;

import java.util.List;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Admin;
import com.unper.samper.model.dto.AddAdminRequestDto;

public interface AdminService {
    List<Admin> getAll() throws ResourceNotFoundException;

    Admin getCurrentAdmin() throws ResourceNotFoundException;

    Admin getById(Long id) throws ResourceNotFoundException;

    Admin add(AddAdminRequestDto requestDto) throws ResourceNotFoundException;

    void delete(Long id) throws ResourceNotFoundException;
}
