package com.unper.samper.controller;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Admin;
import com.unper.samper.model.Previllage;
import com.unper.samper.model.Role;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AdminResponseDto;
import com.unper.samper.model.dto.GetAllPrevillageByAdminResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.impl.AdminPrevillageServiceImpl;
import com.unper.samper.service.impl.AdminServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Admin")
@RestController
@RequestMapping("/admin")
@SecurityRequirement(name = "bearer-key")
public class AdminController {
    @Autowired
    AdminServiceImpl adminServiceImpl;

    @Autowired
    AdminPrevillageServiceImpl adminPrevillageServiceImpl;
    
    @Operation(summary = "Get all data of admin")
    @PreAuthorize("hasAuhtority('ADMIN')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll() throws ResourceNotFoundException{
        List<Admin> adminList = adminServiceImpl.getAll();
        List<AdminResponseDto> adminResponseDtoList =  new ArrayList<>();
        adminList.forEach(admin -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : admin.getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(admin.getUser().getId())
                .firstName(admin.getUser().getFirstName())
                .lastName(admin.getUser().getLastName())
                .dateOfBirth(admin.getUser().getDateOfBirth())
                .username(admin.getUser().getUsername())
                .email(admin.getUser().getEmail())
                .phoneNumber(admin.getUser().getPhoneNumber())
                .roles(roleList)
                .build();
            List<Previllage> previllageList = new ArrayList<>();
            try {
                previllageList = adminPrevillageServiceImpl.getAllByAdmin(admin.getId());
            } catch (ResourceNotFoundException e) {
                
            }
            List<GetAllPrevillageByAdminResponseDto> responseDtoList = new ArrayList<>();
            previllageList.forEach(previllage -> {
                Set<String> previllageNameSet = new HashSet<>();
                previllageNameSet.add(previllage.getPrevillage().name());
                GetAllPrevillageByAdminResponseDto responseDto = GetAllPrevillageByAdminResponseDto.builder()
                    .id(previllage.getBasePrevillage().getId())
                    .name(previllage.getBasePrevillage().getName())
                    .nameDisplay(previllage.getBasePrevillage().getNameDisplay())
                    .nameDb(previllage.getBasePrevillage().getNameDb())
                    .url(previllage.getBasePrevillage().getUrl())
                    .description(previllage.getBasePrevillage().getDescription())
                    .previllages(previllageNameSet)
                    .build();
                if (responseDtoList.isEmpty()) {
                    responseDtoList.add(responseDto);
                } else {
                    if (responseDtoList.stream().anyMatch(x -> x.getId().equals(responseDto.getId()))) {
                        responseDtoList.stream().filter(x -> x.getId().equals(responseDto.getId())).findFirst().get().getPrevillages().add(previllage.getPrevillage().name());
                    } else {
                        responseDtoList.add(responseDto);
                    }
                }
            });
            Collections.sort(responseDtoList, (r1, r2) -> {
                return r1.getName().compareTo(r2.getName());
            });
            AdminResponseDto responseDto = AdminResponseDto.builder()
                .id(admin.getId())
                .NIP(admin.getNIP())
                .user(userResponseDto)
                .previllages(responseDtoList)
                .build();
            adminResponseDtoList.add(responseDto);
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), adminResponseDtoList);
    }

    @Operation(summary = "Get admin data by id")
    @PreAuthorize("hasAuthorization('ADMIN')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException {
        Admin admin = adminServiceImpl.getById(id);
        List<String> roleList = new ArrayList<>();
        for (Role role : admin.getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(admin.getUser().getId())
            .firstName(admin.getUser().getFirstName())
            .lastName(admin.getUser().getLastName())
            .dateOfBirth(admin.getUser().getDateOfBirth())
            .username(admin.getUser().getUsername())
            .email(admin.getUser().getEmail())
            .phoneNumber(admin.getUser().getPhoneNumber())
            .roles(roleList)
            .build();
        List<Previllage> previllageList = new ArrayList<>();
        try {
            previllageList = adminPrevillageServiceImpl.getAllByAdmin(admin.getId());
        } catch (ResourceNotFoundException e) {
            
        }
        List<GetAllPrevillageByAdminResponseDto> responseDtoList = new ArrayList<>();
        previllageList.forEach(previllage -> {
            Set<String> previllageNameSet = new HashSet<>();
            previllageNameSet.add(previllage.getPrevillage().name());
            GetAllPrevillageByAdminResponseDto responseDto = GetAllPrevillageByAdminResponseDto.builder()
                .id(previllage.getBasePrevillage().getId())
                .name(previllage.getBasePrevillage().getName())
                .nameDisplay(previllage.getBasePrevillage().getNameDisplay())
                .nameDb(previllage.getBasePrevillage().getNameDb())
                .url(previllage.getBasePrevillage().getUrl())
                .description(previllage.getBasePrevillage().getDescription())
                .previllages(previllageNameSet)
                .build();
            if (responseDtoList.isEmpty()) {
                responseDtoList.add(responseDto);
            } else {
                if (responseDtoList.stream().anyMatch(x -> x.getId().equals(responseDto.getId()))) {
                    responseDtoList.stream().filter(x -> x.getId().equals(responseDto.getId())).findFirst().get().getPrevillages().add(previllage.getPrevillage().name());
                } else {
                    responseDtoList.add(responseDto);
                }
            }
        });
        Collections.sort(responseDtoList, (r1, r2) -> {
            return r1.getName().compareTo(r2.getName());
        });
        AdminResponseDto responseDto = AdminResponseDto.builder()
            .id(admin.getId())
            .NIP(admin.getNIP())
            .user(userResponseDto)
            .previllages(responseDtoList)
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    @Operation(summary = "Soft delete an admin")
    @PreAuthorize("hasAuthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(Long id) throws ResourceNotFoundException{
        adminServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}