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
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Previllage;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.GetAllPrevillageByAdminResponseDto;
import com.unper.samper.service.impl.AdminPrevillageServiceImpl;

import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@SecurityRequirement(name = "bearer-key")
@Tag(name = "Admin Previllage")
@RestController
@RequestMapping("/adminprevillage")
public class AdminPrevillageController {
    @Autowired
    AdminPrevillageServiceImpl adminPrevillageServiceImpl;
    
    @GetMapping("/getbycurrentadmin")
    @PreAuthorize("hasAuthority('ADMIN')")
    public ResponseEntity<?> getAllByCurrentAdmin(@RequestParam(value = "name", required = false) String name) throws ResourceNotFoundException {
        List<Previllage> previllageList = adminPrevillageServiceImpl.getAllByCurrentAdmin(name);
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
            }else{
                if (responseDtoList.stream().anyMatch(x -> x.getId().equals(responseDto.getId()))) {
                    responseDtoList.stream().filter(x -> x.getId().equals(responseDto.getId())).findFirst().get().getPrevillages().add(previllage.getPrevillage().name());
                }else{
                    responseDtoList.add(responseDto);
                }
            }
        });
        Collections.sort(responseDtoList, (r1, r2) -> {
            return r1.getName().compareTo(r2.getName());
        });
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList);
    }
}
