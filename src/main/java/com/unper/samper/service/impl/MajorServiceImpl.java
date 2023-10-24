package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Lecture;
import com.unper.samper.model.Major;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.AddMajorRequestDto;
import com.unper.samper.model.dto.UpdateMajorRequestDto;
import com.unper.samper.repository.MajorRepository;
import com.unper.samper.service.MajorService;

@Service
public class MajorServiceImpl implements MajorService {
    @Autowired
    MajorRepository majorRepository;

    @Autowired
    LectureServiceImpl lectureServiceImpl;

    @Override
    public List<Major> getAll() throws ResourceNotFoundException {
        List<Major> majorList = majorRepository.findAll();
        if (majorList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        return majorList;
    }

    @Override
    public Major getById(Long id) throws ResourceNotFoundException {
        Major major = majorRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        return major;
    }

    @Override
    public Major add(AddMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        Lecture majorHead = lectureServiceImpl.getById(requestDto.getMajorHeadId());
        Major major = Major.builder()
            .majorCode(requestDto.getMajorCode())
            .name(requestDto.getName())
            .majorHead(majorHead)
            .build();
        if (Boolean.TRUE.equals(majorRepository.existsByMajorCode(requestDto.getMajorCode()))){
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }
        Major newMajor = majorRepository.save(major);
        return newMajor;
    }

    @Override
    public Major update(UpdateMajorRequestDto requestDto) throws ResourceNotFoundException, ResourceAlreadyExistException {
        Lecture majorHead = lectureServiceImpl.getById(requestDto.getMajorHeadId());
        Major major = Major.builder()
            .id(requestDto.getId())
            .majorCode(requestDto.getMajorCode())
            .name(requestDto.getName())
            .majorHead(majorHead)
            .build();
        if (Boolean.TRUE.equals(majorRepository.existsByMajorCode(requestDto.getMajorCode()))){
            throw new ResourceAlreadyExistException(EResponseMessage.INSERT_DATA_ALREADY_EXIST.getMessage());
        }
        Major newMajor = majorRepository.save(major);
        return newMajor;
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        majorRepository.deleteById(id);
    }
}
