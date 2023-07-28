package com.unper.samper.service.impl;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.model.Role;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.UserService;

@Service
public class UserServiceImpl implements UserService {
    @Autowired
    UserRepository userRepository;

    @Override
    public List<UserResponseDto> getAll() throws ResourceNotFoundException {
        List<User> users = userRepository.findAll();

        // check if list of User is empty
        if (users.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        List<UserResponseDto> responseDtoList = new ArrayList<>();
        users.forEach(user -> {
            List<Role> roleList = new ArrayList<>();
            roleList.addAll(user.getRoles());
            UserResponseDto responseDto = UserResponseDto.builder()
                .id(user.getId())
                .firstName(user.getFirstName())
                .lastName(user.getLastName())
                .dateOfBirth(user.getDateOfBirth())
                .username(user.getUsername())
                .email(user.getEmail())
                .phoneNumber(user.getPhoneNumber())
                .roles(roleList)
                .build();
            responseDtoList.add(responseDto);
        });
        
        return responseDtoList;
    }

    @Override
    public UserResponseDto getById(Long id) throws ResourceNotFoundException {
        Optional<User> user = userRepository.findById(id);

        // check if user with that ID not exist in DB
        if (user.isEmpty()){
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        List<Role> roleList = new ArrayList<>();
        roleList.addAll(user.get().getRoles());
        UserResponseDto responseDto = UserResponseDto.builder()
            .id(user.get().getId())
            .firstName(user.get().getFirstName())
            .lastName(user.get().getLastName())
            .dateOfBirth(user.get().getDateOfBirth())
            .username(user.get().getUsername())
            .email(user.get().getEmail())
            .phoneNumber(user.get().getPhoneNumber())
            .roles(roleList)
            .build();
        return responseDto;
    }

    @Override
    public UserResponseDto edit(Long id, EditUserRequestDto requestDto) throws ResourceNotFoundException {
        User user = User.builder()
            .id(id)
            .firstName(requestDto.getFirstName())
            .lastName(requestDto.getLastName())
            .dateOfBirth(requestDto.getDateOfBirth())
            .build();

        // check if user exists in DB
        if (Boolean.FALSE == userRepository.existsById(id)) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }

        User editedUser = userRepository.save(user);

        UserResponseDto responseDto = UserResponseDto.builder()
            .id(editedUser.getId())
            .firstName(editedUser.getFirstName())
            .lastName(editedUser.getLastName())
            .dateOfBirth(editedUser.getDateOfBirth())
            .email(editedUser.getEmail())
            .username(editedUser.getUsername())
            .phoneNumber(editedUser.getPhoneNumber())
            .build();
        return responseDto;
    }
    
}
