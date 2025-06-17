package com.unper.samper.service.impl;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.StatusNotFoundException;
import com.unper.samper.model.User;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.constant.EUserStatus;
import com.unper.samper.model.dto.EditUserRequestDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.UserService;

@Service
public class UserServiceImpl implements UserService {
    @Autowired
    UserRepository userRepository;

    @Override
    public List<User> getAll() throws ResourceNotFoundException {
        List<User> userList = userRepository.findAll();

        // check if list of User is empty
        if (userList.isEmpty()) {
            throw new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage());
        }
        
        return userList;
    }

    @Override
    public User getById(Long id) throws ResourceNotFoundException {
        User user = userRepository.findById(id).orElseThrow(() -> new ResourceNotFoundException(EResponseMessage.GET_DATA_NO_RESOURCE.getMessage()));
        
        return user;
    }

    @Override
    public User getByEmail(String emailAddress) throws ResourceNotFoundException {
        return userRepository.findByEmail(emailAddress).orElseThrow(() -> new ResourceNotFoundException(emailAddress));
    }

    @Override
    public boolean existsUsername(String username) {
        return userRepository.existsByUsername(username);
    }

    @Override
    public boolean existsByEmail(String emailAddress) {
        return userRepository.existsByEmail(emailAddress);
    }

    @Override
    public boolean existByPhoneNumber(String phoneNumber) {
        return userRepository.existsByPhoneNumber(phoneNumber);
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

    @Override
    public void changeStatus(Long userId, String status) throws ResourceNotFoundException, StatusNotFoundException {
        switch (status) {
            case "ACTIVE":
                activateUser(userId);
                break;
            
            case "INACTIVE":
                deactivateUser(userId);
            
            case "SUSPEND":
                suspendUser(userId);
            default:
                throw new StatusNotFoundException(status);
        }
    }
    
    @Override
    public void activateUser(Long userId) throws ResourceNotFoundException {
        User user =  getById(userId);
        user.setStatus(EUserStatus.ACTIVE);
        userRepository.save(user);
    }

    @Override
    public void deactivateUser(Long userId) throws ResourceNotFoundException {
        User user =  getById(userId);
        user.setStatus(EUserStatus.INACTIVE);
        userRepository.save(user);
    }
    
    @Override
    public void suspendUser(Long userId) throws ResourceNotFoundException {
        User user =  getById(userId);
        user.setStatus(EUserStatus.SUSPEND);
        userRepository.save(user);
    }

    @Override
    public void delete(Long id) throws ResourceNotFoundException {
        getById(id);
        userRepository.deleteById(id);
    }

}
