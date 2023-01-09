package com.unper.samper.service.impl;

import java.util.Set;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.unper.samper.constant.ERole;
import com.unper.samper.domain.dao.Role;
import com.unper.samper.domain.dao.User;
import com.unper.samper.domain.dto.EditUserRequestDTO;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.repository.RoleRepository;
import com.unper.samper.repository.UserRepository;
import com.unper.samper.service.StudentService;

@Service
public class StudentServiceImpl implements StudentService {

    @Autowired
    UserRepository userRepository;

    @Autowired
    RoleRepository roleRepository;

    @Override
    public User get(Long userId) throws ResourceNotFoundException {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User not found"));
        Set<Role> roles = user.getRoles();
        Role role = roleRepository.findByName(ERole.ROLE_STUDENT).orElseThrow(() -> new ResourceNotFoundException("Role not found"));
        if (!roles.contains(role)) {
            throw new ResourceNotFoundException("User not found");
        }
        return user;
    }

    // @Override
    // public List<User> getAll() throws ResourceNotFoundException {
    //     Role role = roleRepository.findByName(ERole.ROLE_STUDENT).orElseThrow(() -> new ResourceNotFoundException("Role not found"));
    //     Set<User> users = role.getUsers();
    //     List<User> userList = users.stream().collect(Collectors.toList());
    //     return userList;
    // }

    @Override
    public User edit(Long userId, EditUserRequestDTO requestDTO) throws ResourceNotFoundException {
        User user = userRepository.findById(userId).orElseThrow(() -> new ResourceNotFoundException("User not found"));
        Set<Role> roles = user.getRoles();
        Role role = roleRepository.findByName(ERole.ROLE_STUDENT).orElseThrow(() -> new ResourceNotFoundException("Role not found"));
        if (!roles.contains(role)) {
            throw new ResourceNotFoundException("User not found");
        }
        user.setFirstName(requestDTO.getFirstName());
        user.setLastName(requestDTO.getLastName());
        user.setDateOfBirth(requestDTO.getDateOfBirth());
        user.setEmail(requestDTO.getEmail());
        user.setPhoneNumber(requestDTO.getPhoneNumber());
        User editedUser = userRepository.save(user);
        return editedUser;
    }

    @Override
    public void delete(Long userId) {
        
    }
    
}
