package com.unper.samper.controller;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PatchMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.handler.ResponseHandler;
import com.unper.samper.model.Class;
import com.unper.samper.model.Major;
import com.unper.samper.model.Role;
import com.unper.samper.model.Student;
import com.unper.samper.model.constant.EResponseMessage;
import com.unper.samper.model.dto.ClassResponseDto;
import com.unper.samper.model.dto.MajorResponseDto;
import com.unper.samper.model.dto.SetLeaderStudentRequestDto;
import com.unper.samper.model.dto.StudentResponseDto;
import com.unper.samper.model.dto.UserResponseDto;
import com.unper.samper.service.ClassService;
import com.unper.samper.service.impl.StudentServiceImpl;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.swagger.v3.oas.annotations.tags.Tag;

@Tag(name = "Student")
@RestController
@RequestMapping("/student")
@SecurityRequirement(name = "bearer-key")
public class StudentController {
    @Autowired
    StudentServiceImpl studentServiceImpl;

    @Autowired
    ClassService classService;
    
    /***
     * Get data of all student 
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get data of all student")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @GetMapping("/all")
    public ResponseEntity<?> getAll(@RequestParam(value = "classId", required = false) Long classId) throws ResourceNotFoundException {
        
        List<Student> studentList = new ArrayList<>();
        if (classId == null) {
            studentList = studentServiceImpl.getAll();
        }else{
            studentList = studentServiceImpl.getAllByClass(classId);
        }

        List<StudentResponseDto> responseDtoList = new ArrayList<>();
        studentList.forEach(student -> {
            List<String> roleList = new ArrayList<>();
            for (Role role : student.getUser().getRoles()) {
                roleList.add(role.getName().toString());
            }
            UserResponseDto userResponseDto = UserResponseDto.builder()
                .id(student.getUser().getId())
                .firstName(student.getUser().getFirstName())
                .lastName(student.getUser().getLastName())
                .dateOfBirth(student.getUser().getDateOfBirth())
                .username(student.getUser().getUsername())
                .email(student.getUser().getEmail())
                .phoneNumber(student.getUser().getPhoneNumber())
                .registeredFaceUrl(student.getUser().getRegisteredFaceUrl())
                .status(student.getUser().getStatus().name())
                .roles(roleList)
                .build();
            ClassResponseDto classResponseDto = ClassResponseDto.builder()
                .id(student.getKelas().getId())
                .lecture(null)
                .name(student.getKelas().getName())
                .build(); 
            StudentResponseDto responseDto = StudentResponseDto.builder()
                .id(student.getId())
                .NIM(student.getNIM())
                .user(userResponseDto)
                .kelas(classResponseDto)
                .isLeader(student.getIsLeader())
                .isActive(student.getIsActive())
                .build();
            responseDtoList.add(responseDto);
        });

        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_total", responseDtoList.size());
        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDtoList, metaData);
    }

    /***
     * Get data of all student by lecture
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get data of all student by lecture")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @GetMapping("/allbylecture/{lectureId}")
    public ResponseEntity<?> getAllByLecture(@PathVariable("lectureId") Long lectureId) throws ResourceNotFoundException{
        Map<String, List<StudentResponseDto>> responseMap = new LinkedHashMap<>();
        List<Class> classList = classService.getAllByLecture(lectureId);
        
        classList.forEach(kelas -> {
            List<Student> studentList = new ArrayList<>();
            try {
                studentList = studentServiceImpl.getAllByLectureAndClass(lectureId, kelas.getId());
                List<StudentResponseDto> responseDtoList = new ArrayList<>();
                studentList.forEach(student -> {
                    List<String> roleList = new ArrayList<>();
                    for (Role role : student.getUser().getRoles()) {
                        roleList.add(role.getName().toString());
                    }
                    Major major = student.getKelas().getMajor();
                    MajorResponseDto majorResponseDto = MajorResponseDto.builder()
                        .majorCode(major.getMajorCode())
                        .name(major.getName())
                        .build();
                    UserResponseDto userResponseDto = UserResponseDto.builder()
                        .id(student.getUser().getId())
                        .firstName(student.getUser().getFirstName())
                        .lastName(student.getUser().getLastName())
                        .dateOfBirth(student.getUser().getDateOfBirth())
                        .username(student.getUser().getUsername())
                        .email(student.getUser().getEmail())
                        .phoneNumber(student.getUser().getPhoneNumber())
                        .registeredFaceUrl(student.getUser().getRegisteredFaceUrl())
                        .status(student.getUser().getStatus().name())
                        .roles(roleList)
                        .build();
                    ClassResponseDto classResponseDto = ClassResponseDto.builder()
                        .id(student.getKelas().getId())
                        .lecture(null)
                        .name(student.getKelas().getName())
                        .major(majorResponseDto)
                        .build(); 
                    StudentResponseDto responseDto = StudentResponseDto.builder()
                        .id(student.getId())
                        .NIM(student.getNIM())
                        .user(userResponseDto)
                        .kelas(classResponseDto)
                        .isLeader(student.getIsLeader())
                        .isActive(student.getIsActive())
                        .build();
                    responseDtoList.add(responseDto);
                });
                responseMap.put(kelas.getName(), responseDtoList);
            } catch (ResourceNotFoundException e) {
                responseMap.put(kelas.getName(), null);
            }
        });
        

        Map<String, Object> metaData = new HashMap<>();
        metaData.put("_totalClass", responseMap.size());
        metaData.put("_totalStudent", responseMap.values().stream().mapToInt(List::size).sum());
        metaData.put("_totalActiveStudent", responseMap.values().stream().flatMap(List::stream).filter(StudentResponseDto::getIsActive).count());

        return ResponseHandler.generateSuccessResponseWithMeta(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseMap, metaData);
    }

    /***
     * Get student data by id
     * @param id
     * @return
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Get student data by id")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('STUDENT') or hasAuthority('LECTURE')")
    @GetMapping("/get/{id}")
    public ResponseEntity<?> getById(@PathVariable("id") Long id) throws ResourceNotFoundException{
        Student student = studentServiceImpl.getById(id);
        List<String> roleList = new ArrayList<>();
        for (Role role : student.getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        Major major = student.getKelas().getMajor();
        MajorResponseDto majorResponseDto = MajorResponseDto.builder()
            .majorCode(major.getMajorCode())
            .name(major.getName())
            .build();
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(student.getUser().getId())
            .firstName(student.getUser().getFirstName())
            .lastName(student.getUser().getLastName())
            .dateOfBirth(student.getUser().getDateOfBirth())
            .username(student.getUser().getUsername())
            .email(student.getUser().getEmail())
            .phoneNumber(student.getUser().getPhoneNumber())
            .status(student.getUser().getStatus().name())
            .roles(roleList)
            .build();
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .id(student.getKelas().getId())
            .lecture(null)
            .name(student.getKelas().getName())
            .major(majorResponseDto)
            .build(); 
        StudentResponseDto responseDto = StudentResponseDto.builder()
            .id(student.getId())
            .NIM(student.getNIM())
            .user(userResponseDto)
            .kelas(classResponseDto)
            .isLeader(student.getIsLeader())
            .isActive(student.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.GET_DATA_SUCCESS.getMessage(), responseDto);
    }

    /***
     * Set a student as class leader
     * @param id
     * @return
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Set a student as class leader")
    @PreAuthorize("hasAuthority('ADMIN') or hasAuthority('LECTURE')")
    @PatchMapping("/set-leader")
    public ResponseEntity<?> setAsLeader(@RequestBody SetLeaderStudentRequestDto requestDto) throws ResourceNotFoundException{
        Student student = studentServiceImpl.setAsLeader(requestDto.getStudentId());
        List<String> roleList = new ArrayList<>();
        for (Role role : student.getUser().getRoles()) {
            roleList.add(role.getName().toString());
        }
        Major major = student.getKelas().getMajor();
        MajorResponseDto majorResponseDto = MajorResponseDto.builder()
            .majorCode(major.getMajorCode())
            .name(major.getName())
            .build();
        UserResponseDto userResponseDto = UserResponseDto.builder()
            .id(student.getUser().getId())
            .firstName(student.getUser().getFirstName())
            .lastName(student.getUser().getLastName())
            .dateOfBirth(student.getUser().getDateOfBirth())
            .username(student.getUser().getUsername())
            .email(student.getUser().getEmail())
            .phoneNumber(student.getUser().getPhoneNumber())
            .registeredFaceUrl(student.getUser().getRegisteredFaceUrl())
            .status(student.getUser().getStatus().name())
            .roles(roleList)
            .build();
        ClassResponseDto classResponseDto = ClassResponseDto.builder()
            .id(student.getKelas().getId())
            .lecture(null)
            .name(student.getKelas().getName())
            .major(majorResponseDto)
            .build(); 
        StudentResponseDto responseDto = StudentResponseDto.builder()
            .id(student.getId())
            .NIM(student.getNIM())
            .user(userResponseDto)
            .kelas(classResponseDto)
            .isLeader(student.getIsLeader())
            .isActive(student.getIsActive())
            .build();
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.EDIT_DATA_SUCCESS.getMessage(), responseDto);
    }

    /***
     * Soft delete student
     * @param id
     * @return
     * @throws ResourceNotFoundException
     */
    @Operation(summary = "Soft delete student")
    @PreAuthorize("hasAthority('ADMIN')")
    @PatchMapping("/delete/{id}")
    public ResponseEntity<?> delete(@PathVariable("id") Long id) throws ResourceNotFoundException{
        studentServiceImpl.delete(id);
        return ResponseHandler.generateSuccessResponse(HttpStatus.OK, EResponseMessage.DELETE_SUCCESS.getMessage(), null);
    }
}
