package com.unper.samper.model.dto;

import java.util.Set;

import com.unper.samper.model.User;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AddAdminRequestDto {
    private User user; 
    
    private String nip;

    private Set<Integer> previllagesId;
}
