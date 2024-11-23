package com.unper.samper.model.dto;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;
import com.unper.samper.util.EmptyStringAsNullDeserializer;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Data
@Builder
public class AddRequestRequestDto {
    private String type;
    
    private Long scheduleId;
    
    private String reason;
    
    @JsonDeserialize(using = EmptyStringAsNullDeserializer.class)
    private String requestData;
}
