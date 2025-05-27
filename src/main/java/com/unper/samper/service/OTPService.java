package com.unper.samper.service;

import com.unper.samper.exception.ResourceAlreadyExistException;
import com.unper.samper.exception.ResourceNotFoundException;
import com.unper.samper.exception.WrongOTPException;
import com.unper.samper.model.constant.EType;
import com.unper.samper.model.dto.ConfirmOTPResponseDto;

public interface OTPService {
    int generateOTP(String key);

    int getOTP(String key);

    void clearOTP(String key);

    ConfirmOTPResponseDto confirmOTP(String key, int otpValue, EType type) throws WrongOTPException, ResourceNotFoundException, ResourceAlreadyExistException;
}

