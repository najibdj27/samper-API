package com.unper.samper.model.constant;

public enum EResponseMessage {
    GET_DATA_SUCCESS("Data retrieved successfully!"),
    GET_DATA_NO_RESOURCE("Data not found!"),
    INSERT_DATA_SUCCESS("Data created successfully!"),
    INSERT_DATA_ALREADY_EXIST("Data already exist!"),
    EDIT_DATA_SUCCESS("Data Successfully edited!");

    private final String message;

    private EResponseMessage(String message){
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}
