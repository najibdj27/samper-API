package com.unper.samper.model.constant;

public enum EResponseMessage {
    GET_DATA_SUCCESS("Data retrieved successfully!"),
    GET_DATA_NO_RESOURCE("Data not found!"),
    INSERT_DATA_SUCCESS("Data created successfully!"),
    INSERT_DATA_ALREADY_EXIST("Data already exist!"),
    EDIT_DATA_SUCCESS("Data Successfully edited!"),
    PASSWORD_NOT_MATCH("Password not match!"),
    ROLE_NOT_FOUND("Role not found!"),
    USERNAME_ALREADY_TAKEN("Username is already exists!"),
    EMAIL_ALREADY_EXIST("Email is already exists!"),
    PHONE_NUMBER_ALREADY_EXIST("Phone number is already exists!"),
    REGISTRATION_SUCCESS("Registration success!"),
    PRESENCE_DIFFERENT_CLASS("Can't present in other class!"),
    PRESENCE_SUCCESS("Your presence successfully recorded!"),
    ILLEGAL_ACCESS("You are not allowed to do this action!"),
    ACTIVATE_SCHEDULE_SUCCESS("Schedule is activated!"),
    DEACTIVATE_SCHEDULE("Schedule is deactivated!"),
    SCHEDULE_UNAVAILABLE("Your schedule is not available at current time!"),
    ON_SCHEDULE("You are on the other schedule!"),
    DELETE_SUCCESS("Data deleted successfully!"),
    DELETE_NO_RESOURCE("Can't delete data!"),
    REFRESH_TOKEN_NOT_EXIST("Refresh token is not found!"),
    REFRESH_TOKEN_EXPIRED("Refresh token is expired!");

    private final String message;

    private EResponseMessage(String message){
        this.message = message;
    }

    public String getMessage() {
        return message;
    }
}
