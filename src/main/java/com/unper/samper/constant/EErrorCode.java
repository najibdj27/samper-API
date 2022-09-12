package com.unper.samper.constant;

public enum EErrorCode {
    SERVER_ERROR(1000, "App Server Error, please contact the admin"),
    MISSING_HEADERS(1001, "Missing Headers"),
    MISSING_PARAM(1002, "Missing Parameters"),
    INVALID_LIMIT(1003, "Invalid offset or limit"),
    INVALID_LOCALE(1004, "Invalid Locale"),
    INVALID_TIMEZONE(1005, "Invalid Timezone"),
    LIMIT_REQUEST(1006, "You exceeded the limit of requests per minute, Please try again after sometime."),
    AUTH_FAIL(1102, "Authentication Failed"),
    NOT_FOUND(1103, "Not Found"),
    SESSION_EXP(1201, "Your session is expired, please login again"),
    SESSION_INVALID(1202, "Your sessions is invalid"),
    TOKEN_INVALID(1203, "Your sessions token is invalid"),
    INVALID_CRED(1301, "Invalid Credentials"),
    ACCOUNT_DISABLED(1302, "You Account is disabled by the admin"),
    INVALID_PHONE_NUMBER(1303, "Invalid phone number");


    private final int code;
    private final String description;

    private EErrorCode(int code, String description) {
        this.code = code;
        this.description = description;
    }

    public String getDescription() {
        return description;
    }

    public int getCode() {
        return code;
    }

    @Override
    public String toString() {
        return code + ": " + description;
    }
}
