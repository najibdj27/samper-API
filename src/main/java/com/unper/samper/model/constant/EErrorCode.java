package com.unper.samper.model.constant;

public enum EErrorCode {
    SERVER_ERROR(1000, "App Server Error, please contact the admin"),
    MISSING_HEADERS(1001, "Missing Headers"),
    MISSING_PARAM(1002, "Missing Parameters"),
    MALFORMED_JSON(1003, "Malformed JSON"),
    INVALID_LIMIT(1004, "Invalid offset or limit"),
    INVALID_LOCALE(1005, "Invalid Locale"),
    INVALID_TIMEZONE(1006, "Invalid Timezone"),
    LIMIT_REQUEST(1007, "You exceeded the limit of requests per minute, Please try again after sometime."),
    ACTIVITY_NOT_ALLOWED(1008, "You are not allowed to do this activity."),
    JSON_STRING(1009, "Missing parameter on JSON string."),
    UNAUTHORIZED(1101, "You are Unauthorized, please login with the correct user"),
    AUTH_FAIL(1102, "Authentication Failed"),
    NOT_FOUND(1103, "Not Found"),
    SESSION_EXP(1201, "Your session is expired, please login again"),
    SESSION_INVALID(1202, "Your sessions is invalid"),
    TOKEN_INVALID(1203, "Your sessions token is invalid"),
    INVALID_CRED(1301, "Invalid Credentials"),
    ACCOUNT_DISABLED(1302, "You Account is disabled by the admin"),
    INVALID_PHONE_NUMBER(1303, "Invalid phone number"),
    RESOURCE_EXIST(1304, "Resource already exists"),
    WRONG_OTP(1305, "Wrong OTP value"),
    INVALIND_REQUEST(1306, "Invalid request"),
    FILE_NOT_EXIST(1307, "File not exist"),
    FILE_TOO_LARGE(1308, "File too large"),
    DIFFERENT_CLASS(1400, "Not allowed"),
    SCHEDULE_NOT_ACTIVE(1401, "Schedule not active"),
    SCHEDULE_UNAVAILABLE(1402, "Schedule unavailable"),
    ON_SCHEDULE(1403, "Student on schedule"),
    FACE_NOT_MATCH(1404, "Face not match"),
    LOCATION_NOT_IN_RANGE(1405, "Location not in range"),
    STATUS_NOT_FOUND(1406, "Invalid status"),
    EXT_API_ERR(2001, "Failed when calling external API");


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

