package com.unper.samper.model.constant;

public enum EStatus {
    PENDING('P'),
    APPROVED('V'),
    REJECTED('R'),
    ACTIVE('A'),
    INACTIVE('I'),
    EXPIRED('X'),
    SENT('S'),
    READ('D'),
    OPENED('O'),
    CLOSED('C'),
    ELIGIBLE('Y'),
    NOT_ELIGIBLE('N');

    private final char code;

    private EStatus(char code) {
        this.code = code;
    }

    public char getCode() {
        return code;
    }

    @Override
    public String toString() {
        return "code";
    }
    
}

