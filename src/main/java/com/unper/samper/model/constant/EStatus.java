package com.unper.samper.model.constant;

public enum EStatus {
    PENDING('P'),
    APPROVED('V'),
    REJECTED('R'),
    ACTIVE('A'),
    INACTIVE('I'),
    EXPIRED('E'),
    SENT('S'),
    READ('D'),
    OPENED('O'),
    CLOSED('C');

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

