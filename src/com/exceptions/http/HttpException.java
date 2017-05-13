package com.exceptions.http;

import com.exceptions.intf.DetailedException;
import com.exceptions.intf.IHttpException;
import java.util.List;

public class HttpException extends DetailedException implements IHttpException {

    private int code = 500;

    public HttpException(int code, String message) {
        super(message);
        this.code = code;
    }

    public HttpException(int code, String message, Throwable e) {
        super(message, e);
        this.code = code;
    }

    @Override
    public int getHttpStatus() {
        return code;
    }

}
