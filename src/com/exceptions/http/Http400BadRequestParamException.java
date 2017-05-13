package com.exceptions.http;

import com.servlet.requests.HttpRequest;
import com.utils.collections.Strings;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http400BadRequestParamException extends HttpException {

    public Http400BadRequestParamException(HttpRequest request, String missingParam) {
        this(request, missingParam, null);
    }

    public Http400BadRequestParamException(HttpRequest request, String missingParam, Throwable e) {
        super(400, "Nieprawidłowe żądanie", e);

        if (missingParam == null || missingParam.isEmpty()) {
            details("Brak wymaganaego parametru");
            return;
        }

        Strings pars = new Strings(request.params.getList(missingParam))
                .prefix("\"").sufix("\"");
        details((pars.isEmpty() ? "Brak parametru \""
                : "Nieprawidłowa wartość parametru \"") + missingParam + "\""
                + (pars.isEmpty() ? "" : " (" + pars.toString(", ") + ")"));
    }

}
