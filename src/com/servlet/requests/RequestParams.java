package com.servlet.requests;

import com.utils.Utils;
import com.utils.Is;
import com.exceptions.http.Http400BadRequestException;
import com.exceptions.http.Http400BadRequestParamException;
import com.utils.collections.Params;
import java.lang.reflect.Field;
import java.util.LinkedList;

public class RequestParams extends Params {

    private final HttpRequest request;

    public RequestParams(HttpRequest request) {
        this.request = request;
    }

    public String firstStr() throws Http400BadRequestParamException {
        String result = firstStr(null);
        if (result != null)
            return result;
        throw new Http400BadRequestParamException(request, null);
    }

    public String firstStr(String def) {
        Param par = first();
        String result = par != null ? par.name : null;
        return result != null ? result : def;
    }

    public Integer firstInt(Integer def) {
        return Utils.strInt(firstStr(null), def);
    }

    public String firstName() {
        Param param = first();
        return param != null ? param.name : null;
    }

    public int firstInt() throws Http400BadRequestParamException {
        Integer val = Utils.strInt(firstStr(null), null);
        if (val != null)
            return val;
        throw new Http400BadRequestParamException(request, null);
    }

    public String getStr(String name, String def) {
        Param par = getParam(name);
        return par != null ? Utils.toString(par.value) : def;
    }

    public String getStr(String name) throws Http400BadRequestParamException {
        String par = getStr(name, null);
        if (par != null)
            return par;
        throw new Http400BadRequestParamException(request, name);
    }

    public Integer getInt(String name, Integer def) {
        return Utils.strInt(getStr(name, null), def);
    }

    public int getInt(String name) throws Http400BadRequestParamException {
        Integer val = Utils.strInt(getStr(name), null);
        if (val != null)
            return val;
        throw new Http400BadRequestParamException(request, name);
    }

    public Long getLong(String name, Long def) {
        return Utils.strLong(getStr(name, null), def);
    }

    public long getLong(String name) throws Http400BadRequestParamException {
        Long val = Utils.strLong(getStr(name), null);
        if (val != null)
            return val;
        throw new Http400BadRequestParamException(request, name);
    }

    public Double getDouble(String name, Double def) {
        return Utils.strDouble(getStr(name, null), def);
    }

    public double getDouble(String name) throws Http400BadRequestParamException {
        Double val = Utils.strDouble(getStr(name), null);
        if (val != null)
            return val;
        throw new Http400BadRequestParamException(request, name);
    }

    public Boolean getBool(String name, Boolean def) {
        return Utils.strBool(getStr(name, null), def);
    }

    public boolean getBool(String name) throws Http400BadRequestParamException {
        Boolean val = Utils.strBool(getStr(name), null);
        if (val != null)
            return val;
        throw new Http400BadRequestParamException(request, name);
    }

    public int size() {
        return list.size();
    }

    /**
     * Czy występuja wszystkie parametry zdefiniowane w $names
     *
     * @param names
     * @return
     */
    public boolean has(String... names) {
        if (names == null || names.length == 0)
            return false;

        for (String name : names)
            if (getParam(name) == null)
                return false;

        return true;
    }

    public boolean hasOneOf(String... names) {
        if (names == null || names.length == 0)
            return false;
        for (String name : names)
            if (getParam(name) != null)
                return true;
        return false;
    }

    public boolean exists(String name) {
        return getParam(name) != null;
    }

    public LinkedList<String> getList(String name) {
        LinkedList<String> list = new LinkedList<>();
        for (Param par : getParams(name))
            list.add(Utils.toString(par.value));
        return list;
    }

}
