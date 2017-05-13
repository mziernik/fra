package com.exceptions.intf;

import java.util.LinkedHashMap;
import java.util.Map;

public interface IDetailedException {

    public void getDetails(Map<String, String> details);

    default Map<String, String> getDetails() {
        Map<String, String> map = new LinkedHashMap<>();
        getDetails(map);
        return map;
    }

}
