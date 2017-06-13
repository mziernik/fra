package com.utils.collections;

import com.model.dao.core.VParser;
import java.util.HashMap;
import com.model.dao.core.DAOName;

public class Props extends HashMap<String, Object> implements DAOName {

    @Override
    public VParser getDataProvider(String name) {
        return new VParser(name, get(name));
    }

}
