package com.utils.collections;

import com.utils.vparser.VParser;
import com.utils.vparser.VParserName;
import java.util.HashMap;

public class Props extends HashMap<String, Object> implements VParserName {

    @Override
    public VParser getDataProvider(String name) {
        return new VParser(name, get(name));
    }

}
