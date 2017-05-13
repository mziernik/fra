package com.script;

import com.context.AppContext;
import com.context.Environment;
import com.json.JObject;
import com.json.JSON;
import com.script.evaluator.Evaluator;
import java.util.*;

public class ConfFile extends Evaluator {

    public ConfFile(String data) {
        super(data);
        constants.put("WINDOWS", Environment.isWindows);
        constants.put("LINUX", Environment.isLinux);
        constants.put("USER_NAME", Environment.userName);
        constants.put("HOST_NAME", Environment.hostname);
        constants.put("DEV_MODE", AppContext.devMode);
        constants.put("DEBUG_MODE", AppContext.debugMode(null));
        constants.put("UNIT_TEST_MODE", AppContext.unitTestMode);
    }

    public JObject process() {
        eval();
        return JSON.serialize(variables).asObject();
    }

}
