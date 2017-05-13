package com.script.evaluator.function.other;

import com.io.IOUtils;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EBlock;
import com.script.evaluator.element.EFunction;
import com.script.evaluator.element.EValue;
import com.script.evaluator.function.Function;
import com.utils.Utils;
import com.utils.Is;
import java.io.File;
import java.io.InputStream;
import java.net.URL;

public class EfRead extends Function {

    public EfRead(Evaluator evaluator) {
        super(evaluator, "read");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) throws Exception {
        checkArgs(args, String.class);
        String arg = args[0].asString();

        if (arg.contains("://"))
            return IOUtils.read(new URL(arg), Utils.UTF8);

        File file = new File(arg);
        if (!file.exists()) {
            com.mlogger.Log.warning("File " + file + " not exists");
            return Void.TYPE;
        }
        return IOUtils.read(file, Utils.UTF8);
    }

}
