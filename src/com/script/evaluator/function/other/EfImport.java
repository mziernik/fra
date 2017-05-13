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
import java.net.URL;

public class EfImport extends Function {

    public EfImport(Evaluator evaluator) {
        super(evaluator, "import");
    }

    @Override
    protected Object doEvaluate(EFunction funct, EValue[] args) throws Exception {
        checkArgs(args, String.class);
        String arg = args[0].asString();

        String data = null;

        if (arg.contains("://")) {
            URL url = new URL(arg);
            data = IOUtils.read(url, Utils.UTF8);
        }

        if (data == null) {
            File file = new File(arg);
            if (!file.exists()) {
                com.mlogger.Log.warning("File " + file + " not exists");
                return Void.TYPE;

            }

            data = IOUtils.read(file, Utils.UTF8);
        }

        EBlock block = new EBlock(evaluator, funct.pos);
        block.parse(arg, data);
        return block.eval().getValue();
    }

}
