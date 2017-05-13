package com.script.evaluator.operator;

import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.element.EOperand;
import com.script.evaluator.element.EValue;
import com.utils.Utils;
import com.utils.Is;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public abstract class AssignOperator extends Operator {

    public AssignOperator(Evaluator evaluator, String opr) {
        super(evaluator, opr, 0);
    }

    @Override
    protected Object doEvaluate(EOperand operand, EValue left, EValue right) {

        if (!left.isVariable() && left.isString()) {
            evaluator.variables.put(left.asString(), right.getValue());
            return right;
        }

        if (!(left.isVariable()))
            throw new EvaluationException(left.pos, left.toString() + " is not variable");

        Map vars = evaluator.variables;
        List list = null;
        String[] names = left.getName().split("\\.");
        Object result = null;

        for (int i = 0; i < names.length - 1; i++) {
            String name = names[i];
            String next = i < names.length - 1 ? names[i + 1] : null;

            result = vars != null ? vars.get(name) : null;

            if (vars == null && list != null && Utils.strInt(name, null) != null) {
                Integer idx = Utils.strInt(name);
                if (idx >= 0 && idx < list.size())
                    result = list.get(idx);
            }

            if (result == null)
                result = new LinkedHashMap<>();
            // result = Utils.strInt(next, null) != null ? new ArrayList<>() : new LinkedHashMap<>();

            if (result != null)
                if (vars != null)
                    vars.put(name, result);
                else if (list != null)
                    list.add(result);

            vars = null;
            list = null;

            if (result instanceof Map)
                vars = (Map) result;

            if (result instanceof List)
                list = (List) result;
        }

        if (vars != null)
            vars.put(names[names.length - 1], right.getValue());

//        if (evaluator.getVariableAssigner() != null)
//            evaluator.getVariableAssigner().assign(left.getName(), right);
//        else
//            evaluator.variables.put(left.getName(), right.getValue());
        //     evaluator.vars.put(left.getName(), right.getValue());
        return right;
    }

    public static class Assign1Operator extends AssignOperator {

        public Assign1Operator(Evaluator evaluator) {
            super(evaluator, "=");
        }
    }

    public static class Assign2Operator extends AssignOperator {

        public Assign2Operator(Evaluator evaluator) {
            super(evaluator, ":");
        }
    }
}
