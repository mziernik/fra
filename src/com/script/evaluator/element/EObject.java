package com.script.evaluator.element;

import com.script.evaluator.Pos;
import com.script.evaluator.operator.AssignOperator;
import java.util.LinkedHashMap;
import java.util.Map;

public class EObject extends ECollection {

    public EObject(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    @Override
    public EValue eval() {
        Map<Object, Object> map = new LinkedHashMap<>();

        for (Element el : children) {

            if (el instanceof EOperand) {
                EOperand opnd = (EOperand) el;
                if (opnd.opr instanceof AssignOperator) {
                    Object key
                            = opnd.left instanceof EValue && ((EValue) opnd.left).isVariable()
                            ? ((EValue) opnd.left).getName()
                            : opnd.left.eval().value();
                    Object value = opnd.right.eval().value();
                    map.put(key, value);
                    continue;
                }
            }

            Object key = el.eval().value();
            map.put(key, null);
        }

        return new EValue(parent, map, pos);
    }
}
