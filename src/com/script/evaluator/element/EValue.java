package com.script.evaluator.element;

import com.json.Escape;
import com.script.evaluator.Pos;
import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import java.util.List;
import java.util.Map;

public class EValue extends Element {

    private final Object val;
    String varName;
    String constName;

    public EValue(ECollection parent, Object value, Pos pos) {
        super(parent, pos);
        this.val = value;
    }

    public Object value() {

        if (varName != null) {

            String[] names = varName.split("\\.");
            Object result = null;
            Map vars = evaluator.variables;
            List list = null;
            for (String name : names) {
                result = vars != null ? vars.get(name) : null;

                if (vars == null && list != null && Utils.strInt(name, null) != null) {
                    Integer idx = Utils.strInt(name);
                    if (idx >= 0 && idx < list.size())
                        result = list.get(idx);
                }

                vars = null;
                list = null;

                if (result instanceof Map)
                    vars = (Map) result;

                if (result instanceof List)
                    list = (List) result;
            }

            if (evaluator.getVariableResolver() != null)
                result = evaluator.getVariableResolver().resolveVariable(varName, result);

            return result;
        }
        return val;
    }

    public String getName() {
        return varName;
    }

    public boolean isVariable() {
        return varName != null;
    }

    @Override
    public EValue eval() {
        return this;
    }

    public boolean isString() {
        return value() instanceof String;
    }

    public Integer getSize() {
        if (isString())
            return asString().length();

        if (isArray())
            return asArray().size();

        if (isObject())
            return asObject().size();

        return null;
    }

    public String asString() {
        if (isDouble()) {
            String result = Double.toString(asDouble());
            if (result.endsWith(".0"))
                return result.substring(0, result.length() - 2);
            return result;
        }

        return Utils.toString(value());
    }

    @Override
    public void getContent(StrWriter writer) {
        if (varName != null)
            writer.append(varName);
        else if (constName != null)
            writer.append(constName);
        else
            writer.append(Escape.escape(value()));
    }

    public boolean isArray() {
        return value() instanceof List;
    }

    public List<EValue> asArray() {
        return (List<EValue>) value();
    }

    public boolean isObject() {
        return value() instanceof Map;
    }

    public Map<String, EValue> asObject() {
        return (Map<String, EValue>) value();
    }

    public boolean isDouble() {
        return value() instanceof Double;
    }

    public boolean isNumber() {
        return value() instanceof Number;
    }

    public boolean isLong() {
        return isNumber() && ((Number) value()).longValue() == asDouble();
    }

    public long asLong() {
        return ((Number) value()).longValue();
    }

    public Number asNumber() {
        return ((Number) value());
    }

    public double asDouble() {
        return ((Number) value()).doubleValue();
    }

    public int asInteger() {
        return ((Number) value()).intValue();
    }

    public boolean isBoolean() {
        return value() instanceof Boolean;
    }

    public boolean asBoolean() {
        return (Boolean) value();
    }

    public boolean isNull() {
        return value() == null;
    }

    public Object getValue() {
        return value();
    }
    /*
    public static EValue result(Evaluator evaluator, Object value) {
        Element.Type type = Element.Type.STRING;
        if (value instanceof Number)
            type = Element.Type.NUMBER;

        if (value instanceof Boolean)
            type = Element.Type.BOOLEAN;

        if (value instanceof LinkedList)
            type = Element.Type.ARRAY;

        if (value instanceof HashMap)
            type = Element.Type.OBJECT;

        if (type == Element.Type.STRING)
            value = Utils.toString(value);

        EValue val = new EValue(evaluator, type);

        return val;
    }*/
}
