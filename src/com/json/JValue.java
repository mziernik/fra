package com.json;

import com.lang.LJson;
import com.utils.Utils;
import com.utils.text.StrWriter;

/**
 * Miłosz Ziernik 2014/04/15
 */
public class JValue extends JElement {

    private Object value;
    private Boolean quote;
    private boolean raw;

    public JValue(final Object value) {
        super();
        this.value = value;
    }

    public Object value() {
        return value;
    }

    public JValue value(Object value) {
        this.value = value;
        return this;
    }

    /**
     * Wartość nie będzie zapisana w cudzysłowiu
     *
     * @param unquoted
     * @return
     */
    public JValue quote(Boolean quote) {
        this.quote = quote;
        return this;
    }

    /**
     * W trybie RAW dane nie są formatowane ani escapowane. Przydatne w
     * połączeniu z JavaScriptem. Dodawane jest jedynie wcięcie
     *
     * @param raw
     * @return
     */
    public JValue raw(boolean raw) {
        this.raw = raw;
        return this;
    }

    @Override
    public boolean isNull() {
        return value == null;
    }

    public boolean isString() {
        return value instanceof String;
    }

    @Override
    public String asString() {
        if (value == null)
            return null;
        return value.toString();
    }

    public boolean isNumber() {
        return value instanceof Number;
    }

    @Override
    public Number asNumber() {

        if (!isNumber() && value != null)
            try {
                // próba parsowania
                String s = Utils.toString(value);

                if (s.contains(",") || s.contains("."))
                    return Double.parseDouble(s);
                return Long.parseLong(s);
            } catch (NumberFormatException e) {
                throw new Error(LJson.INVALID_VALUE.toString(value));

            }

        return (Number) value;
    }

    public boolean isBoolean() {
        return value instanceof Boolean;
    }

    public Boolean asBoolean() {
        if (!isBoolean() && value != null) {
            // próba parsowania
            String s = Utils.toString(value);
            Boolean x = Utils.strBool(s, null);
            if (x != null)
                return x;
            return Boolean.valueOf(s);
        }

        return (Boolean) value;
    }

    @Override
    public boolean remove() {
        return parent != null ? parent.doRemove(this) : false;
    }

    @Override
    public void move(JCollection destination) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void moveChildren(JCollection destination) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void getContent(StrWriter writer) {

        if (value == null) {
            writer.append("null");
            return;
        }

        JOptions opt = getOptions();

        //  writer.append(intent);
        if (raw) {
            String[] split = value.toString().split("\\n");
            for (int i = 0; i < split.length; i++) {
                if (i > 0)
                    writer.intent();
                writer.append(split[i]);
                if (i < split.length - 1)
                    writer.append("\n");
            }
        } else if (Boolean.FALSE.equals(quote)
                || (quote == null && (isBoolean() || isNumber())))
            writer.append(value.toString());
        else {

            char qChar = parent != null && parent.options.singleQuote()
                    ? '\'' : '"';

            writer.append(qChar);
            escape(value.toString(), writer, opt);
            writer.append(qChar);
        }
    }

    @Override
    public boolean isEmpty() {
        return false;
    }

}
