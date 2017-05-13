package com.script;

import com.events.EventListeners;
import com.exceptions.ServiceException;
import com.intf.callable.Callable1;
import com.json.JElement;
import com.json.JSON;
import java.io.IOException;
import java.io.Reader;
import java.util.LinkedList;
import javax.script.*;
import jdk.nashorn.api.scripting.*;

public class Nashorn implements Compilable, Invocable, ScriptEngine {

    final NashornScriptEngineFactory factory = new NashornScriptEngineFactory();

    public final EventListeners<ClassFilter> classFilters = new EventListeners<>();

    ScriptEngineManager engineManager
            = new ScriptEngineManager();
    public final NashornScriptEngine engine;

    public Nashorn() {

        engine = (NashornScriptEngine) factory.getScriptEngine((String string) -> {
            for (ClassFilter filter : classFilters)
                if (!filter.exposeToScripts(string))
                    return false;
            return true;
        });

    }

    @Override
    public ScriptEngineFactory getFactory() {
        return engine.getFactory();
    }

    @Override
    public Bindings createBindings() {
        return engine.createBindings();
    }

    @Override
    public Object eval(String script) throws ScriptException {
        return engine.eval(script);
    }

    @Override
    public CompiledScript compile(String script) throws ScriptException {
        return engine.compile(script);
    }

    @Override
    public CompiledScript compile(Reader script) throws ScriptException {
        return engine.compile(script);
    }

    @Override
    public Object invokeFunction(String name, Object... args) throws ScriptException, NoSuchMethodException {
        return engine.invokeFunction(name, args);
    }

    @Override
    public Object invokeMethod(Object thiz, String name, Object... args) throws ScriptException, NoSuchMethodException {
        return engine.invokeMethod(thiz, name, args);
    }

    @Override
    public <T extends Object> T getInterface(Class<T> clazz) {
        return engine.getInterface(clazz);
    }

    @Override
    public <T extends Object> T getInterface(Object thiz, Class<T> clazz) {
        return engine.getInterface(thiz, clazz);
    }

    @Override
    public Object eval(Reader reader, ScriptContext ctxt) throws ScriptException {
        return engine.eval(reader, ctxt);
    }

    @Override
    public Object eval(String script, ScriptContext ctxt) throws ScriptException {
        return engine.eval(script, ctxt);
    }

    @Override
    public Object eval(Reader reader) throws ScriptException {
        return engine.eval(reader);
    }

    @Override
    public Object eval(String script, Bindings n) throws ScriptException {
        return engine.eval(script, n);
    }

    @Override
    public Object eval(Reader reader, Bindings n) throws ScriptException {
        return engine.eval(reader, n);
    }

    @Override
    public void put(String key, Object value) {
        engine.put(key, value);
    }

    @Override
    public Object get(String key) {
        return engine.get(key);
    }

    @Override
    public Bindings getBindings(int scope) {
        return engine.getBindings(scope);
    }

    @Override
    public void setBindings(Bindings bindings, int scope) {
        engine.setBindings(bindings, scope);
    }

    @Override
    public ScriptContext getContext() {
        return engine.getContext();
    }

    @Override
    public void setContext(ScriptContext context) {
        engine.setContext(context);
    }

    public static String parseVariablesScript(String script, Callable1<Object, String> getter)
            throws ScriptException {
        StringBuilder sb = new StringBuilder();
        try {
            parseVariablesScript(script, sb, getter);
        } catch (IOException ex) {
            throw new ServiceException(ex);
        }
        return sb.toString();
    }

    /**
     * Metoda parsuje skrypt zawierający zmienne w postaci ${zmienna}
     *
     * @param script
     * @param getter
     * @return
     * @throws ScriptException
     */
    public static void parseVariablesScript(String script,
            Appendable writer, Callable1<Object, String> getter)
            throws ScriptException, IOException {

        StringBuilder variable = null;

        LinkedList<Character> quotaStack = new LinkedList<>();

        char prev = 0;

        char[] arr = script.toCharArray();
        for (int i = 0; i < arr.length; i++) {

            char c = arr[i];
            char next = i < arr.length - 1 ? arr[i + 1] : 0;

            if (c == '$' && next == '{') {
                variable = new StringBuilder();
                ++i;
                continue;
            }

            if (variable != null && c == '}') {
                String var = variable.toString();
                variable = null;
                Object v = getter.run(var);
                if (v == null) {
                    writer.append("${").append(var).append("}");
                    continue;
                }

                JElement el = JSON.serialize(v);
                if (el.isCollection())
                    el.asCollection().options.compactMode(true);
                writer.append(el.toString());
                continue;
            }

            boolean accepted = (c >= '0' && c <= '9')
                    || (c >= 'a' && c <= 'z')
                    || (c >= 'A' && c <= 'Z')
                    || (c == '_')
                    || (c == '.');

            if ((c == '"' || c == '\'') && prev != '\\')
                if (quotaStack.isEmpty() || quotaStack.peekLast() != c)
                    quotaStack.add(c);
                else
                    quotaStack.pop();

            if (variable != null && (!quotaStack.isEmpty() || !accepted)) {
                writer.append(variable.toString());
                variable = null;
            }

            if (variable != null)
                variable.append(c);
            else
                writer.append(c);

            prev = c;
        }
    }

}
