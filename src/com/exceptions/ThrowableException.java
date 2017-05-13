package com.exceptions;

import com.json.Escape;
import java.util.LinkedHashMap;

/**
 * Klasa pośrednicząca w przechwytywaniu i przekazywaniu wyjątków, która jest
 * ignorowana w stosach wywołań
 *
 * @author user
 * @
 */
public class ThrowableException extends RuntimeException {

    public final LinkedHashMap<String, String> detials = new LinkedHashMap<>();

    public ThrowableException details(String name, Object value) {
        detials.put(name, Escape.escape(value));
        return this;
    }

    public ThrowableException(Throwable cause) {
        super(cause);
    }

    public ThrowableException(String message, Throwable cause) {
        super(message, cause);
    }

}

/**
 * Przykład:
 *
 * public void foo() { try { throw new Exception("dsfdasf"); } catch
 * (RuntimeException | Error ex) { throw ex; } catch (Exception ex) { throw new
 * ThrowableException(ex); } }
 */
