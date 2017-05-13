package com.config.engine.field;

import com.config.engine.SingleConfigField;
import com.config.engine.cell.select.CcSelect;
import com.config.engine.cell.select.SelectEntries;
import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Klasa reprezentująca komórkę wyboru wartości z kolekcji. Kolekcja definiowana
 * jest jako mapa zawierajca klucz oraz pary nazwa - wartość
 *
 * @author user
 * @param <T>
 */
public class CfSelect<T> extends CfAbstractSelect<CfSelect<T>, T> {

    /**
     *
     * @param key
     * @param name
     * @param clazz
     * @param defaultValue
     * @param enumerate Funkcja zwracająca mapę zawierającą klucz oraz parę
     * nazwa - wartość
     */
    public CfSelect(String key, CharSequence name, Class<? extends T> clazz,
            T defaultValue, Callable<SelectEntries<T>> enumerate) {
        super(key, name, defaultValue, new CcSelect<T, T>(clazz, null)
                .enumerate(enumerate));
    }

    /**
     *
     * @param key
     * @param name
     * @param clazz
     * @param defaultValue
     * @param enumerate Funkcja przekazująca w argumencie mapę zawierającą klucz
     * oraz parę nazwa - wartość
     */
    public CfSelect(String key, CharSequence name, Class<? extends T> clazz,
            T defaultValue, Runnable1<SelectEntries<T>> enumerate) {
        super(key, name, defaultValue, new CcSelect<T, T>(clazz, null)
                .enumerate(enumerate));
    }

    /**
     * Wersja uproszczona, gdzie przekazywana jest zwykła mapa klucz-wartość,
     * nazwa pobierana jest z wartości (wartość.toString)
     *
     * @param enumerate
     * @param key
     * @param name
     * @param clazz
     * @param defaultValue
     */
    protected CfSelect(Callable<Map<String, T>> enumerate,
            String key, CharSequence name, Class<? extends T> clazz, T defaultValue) {
        super(key, name, defaultValue, new CcSelect<T, T>(clazz, null).enumerateSimple(enumerate));
    }

    /**
     * Wersja uproszczona, gdzie przekazywana jest zwykła mapa klucz-wartość,
     * nazwa pobierana jest z wartości (wartość.toString)
     *
     * @param enumerate
     * @param key
     * @param name
     * @param clazz
     * @param defaultValue
     */
    protected CfSelect(Runnable1<LinkedHashMap<String, T>> enumerate,
            String key, CharSequence name, Class<? extends T> clazz,
            T defaultValue) {
        super(key, name, defaultValue, new CcSelect<T, T>(clazz, null)
                .enumerateSimple(enumerate));
    }

}
