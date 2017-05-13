package com.config.engine.field;

import com.config.engine.SingleConfigField;
import com.config.engine.cell.select.CcSelect;


/**
 * Klasa reprezentująca komórkę wyboru wartości z kolekcji. Kolekcja definiowana
 * jest jako mapa zawierajca klucz oraz pary nazwa - wartość
 *
 * @author user
 * @param <T>
 */
public abstract class CfAbstractSelect<SELF extends CfAbstractSelect<SELF, T>, T>
        extends SingleConfigField<SELF, T> {

    public CfAbstractSelect(String key, CharSequence name, T defaultValue,
            CcSelect<T, T>... cells) {
        super(key, name, defaultValue, cells);
    }

}
