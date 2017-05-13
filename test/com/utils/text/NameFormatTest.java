package com.utils.text;

import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author milosz
 */
public class NameFormatTest {

    @Test
    public void testCaseConvert() {

        assertEquals("AlaMaKota", new NameFormat().format("Ala ma Kota."));
        assertEquals("ALA_MA_KOTA", new NameFormat()
                .caseConvert(NameFormat.CaseConvert.UPPER)
                .format("Ala ma Kota."));

        assertEquals("ala_ma_kota", new NameFormat()
                .caseConvert(NameFormat.CaseConvert.LOWER)
                .format("Ala ma Kota."));
    }

}
