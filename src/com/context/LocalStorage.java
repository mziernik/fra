package com.context;

import com.utils.Utils;
import com.utils.Is;
import com.database.QueryRow;
import com.database.service.ServiceDB;
import com.exceptions.ThrowableException;
import com.json.*;
import com.utils.TCurrency;
import com.utils.Unquoted;
import com.utils.date.TDate;
import com.utils.hashes.Base64;
import com.utils.hashes.Crypto;
import com.model.dao.core.VParser;
import java.sql.SQLException;
import java.util.LinkedList;

/**
 * @author Miłosz Ziernik
 * @date 06 listopada 2015
 * @encoding UTF-8
 */
public class LocalStorage {

    /*
        local_storage_id INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,  
    type CHAR NOT NULL, 
    key VARCHAR(100) NOT NULL UNIQUE, 
    value VARCHAR, 
    last_modified TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP), 
    username VARCHAR 
     */
    private static JObject json;
    private final static Object sync = new Object();

    public static void put(String key, Object value) {
        JElement el = JSON.serialize(value);
        try {
            new ServiceDB().merge("local_storage", "key")
                    .arg("key", key)
                    .arg("value", value != null ? el.toString() : null)
                    .arg("date", new Unquoted("CURRENT_TIMESTAMP"))
                    .arg("encrypted", false)
                    .execute();
        } catch (SQLException ex) {
            throw new ThrowableException(ex);
        }
    }

    public static boolean exists(String key) {
        try {
            return key != null
                    ? new ServiceDB().execute("SELECT 1 FROM local_storage "
                            + "WHERE key = ?", key).first() != null
                    : false;
        } catch (SQLException ex) {
            throw new ThrowableException(ex);
        }
    }

    public static LinkedList<String> getKeys() {
        LinkedList<String> lst = new LinkedList<>();
        try {
            for (Object o : new ServiceDB().execute("SELECT key FROM local_storage").getContent("key"))
                lst.add(Utils.toString(o));
        } catch (SQLException ex) {
            throw new ThrowableException(ex);
        }
        return lst;
    }

    public static void remove(String key) {
        if (key != null)
            try {
                new ServiceDB().execute("DELETE FROM local_storage "
                        + "WHERE key = ?", key).first();
            } catch (SQLException ex) {
                throw new ThrowableException(ex);
            }
    }

    public static void putEncrypted(String key, Object value) {
        value = Base64.encode(Crypto.encrypt(JSON.serialize(value).toString()));
        try {
            new ServiceDB().merge("local_storage", "key")
                    .arg("key", key)
                    .arg("value", value)
                    .arg("date", new Unquoted("CURRENT_TIMESTAMP"))
                    .arg("encrypted", true)
                    .execute();

        } catch (SQLException ex) {
            throw new ThrowableException(ex);
        }
    }

    private static Object get(String key) {
        try {

            QueryRow row = new ServiceDB().execute("SELECT * FROM local_storage "
                    + "WHERE key = ?", key).first();

            if (row == null)
                return null;

            String value = row.getStr("value", null);
            if (value == null)
                return null;

            if (row.getBool("encrypted"))
                value = new String(Crypto.decrypt(Base64.decode(value)));

            JElement el = JSON.parse(value);

            if (el == null)
                return null;

            return el.isValue() ? el.asValue().value() : el;

        } catch (SQLException ex) {
            throw new ThrowableException(ex);
        }
    }

    private static VParser getDataProvider(String key) {
        return new VParser(key, get(key), VParser.Option.LENIENT);
    }

    public static String getStr(String key) {
        return getDataProvider(key).getStr();
    }

    public static String getStr(String key, String def) {
        return getDataProvider(key).getStr(def);
    }

    public static char getChar(String key) {
        return getDataProvider(key).getChar();
    }

    public static Character getChar(String key, Character def) {
        return getDataProvider(key).getChar(def);
    }

    public static Boolean getBool(String key, Boolean def) {
        return getDataProvider(key).getBool(def);
    }

    public static boolean getBool(String key) {
        return getDataProvider(key).getBool();
    }

    public static Byte getByte(String key, Byte def) {
        return getDataProvider(key).getByte(def);
    }

    public static byte getByte(String key) {
        return getDataProvider(key).getByte();
    }

    public static Short getShort(String key, Short def) {
        return getDataProvider(key).getShort(def);
    }

    public static short getShort(String key) {
        return getDataProvider(key).getShort();
    }

    public static Integer getInt(String key, Integer def) {
        return getDataProvider(key).getInt(def);
    }

    public static int getInt(String key) {
        return getDataProvider(key).getInt();
    }

    public static Long getLong(String key, Long def) {
        return getDataProvider(key).getLong(def);
    }

    public static long getLong(String key) {
        return getDataProvider(key).getLong();
    }

    public static Float getFloat(String key, Float def) {
        return getDataProvider(key).getFloat(def);
    }

    public static float getFloat(String key) {
        return getDataProvider(key).getFloat();
    }

    public static Double getDouble(String key, Double def) {
        return getDataProvider(key).getDouble(def);
    }

    public static double getDouble(String key) {
        return getDataProvider(key).getDouble();
    }

    public static TCurrency getCurrency(String key, TCurrency def) {
        return getDataProvider(key).getCurrency(def);
    }

    public static TCurrency getCurrency(String key) {
        return getDataProvider(key).getCurrency();
    }

    public static TDate getDate(String key, TDate def) {
        return getDataProvider(key).getDate(def);
    }

    public static TDate getDate(String key) {
        return getDataProvider(key).getDate();
    }
}
