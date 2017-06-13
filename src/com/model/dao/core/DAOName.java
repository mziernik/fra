package com.model.dao.core;

import com.utils.TCurrency;
import com.utils.date.TDate;

public interface DAOName {

    VParser getDataProvider(String name);

    default boolean isNull(String name) {
        return getDataProvider(name).isNull();
    }

    default String getStr(String name) {
        return getDataProvider(name).getStr();
    }

    default String getStr(String name, String def) {
        return getDataProvider(name).getStr(def);
    }

    default char getChar(String name) {
        return getDataProvider(name).getChar();
    }

    default Character getChar(String name, Character def) {
        return getDataProvider(name).getChar(def);
    }

    default Boolean getBool(String name, Boolean def) {
        return getDataProvider(name).getBool(def);
    }

    default boolean getBool(String name) {
        return getDataProvider(name).getBool();
    }

    default Byte getByte(String name, Byte def) {
        return getDataProvider(name).getByte(def);
    }

    default byte getByte(String name) {
        return getDataProvider(name).getByte();
    }

    default Short getShort(String name, Short def) {
        return getDataProvider(name).getShort(def);
    }

    default short getShort(String name) {
        return getDataProvider(name).getShort();
    }

    default Integer getInt(String name, Integer def) {
        return getDataProvider(name).getInt(def);
    }

    default int getInt(String name) {
        return getDataProvider(name).getInt();
    }

    default Long getLong(String name, Long def) {
        return getDataProvider(name).getLong(def);
    }

    default long getLong(String name) {
        return getDataProvider(name).getLong();
    }

    default Float getFloat(String name, Float def) {
        return getDataProvider(name).getFloat(def);
    }

    default float getFloat(String name) {
        return getDataProvider(name).getFloat();
    }

    default Double getDouble(String name, Double def) {
        return getDataProvider(name).getDouble(def);
    }

    default double getDouble(String name) {
        return getDataProvider(name).getDouble();
    }

    default TCurrency getCurrency(String name) {
        return getDataProvider(name).getCurrency();
    }

    default TCurrency getCurrency(String name, TCurrency def) {
        return getDataProvider(name).getCurrency(def);
    }

    default TCurrency getCurrency(String name, double def) {
        return getDataProvider(name).getCurrency(def);
    }

    default TDate getDate(String name, TDate def) {
        return getDataProvider(name).getDate(def);
    }

    default TDate getDate(String name) {
        return getDataProvider(name).getDate();
    }
}
