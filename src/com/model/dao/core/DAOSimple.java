package com.model.dao.core;

import com.utils.TCurrency;
import com.utils.date.TDate;

public interface DAOSimple {

    VParser getDataProvider();

    default String getStr() {
        return getDataProvider().getStr();
    }

    default String getStr(String def) {
        return getDataProvider().getStr(def);
    }

    default char getChar() {
        return getDataProvider().getChar();
    }

    default Character getChar(Character def) {
        return getDataProvider().getChar(def);
    }

    default Boolean getBool(Boolean def) {
        return getDataProvider().getBool(def);
    }

    default boolean getBool() {
        return getDataProvider().getBool();
    }

    default Byte getByte(Byte def) {
        return getDataProvider().getByte(def);
    }

    default byte getByte() {
        return getDataProvider().getByte();
    }

    default Short getShort(Short def) {
        return getDataProvider().getShort(def);
    }

    default short getShort() {
        return getDataProvider().getShort();
    }

    default Integer getInt(Integer def) {
        return getDataProvider().getInt(def);
    }

    default int getInt() {
        return getDataProvider().getInt();
    }

    default Long getLong(Long def) {
        return getDataProvider().getLong(def);
    }

    default long getLong() {
        return getDataProvider().getLong();
    }

    default Float getFloat(Float def) {
        return getDataProvider().getFloat(def);
    }

    default float getFloat() {
        return getDataProvider().getFloat();
    }

    default Double getDouble(Double def) {
        return getDataProvider().getDouble(def);
    }

    default double getDouble() {
        return getDataProvider().getDouble();
    }

    default TCurrency getCurrency() {
        return getDataProvider().getCurrency();
    }

    default TCurrency getCurrency(TCurrency def) {
        return getDataProvider().getCurrency(def);
    }

    default TCurrency getCurrency(double def) {
        return getDataProvider().getCurrency(def);
    }

    default TDate getDate(TDate def) {
        return getDataProvider().getDate(def);
    }

    default TDate getDate() {
        return getDataProvider().getDate();
    }
}
