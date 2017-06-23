package com.mlogger;

@Deprecated // użyć com.utils.reflections.DataType_old
public enum DataType_old {

    TEXT(0),
    LIST(1),
    DOTTED_LIST(2),
    NUMERIC_LIST(3),
    NUMBER(4),
    XML(5),
    JSON(6),
    CSV(7),
    HEX(7),
    BASE64(8),
    URI(9),
    HTML(10);

    public final int id;

    public static DataType_old get(int id) {
        for (DataType_old dt : values())
            if (dt.id == id)
                return dt;
        return null;
    }

    private DataType_old(int id) {
        this.id = id;
    }

}
