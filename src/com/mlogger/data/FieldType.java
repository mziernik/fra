package com.mlogger.data;

import com.utils.Is;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import com.utils.Utils;
import com.utils.Is;

/**
 * Miłosz Ziernik 2014/06/12
 */
@SuppressWarnings("unchecked")
public enum FieldType {

    bool_(1, 1, Boolean.class),
    byte_(2, 1, Byte.class),
    short_(3, 2, Short.class),
    int_(4, 4, Integer.class),
    long_(5, 8, Long.class),
    float_(6, 4, Float.class),
    double_(7, 8, Double.class),
    string_(8, -1, String.class),
    date_(9, 8, Date.class),
    uuid_(10, 16, UUID.class),
    collection_(11, -1, Object.class);

    final int id;
    final int length;
    final Class<?> cls;

    public static class SField {

    }

    private FieldType(int id, int length, Class<?> cls) {
        this.id = id;
        this.length = length;
        this.cls = cls;
    }

    static byte[] wrap(Object obj) throws IOException {

        if (obj == null)
            return null;

        if (obj instanceof Boolean)
            return ByteBuffer.allocate(1)
                    .put((Boolean) obj ? (byte) 1 : (byte) 0).array();

        if (obj instanceof Byte)
            return ByteBuffer.allocate(1).put((Byte) obj).array();

        if (obj instanceof Short)
            return ByteBuffer.allocate(2).putShort((Short) obj).array();

        if (obj instanceof Integer)
            return ByteBuffer.allocate(4).putInt((Integer) obj).array();

        if (obj instanceof Long)
            return ByteBuffer.allocate(8).putLong((Long) obj).array();

        if (obj instanceof Float)
            return ByteBuffer.allocate(4).putFloat((Float) obj).array();

        if (obj instanceof Double)
            return ByteBuffer.allocate(8).putDouble((Double) obj).array();

        if (obj instanceof Date)
            return ByteBuffer.allocate(8).putLong(((Date) obj).getTime()).array();

        if (obj instanceof String)
            return obj.toString().getBytes(Charset.forName("UTF-8"));

        if (obj instanceof UUID)
            return ByteBuffer.wrap(new byte[16])
                    .putLong(((UUID) obj).getMostSignificantBits())
                    .putLong(((UUID) obj).getLeastSignificantBits())
                    .array();

        if (obj instanceof boolean[]) {
            byte[] bb = new byte[Array.getLength(obj)];
            for (int i = 0; i < bb.length; i++)
                bb[i] = ((boolean[]) obj)[i] ? (byte) 1 : 0;
            return bb;
        }

        if (obj instanceof byte[])
            return (byte[]) obj;

        if (obj instanceof short[]) {
            ByteBuffer bb = ByteBuffer.allocate(Array.getLength(obj) * 2);
            bb.asShortBuffer().put((short[]) obj);
            return bb.array();
        }

        if (obj instanceof int[]) {
            ByteBuffer bb = ByteBuffer.allocate(Array.getLength(obj) * 4);
            bb.asIntBuffer().put((int[]) obj);
            return bb.array();
        }

        if (obj instanceof long[]) {
            ByteBuffer bb = ByteBuffer.allocate(Array.getLength(obj) * 8);
            bb.asLongBuffer().put((long[]) obj);
            return bb.array();
        }

        if (obj instanceof float[]) {
            ByteBuffer bb = ByteBuffer.allocate(Array.getLength(obj) * 4);
            bb.asFloatBuffer().put((float[]) obj);
            return bb.array();
        }

        if (obj instanceof double[]) {
            ByteBuffer bb = ByteBuffer.allocate(Array.getLength(obj) * 8);
            bb.asDoubleBuffer().put((double[]) obj);
            return bb.array();
        }

        if (obj instanceof String[])
            return wrapCollection(Arrays.asList((String[]) obj), true);

        if (obj instanceof Date[])
            return wrapCollection(Arrays.asList((Date[]) obj), true);

        if (obj instanceof UUID[])
            return wrapCollection(Arrays.asList((UUID[]) obj), true);

        if (obj instanceof byte[][])
            return wrapCollection(Arrays.asList((byte[][]) obj), true);

        if (obj instanceof Collection)
            return wrapCollection((Collection) obj, false);

        return null;
    }

    private static byte[] wrapCollection(Collection<?> collection, boolean isArray) throws IOException {
        // jesli kolekcja nie jest tablica, nalezy okreslic typ kazdego z elementow
        ByteArrayOutputStream bout = new ByteArrayOutputStream();

        Serializer.writeDynLen(collection != null ? collection.size() + 1 : 0, bout);
        for (Object o : collection) {

            FieldType type = null;
            if (!isArray && o != null)
                type = FieldType.get(o.getClass());

            byte[] buff = wrap(o);
            Serializer.writeDynLen(buff != null ? buff.length + 1 : 0, bout);

            if (!isArray)
                bout.write(type != null ? type.id : 0);

            if (buff != null)
                bout.write(buff);
        }
        return bout.toByteArray();
    }

    public static <T> List<T> unwrapCollection(Class<T> type, byte[] data, boolean isArray) throws IOException {
        ByteArrayInputStream bin = new ByteArrayInputStream(data);
        long length = Serializer.readDynamicLength(bin);
        if (length <= 0)
            return null;

        List<T> list = new LinkedList<T>();

        for (int i = 0; i < length - 1; i++) {
            long len = Serializer.readDynamicLength(bin);
            if (len <= 0) {
                list.add(null);
                continue;
            }
            FieldType ft = null;
            if (!isArray)
                ft = FieldType.get(bin.read());

            byte[] buff = new byte[(int) len - 1];
            bin.read(buff);

            if (!isArray && ft == null)
                continue;

            list.add(isArray ? unwrap(type, buff) : (T) unwrap(ft.cls, buff));
        }

        return list;
    }

    public static FieldType get(Integer id) {
        for (FieldType ft : values())
            if (ft.id == id)
                return ft;
        return null;
    }

    public static FieldType get(Class<?> type) {
        return Is.in(type, Boolean.class, Boolean.TYPE, boolean[].class, Boolean[].class) ? bool_
                : Is.in(type, Byte.class, Byte.TYPE, byte[].class,
                        Byte[].class, byte[][].class, Byte[][].class) ? byte_
                        : Is.in(type, Short.class, Short.TYPE, short[].class, Short[].class) ? short_
                        : Is.in(type, Integer.class, Integer.TYPE, int[].class, Integer[].class) ? int_
                        : Is.in(type, Long.class, Long.TYPE, long[].class, Long[].class) ? long_
                        : Is.in(type, Float.class, Float.TYPE, float[].class, Float[].class) ? float_
                        : Is.in(type, Double.class, Double.TYPE, double[].class, Double[].class) ? double_
                        : Is.in(type, String.class, String[].class) ? string_
                        : Is.in(type, Date.class, Date[].class) ? date_
                        : Is.in(type, UUID.class, UUID[].class) ? uuid_
                        : Is.in(type, List.class, Set.class, Collection.class) ? collection_
                        : null;
    }

    public static <T extends Object> T unwrap(Class<T> type, byte[] buffer) throws IOException {

        if (type == Boolean.class)
            return (T) Boolean.valueOf(ByteBuffer.wrap(buffer).get() == 1);

        if (type == Byte.class)
            return (T) Byte.valueOf(ByteBuffer.wrap(buffer).get());

        if (type == Short.class)
            return (T) Short.valueOf(ByteBuffer.wrap(buffer).getShort());

        if (type == Integer.class)
            return (T) Integer.valueOf(ByteBuffer.wrap(buffer).getInt());

        if (type == Long.class)
            return (T) (T) Long.valueOf(ByteBuffer.wrap(buffer).getLong());

        if (type == Float.class)
            return (T) Float.valueOf(ByteBuffer.wrap(buffer).getFloat());

        if (type == Double.class)
            return (T) Double.valueOf(ByteBuffer.wrap(buffer).getDouble());

        if (type == Date.class)
            return (T) new Date(ByteBuffer.wrap(buffer).getLong());

        if (type == UUID.class) {
            ByteBuffer bb = ByteBuffer.wrap(buffer);
            return (T) new UUID(bb.getLong(), bb.getLong(8));
        }

        if (type == String.class)
            return (T) new String(buffer, Charset.forName("UTF-8"));

        if (type == boolean[].class) {
            byte[] arr = ByteBuffer.wrap(buffer).array();
            boolean[] bools = new boolean[arr.length];
            for (int i = 0; i < arr.length; i++)
                bools[i] = arr[i] == 1;
            return (T) bools;
        }

        if (type == byte[].class)
            return (T) buffer;

        if (type == short[].class) {
            short[] arr = new short[buffer.length / 2];
            ByteBuffer.wrap(buffer).asShortBuffer().get(arr);
            return (T) arr;
        }

        if (type == int[].class) {
            int[] arr = new int[buffer.length / 4];
            ByteBuffer.wrap(buffer).asIntBuffer().get(arr);
            return (T) arr;

        }

        if (type == long[].class) {
            long[] arr = new long[buffer.length / 8];
            ByteBuffer.wrap(buffer).asLongBuffer().get(arr);
            return (T) arr;
        }

        if (type == float[].class) {
            float[] arr = new float[buffer.length / 4];
            ByteBuffer.wrap(buffer).asFloatBuffer().get(arr);
            return (T) arr;
        }

        if (type == double[].class) {
            double[] arr = new double[buffer.length / 8];
            ByteBuffer.wrap(buffer).asDoubleBuffer().get(arr);
            return (T) arr;
        }

        if (type == Date[].class) {
            List<Date> list = unwrapCollection(Date.class, buffer, true);
            Date[] arr = new Date[list.size()];
            list.toArray(arr);
            return (T) arr;
        }

        if (type == UUID[].class) {
            List<UUID> list = unwrapCollection(UUID.class, buffer, true);
            UUID[] arr = new UUID[list.size()];
            list.toArray(arr);
            return (T) arr;
        }

        if (type == String[].class) {
            List<String> list = unwrapCollection(String.class, buffer, true);
            String[] arr = new String[list.size()];
            list.toArray(arr);
            return (T) arr;
        }

        if (type == byte[][].class) {
            List<byte[]> list = unwrapCollection(byte[].class, buffer, true);
            byte[][] arr = new byte[list.size()][];
            list.toArray(arr);
            return (T) arr;
        }

        return null;
    }

    @SuppressWarnings("unchecked")
    void setValue(Class<?> type, Field field, Object object, byte[] buffer)
            throws IllegalArgumentException, IllegalAccessException, IOException, InstantiationException {

        if (Is.in(type, List.class, Set.class, Collection.class)) {
            Object obj = field.get(object);

            if (obj == null) {
                obj = type == List.class || type == Collection.class ? new LinkedList<Object>()
                        : type == Set.class ? new LinkedHashSet<Object>()
                                : type.newInstance();
                field.set(object, obj);
            }
            ((Collection) obj).addAll(unwrapCollection(type, buffer, false));
            return;
        }

        if (Modifier.isFinal(field.getModifiers()))
            return;

        if (buffer == null) {
            field.set(object, null);
            return;
        }

        if (type == Boolean.TYPE) {
            field.setBoolean(object, ByteBuffer.wrap(buffer).get() == 1);
            return;
        }

        if (type == Byte.TYPE) {
            field.setByte(object, ByteBuffer.wrap(buffer).get());
            return;
        }

        if (type == Short.TYPE) {
            field.setShort(object, ByteBuffer.wrap(buffer).getShort());
            return;
        }
        if (type == Integer.TYPE) {
            field.setInt(object, ByteBuffer.wrap(buffer).getInt());
            return;
        }
        if (type == Long.TYPE) {
            field.setLong(object, ByteBuffer.wrap(buffer).getLong());
            return;
        }
        if (type == Float.TYPE) {
            field.setFloat(object, ByteBuffer.wrap(buffer).getFloat());
            return;
        }
        if (type == Double.TYPE) {
            field.setDouble(object, ByteBuffer.wrap(buffer).getDouble());
            return;
        }

        field.set(object, unwrap(type, buffer));

    }

}
