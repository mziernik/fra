package com.mlogger.data;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class Serializer implements Iterable<ClassData> {

    protected final List<ClassData> classes = new LinkedList<ClassData>();
    public byte[] signature;
    public Date date = new Date();
    public byte[] meta;

    public ObjectProvider objectProvider;

    private final List<byte[]> content = new LinkedList<byte[]>();

    public Serializer(Class<?>... classes) {
        if (classes == null)
            return;

        for (Class<?> cls : classes) {
            ClassData cData = new ClassData(cls);

            for (ClassData cd : this.classes)
                if (cd.id == cData.id)
                    throw new RuntimeException("Klasy " + cd.cls.getName()
                            + " oraz " + cData.cls.getName()
                            + " posiadają identyczne identyfikatory");

            this.classes.add(cData);
        }
    }

    @Override
    public Iterator<ClassData> iterator() {
        return classes.iterator();
    }

    public List<Object> read(InputStream inputStream) throws IOException, InstantiationException, IllegalAccessException {
        List<Object> objects = new LinkedList<Object>();

        DataInputStream in = new DataInputStream(inputStream);

        if (signature != null) {
            byte[] buff = new byte[signature.length];
            in.read(buff);
            if (!Arrays.equals(buff, signature))
                throw new IOException("Nieprawidłowa sygnatura");
        }

        byte version = in.readByte();
        if (version != 1)
            throw new IOException("Nieprawidłowa wersja");

        int flags = (int) readDynamicLength(in);

        if ((flags & 1) > 0)
            date = new Date(in.readLong());

        if ((flags & 2) > 0) {
            meta = new byte[(int) readDynamicLength(in)];
            in.read(meta);
        }

        int objCount = (int) readDynamicLength(in);
        int totalSize = (int) readDynamicLength(in);

        for (int i = 0; i < objCount; i++) {
            long objId = readDynamicLength(in);
            long fieldsCount = readDynamicLength(in);
            byte[] data = new byte[(int) readDynamicLength(in)];
            in.read(data);

            ClassData cData = null;
            for (ClassData cd : this)
                if (cd.id == objId)
                    cData = cd;

            if (cData == null)
                continue;

            objects.add(processClass(cData, fieldsCount, new DataInputStream(new ByteArrayInputStream(data))));

        }

        return objects;

    }

    private Object processClass(ClassData cData, long fieldsCount, DataInputStream din) throws IOException, InstantiationException, IllegalAccessException {

        Object object = null;

        if (objectProvider != null)
            object = objectProvider.getObject(cData.cls, cData.id);

        if (object == null)
            object = cData.cls.newInstance();

        while (din.available() > 0) {
            long fieldId = readDynamicLength(din);
            byte typeId = din.readByte();

            long buffLen = readDynamicLength(din);

            byte[] buffer = null;
            if (buffLen > 0) {
                buffer = new byte[(int) buffLen - 1];
                din.read(buffer);
            }

            ClassField cField = null;
            for (ClassField cf : cData)
                if (cf.id == fieldId)
                    cField = cf;

            if (cField == null)
                continue;

            FieldType type = FieldType.get(cField.cls);

            type.setValue(cField.cls, cField.field, object, buffer);

        }
        return object;
    }

    static long readDynamicLength(InputStream in) throws IOException {
        long value = 0;
        int shift = 0;
        while (in.available() > 0 && shift <= 7) {
            int v = in.read();
            if (v == -1)
                break;
            value |= (v & 0x7f) << shift;
            shift += 7;
            if (v < 0x80)
                break;
        }
        if (value < 0)
            throw new IOException("Wartość " + value + " mniejsza niż 0");
        return value;
    }

    public void write(OutputStream outputStream) throws IOException {

        DataOutputStream out = new DataOutputStream(outputStream);

        int flags = 0;
        if (date != null)
            flags += 1;
        if (meta != null)
            flags += 2;

        if (signature != null)
            out.write(signature);

        out.writeByte(1); // wersja
        writeDynLen(flags, out);

        if (date != null)
            out.writeLong(date.getTime());

        if (meta != null) {
            writeDynLen(meta.length, out);
            out.write(meta);
        }

        int size = 0;
        for (byte[] bb : content)
            size += bb.length;

        writeDynLen(content.size(), out); // ilosc klas
        writeDynLen(size, out); // rozmiar calkowity
        for (byte[] bb : content)
            out.write(bb);

        out.flush();
    }

    public Serializer add(Object object) throws IllegalArgumentException, IllegalAccessException, IOException {
        if (object == null)
            return null;

        Class<?> cls = object.getClass();

        ClassData cData = null;
        for (ClassData cd : classes)
            if (cd.cls == cls)
                cData = cd;

        if (cData == null)
            throw new RuntimeException("Niezdefiniowana klasa: " + cls.getName());

        ByteArrayOutputStream out = new ByteArrayOutputStream();

        int cnt = 0;
        for (ClassField cf : cData) {

            Object obj = cf.field.get(object);

            byte[] buff = null;

            FieldType type = FieldType.get(cf.cls);

            if (type == null)
                continue;

            buff = FieldType.wrap(obj);

            if (obj instanceof byte[])
                buff = (byte[]) obj;

            if (cf.cls.isArray()) {
                // dopisac 

            }

            ByteArrrayWriter wr = new ByteArrrayWriter();
            wr.writeDyn(cf.id);
            wr.writeByte(type.id);
            wr.writeDyn(buff == null ? 0 : buff.length + 1);
            if (buff != null)
                wr.write(buff);
            wr.flush();
            out.write(wr.toByteArray());
            ++cnt;
        }

        byte[] data = out.toByteArray();

        ByteArrrayWriter writer = new ByteArrrayWriter();

        writer.writeDyn(cData.id);
        writer.writeDyn(cnt);
        writer.writeDyn(data.length); // rozmiar calkowity
        writer.write(data); // dane

        content.add(writer.toByteArray());
        return this;
    }

    /*
     static byte[] writeDynLen(long value) throws IOException {
     if (value < 0)
     throw new IOException("Wartość " + value + " nie może być mniejsza niż 0");
     ByteArrayOutputStream out = new ByteArrayOutputStream();
     while (value > 0x70) {
     out.write(((byte) value | 0x80) & 0xFF);
     value = value >> 7;
     }
     out.write((byte) value);
     return out.toByteArray();
     }
     */
    static void writeDynLen(long value, OutputStream out) throws IOException {
        if (value < 0)
            throw new IOException("Wartość " + value + " nie może być mniejsza niż 0");
        while (value > 0x70) {
            out.write(((byte) value | 0x80) & 0xFF);
            value = value >> 7;
        }
        out.write((byte) value);
    }

}

class ClassField {

    public final Field field;
    public final ClassData classData;
    public final long id;
    public final Class<?> cls;

    public ClassField(Field field, ClassData classData, long id) {
        this.field = field;
        this.classData = classData;
        this.id = id;
        this.cls = field.getType();
    }

}

class ByteArrrayWriter extends DataOutputStream {

    public ByteArrrayWriter() {
        super(new ByteArrayOutputStream());
    }

    public ByteArrrayWriter writeDyn(long value) throws IOException {
        Serializer.writeDynLen(value, this);
        return this;
    }

    public byte[] toByteArray() {
        return ((ByteArrayOutputStream) out).toByteArray();
    }

}

class ClassData implements Iterable<ClassField> {

    final List<ClassField> fields = new LinkedList<ClassField>();
    final Class<?> cls;
    final long id;

    public ClassData(Class<?> cls) {
        this.cls = cls;

        ObjId objId = cls.getAnnotation(ObjId.class);
        if (objId == null)
            throw new RuntimeException("Klasa " + cls.getName()
                    + " nie posiada adnotacji " + ObjId.class);

        id = objId.value();

        for (Field f : cls.getFields()) {
            int mods = f.getModifiers();
            if (!Modifier.isPublic(mods))
                continue;

            objId = f.getAnnotation(ObjId.class);
            if (objId == null)
                continue;

            ClassField cf = new ClassField(f, this, objId.value());

            for (ClassField c : this)
                if (c.id == cf.id)
                    throw new RuntimeException("Pola " + c.field.getName()
                            + " oraz " + f.getName()
                            + " posiadają identyczne identyfikatory");

            fields.add(cf);

        }
    }

    @Override
    public Iterator<ClassField> iterator() {
        return fields.iterator();
    }

}
