package com.utils;

import com.utils.text.StrWriter;
import com.utils.collections.Strings;
import java.io.*;
import java.util.*;

public class JavaFile {

    private final static String INTENT = "    ";

    public String pckg;
    public final LinkedHashSet<String> imports = new LinkedHashSet<>();
    public final LinkedHashSet<JavaClass> classes = new LinkedHashSet<>();

    public String defaultIntent = "";
    public final String name;

    public JavaFile(String _package, String name) {
        this.pckg = _package;
        this.name = name;
    }

    public JavaFile import_(String import_) {
        imports.add(import_);
        return this;
    }

    public JavaClass addClass(final String name) {
        JavaClass c = new JavaClass(name);
        classes.add(c);
        return c;
    }

    public void savePckg(File root) throws IOException {
        save(new File(root, pckg.replace(".", "/") + "/" + name + ".java"));
    }

    public void save(File file) throws IOException {
        file.getParentFile().mkdirs();
        try (OutputStreamWriter writer = new OutputStreamWriter(
                new BufferedOutputStream(
                        new FileOutputStream(file)), Utils.UTF8)) {
            write(writer);
            writer.flush();
        }
    }

    @Override
    public String toString() {
        StrWriter wr = new StrWriter();

        write(wr);

        return wr.toString();

    }

    public static String getCanonicalName(Class<?> cls) {
        String cname = cls.getCanonicalName();
        if (cname.startsWith("java.lang."))
            cname = cname.substring("java.lang.".length());
        return cname;
    }

    public StrWriter write(Writer writer) {
        StrWriter w = new StrWriter(writer);

        w.append("package ").append(pckg).append(";\n\n");

        for (String s : imports)
            w.append("import ").append(s).append(";\n");

        for (JavaClass cls : classes) {
            w.append("\n");
            cls.write(w, "");
        }
        return w;
    }

    public JavaFile importIfRequired(Class cls) {
        if (cls == null || cls.getPackage() == null)
            return this;
        String pck = cls.getPackage().getName();
        if (!pck.startsWith("java.lang"))
            imports.add(cls.getName());
        return this;
    }

    public abstract class JavaElement<Self extends JavaElement> {

        public final String name;
        protected final Strings modifiers = new Strings().unique(true);
        protected final Strings annotations = new Strings();

        public JavaElement(final String name) {
            this.name = name;
        }

        public Self modifier(String modifier) {
            modifiers.add(modifier.toLowerCase());
            return (Self) this;
        }

        public Self annotation(String content) {
            annotations.add(content);
            return (Self) this;
        }

        public JavaFile getFile() {
            return JavaFile.this;
        }

    }

    public class JavaField extends JavaElement<JavaField> {

        public final String type;
        public final StrWriter body = new StrWriter();
        private String after;
        private String before;

        public JavaField after(String after) {
            this.after = after;
            return this;
        }

        public JavaField before(String before) {
            this.before = before;
            return this;
        }

        public JavaField(final Class<?> type, final String name) {
            this(getCanonicalName(type), name);
        }

        public JavaField(final String type, final String name) {
            super(name);
            this.type = type;
        }

        public void write(StrWriter writer, String intent) {

            if (before != null)
                writer.append(before);

            for (String s : annotations)
                writer.append(intent).append(s).append("\n");

            int len = writer.length();
            writer.append(intent).append(modifiers.toString(" "))
                    .append(" ").append(type).append(" ").append(name);

            if (!body.isEmpty()) {

                len = writer.length() - len;
                String bdy = body.toString();

                if (len + 3 + bdy.length() > 90)
                    writer.append("\n").append(intent + INTENT).append("= ");
                else
                    writer.append(" = ");

                writer.append(bdy);
            }

            writer.append(";");

            if (after != null)
                writer.append(after);

        }

    }

    public class JavaClass extends JavaElement<JavaClass> {

        private final Strings extends_ = new Strings().unique(true);
        private final Strings implements_ = new Strings().unique(true);
        private final Strings genericTypes = new Strings().unique(true);
        // private final Strings throws_ = new Strings().unique(true);
        private final Strings annotations = new Strings().unique(true);

        public final LinkedHashSet<JavaClass> classes = new LinkedHashSet<>();
        public final LinkedHashSet<JavaMethod> methods = new LinkedHashSet<>();
        public final LinkedHashSet<JavaField> fields = new LinkedHashSet<>();

        public JavaClass(String name) {
            super(name);
        }

        public JavaClass extend(String extend) {
            extends_.add(extend);
            return this;
        }

        public JavaClass implement(String implement) {
            implements_.add(implement);
            return this;
        }

        public JavaClass genericType(Class<?> genericType) {
            genericTypes.add(getCanonicalName(genericType));
            return this;
        }

        public JavaClass genericType(String genericType) {
            genericTypes.add(genericType);
            return this;
        }

        public JavaField addField(final Class<?> type, final String name) {
            JavaField f = new JavaField(type, name);
            fields.add(f);
            return f;
        }

        public JavaField addField(final String type, final String name) {
            JavaField f = new JavaField(type, name);
            fields.add(f);
            return f;
        }

        public JavaClass addClass(final String name) {
            JavaClass c = new JavaClass(name);
            classes.add(c);
            return c;
        }

        public JavaMethod addMethod(final Class<?> returnType, final String name) {
            return addMethod(getCanonicalName(returnType), name);
        }

        public JavaMethod addMethod(final String returnType, final String name) {
            JavaMethod m = new JavaMethod(returnType, name);
            methods.add(m);
            return m;
        }

        public void write(StrWriter writer, String intent) {

            for (String s : annotations)
                writer.append(intent).append(s).append("\n");

            writer.append(intent).append(modifiers.toString(" "));

            if (!modifiers.isEmpty())
                writer.append(" ");
            writer.append("class ").append(name);

            if (!genericTypes.isEmpty())
                writer.append("<").append(genericTypes.toString(", ")).append(">");

            if (!extends_.isEmpty())
                writer.append(" extends ").append(extends_.toString(", "));

            if (!implements_.isEmpty())
                writer.append(" implements ").append(implements_.toString(", "));

            writer.append(" {\n\n");

            for (JavaField f : fields) {
                f.write(writer, intent + INTENT);
                writer.append("\n");
            }

            if (!fields.isEmpty())
                writer.append("\n");

            for (JavaMethod m : methods) {
                m.write(writer, intent + INTENT);
                writer.append("\n\n");
            }

            writer.append("\n}");
        }

    }

    public class JavaMethod extends JavaElement<JavaMethod> {

        public final String returnType;
        public final StrWriter body = new StrWriter();
        private final List<JavaParameter> arguments = new LinkedList<>();
        private final Strings throws_ = new Strings();

        public JavaMethod(final String returnType,
                final String name) {
            super(name);
            this.returnType = returnType;
        }

        public JavaMethod argument(Class<?> type, String name) {
            arguments.add(new JavaParameter(getCanonicalName(type), name));
            return this;
        }

        public JavaMethod throw_(Class<?> type) {
            throws_.add(getCanonicalName(type));
            return this;
        }

        public JavaMethod argument(String type, String name) {
            arguments.add(new JavaParameter(type, name));
            return this;
        }

        public void write(StrWriter writer, String intent) {

            for (String s : annotations)
                writer.append(intent).append(s).append("\n");

            writer.append(intent);

            boolean args = !arguments.isEmpty()
                    || (name != null && !name.isEmpty()
                    || (returnType != null && !returnType.isEmpty()));

            writer.append(modifiers.toString(" "));
            if (!modifiers.isEmpty())
                writer.append(" ");

            if (returnType != null && !returnType.isEmpty())
                writer.append(returnType);

            if (returnType != null && name != null)
                writer.append(" ");

            if (name != null)
                writer.append(name);

            if (args)
                writer.append("(");

            boolean first = true;
            for (JavaParameter s : arguments) {
                if (!first)
                    writer.append(", ");
                first = false;
                s.write(writer, intent);
            }

            if (args)
                writer.append(") ");

            if (!throws_.isEmpty())
                writer.append("throws ")
                        .append(throws_.toString(", "))
                        .append(" ");

            writer.append("{");

            for (String s : body.toString().split("\\n"))
                writer.append("\n").append(intent).append(INTENT).append(s);

            writer.append("\n").append(intent).append("}");

        }
    }

    public class JavaParameter extends JavaElement<JavaParameter> {

        public final String type;

        public JavaParameter(String type, String name) {
            super(name);
            this.type = type;
        }

        public void write(StrWriter writer, String intent) {
            writer.append(type).append(" ").append(name);
        }
    }

}
