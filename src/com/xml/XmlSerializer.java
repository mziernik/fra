package com.xml;

import com.lang.LXml;
import com.utils.reflections.Reflections;
import com.xml.*;
import com.mlogger.Log;
import com.utils.reflections.*;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

/**
 * Miłosz Ziernik 2013/02/26
 */
public class XmlSerializer {
    // anotacja umożliwiająca dodawanie opisów dla pól

    @Target(value = {ElementType.FIELD})
    @Retention(value = RetentionPolicy.RUNTIME)
    public @interface INode {

        public String description() default ""; // opis

        public boolean mustExists() default false;

        public String mapKeyName() default "key";

        public String mapValueName() default "value";
    }
    //-------------------------------------------------------------------------

    public static interface IWriteValue {

        public String serialize(Field field, Object value, String text);
    }

    public static interface IHandleUnknownField {

        public String handleUnknownField(Field field, Object value);
    }

    public static interface ILoadError {

        public String onLoadError(Field field, Exception ex) throws Exception;
    }
    //-------------------------------------------------------------------------
    public boolean useAnnotations = true;
    public boolean skipNullFields = true;
    public boolean trimInnerText = true; // automatycznie przycinaj wartości XMLa podczas deserializacji
    public boolean addClassNameAsComment = false;
    public IWriteValue writeIntf; // interfejs umożliwiający ręczną deserializację
    public IHandleUnknownField handleUnknownField = null; // obsługa nieznanych typów
    public ILoadError loadError;

    public XmlSerializer useAnnotations(boolean useAnnotations) {
        this.useAnnotations = useAnnotations;
        return this;
    }

    public XmlSerializer skipNullFields(boolean skipNullFields) {
        this.skipNullFields = skipNullFields;
        return this;
    }

    public XmlSerializer trimInnerText(boolean trimInnerText) {
        this.trimInnerText = trimInnerText;
        return this;
    }

    public XmlSerializer addClassNameAsComment(boolean addClassNameAsComment) {
        this.addClassNameAsComment = addClassNameAsComment;
        return this;
    }

    public void serialize(XmlNode node, Object object)
            throws XmlException {
        enumSerialize(node, object);
    }

    public XML serialize(Object object, String rootName) throws XmlException {
        XML xml = new XML("<" + (rootName) + "/>");
        if (object != null)
            enumSerialize(xml, object);

        return xml;
    }

    public void deserialize(XML xml, Object obj) throws Exception {
        if (xml == null)
            return;
        enumDeserialize(xml, obj);
    }

    public void deserialize(XmlNode node, Object obj) throws Exception {
        enumDeserialize(node, obj);
    }

    private String getInnerText(XmlNode node, INode ann) throws XmlException {
        if (node == null)
            return "";
        if (ann != null && ann.mustExists() && node.getText().trim().isEmpty())
            throw new XmlException(LXml.VALUE_CANT_BE_EMPTY.toString(node.getPath()));

        return trimInnerText ? node.getText().trim() : node.getText();
    }

    //  @SuppressWarnings("unchecked")
    private void enumDeserialize(XmlNode node, Object cObject) throws Exception {

        if (node == null)
            return;

        Field[] fds = cObject.getClass().getFields();
        for (Field f : fds)
            try {
                int mod = f.getModifiers();
                if (Modifier.isAbstract(mod) || Modifier.isPrivate(mod))
                    continue;

                INode ann = f.getAnnotation(INode.class);
                XmlNode nd = node.node(f.getName());

                if (ann != null && ann.mustExists() && nd == null)
                    throw new XmlException(LXml.LACK_OF_NEEDED_VALUE.toString(node.getPath().add(f.getName())));

                if (nd == null)
                    continue;

                if (Reflections.isSimpleField(f, false)) {
                    String val = getInnerText(nd, ann);
                    if (val != null && val.trim().isEmpty() && f.getType() != String.class)
                        continue;
                    new TField(f).set(cObject, val);
                    continue;
                }

                Class<?> cType = f.getType();
                Type gType = f.getGenericType();

                if (gType instanceof ParameterizedType) {
                    ParameterizedType parameterizedType = (ParameterizedType) gType;
                    Type rawType = parameterizedType.getRawType();

                    Type[] tArgs = parameterizedType.getActualTypeArguments();
                    if (tArgs == null || tArgs.length == 0)
                        continue;

                    List<XmlNode> nodes = node.getNodes(f.getName());

                    if (rawType == Map.class && tArgs.length == 2) {

                        String mapKeyName = "key";
                        String mapValueName = "value";

                        if (useAnnotations && ann != null) {
                            if (ann.description() != null
                                    && !ann.description().trim().isEmpty())
                                node.addComment(ann.description());
                            if (ann.mapKeyName() != null
                                    && !ann.mapKeyName().trim().isEmpty())
                                mapKeyName = ann.mapKeyName();
                            if (ann.mapValueName() != null
                                    && !ann.mapValueName().trim().isEmpty())
                                mapValueName = ann.mapValueName();
                        }

                        HashMap<Object, Object> map = new HashMap<>();
                        f.set(cObject, map);
                        for (XmlNode nn : nodes)
                            try {

                                XmlNode nKey = nn.node(mapKeyName);
                                XmlNode nVal = nn.node(mapValueName);

                                Object key = null;
                                Object val = null;

                                if (nKey != null) {
                                    key = new TClass(tArgs[0]).deserialize(getInnerText(nKey, ann));
                                    if (key == null) {
                                        key = ((Class) tArgs[0]).newInstance();
                                        enumDeserialize(nKey, key);
                                    }
                                }

                                if (nVal != null) {
                                    val = new TClass(tArgs[1]).deserialize(getInnerText(nVal, ann));
                                    if (val == null) {
                                        val = ((Class) tArgs[1]).newInstance();
                                        enumDeserialize(nVal, val);
                                    }
                                }

                                map.put(key, val);
                            } catch (Exception e) {
                                if (loadError != null)
                                    loadError.onLoadError(f, e);
                                else
                                    throw e;
                            }
                        continue;
                    }

                    Type tt = tArgs[0];
                    if (rawType == List.class) {
                        LinkedList<Object> ll = new LinkedList<>();
                        f.set(cObject, ll);

                        for (XmlNode nn : nodes)
                            try {
                                Object obj = new TClass(tt).deserialize(getInnerText(nn, ann));
                                if (obj == null) {
                                    obj = ((Class) tt).newInstance();
                                    enumDeserialize(nn, obj);
                                }

                                ll.add(obj);
                            } catch (Exception e) {
                                if (loadError != null)
                                    loadError.onLoadError(f, e);
                                else
                                    throw e;
                            }
                        continue;
                    }

                    if (!(rawType instanceof Class))
                        throw new IllegalArgumentException();
                }

                if (cType.isArray()) {

                    List<XmlNode> nodes = node.getNodes(f.getName());
                    Class<?> aType = cType.getComponentType();

                    Object oArray = Array.newInstance(aType, nodes.size());
                    f.set(cObject, oArray);

                    for (int i = 0; i < nodes.size(); i++)
                        try {
                            String oStr = getInnerText(nodes.get(i), ann);
                            Object obj = new TClass(aType).deserialize(oStr);
                            if (obj == null) {
                                obj = ((Class) aType).newInstance();
                                enumDeserialize(nodes.get(i), obj);
                            }
                            Array.set(oArray, i, obj);
                        } catch (Exception e) {
                            if (loadError != null)
                                loadError.onLoadError(f, e);
                            else
                                throw e;
                        }

                    continue;
                }

            } catch (Exception e) {
                if (loadError != null)
                    loadError.onLoadError(f, e);
                else
                    throw e;
            }
    }

    private XmlNode enumSerialize(XmlNode node, Object cObject)
            throws XmlException {

        if (node == null || cObject == null)
            return null;

        if (Reflections.isSimpleField(cObject.getClass(), false)) {
            String sVal = cObject.toString();
            if (writeIntf != null)
                sVal = writeIntf.serialize(null, cObject, sVal);
            node.setText(sVal);
            return null;
        }

        Field[] fds = cObject.getClass().getFields();
        for (Field f : fds) {
            int mod = f.getModifiers();
            if (Modifier.isAbstract(mod) || Modifier.isPrivate(mod))
                continue;

            Object obj;
            try {
                obj = f.get(cObject);
                if (obj == null && skipNullFields)
                    continue;
            } catch (Exception e) {
                Log.warning(e);
                continue;
            }

            String mapKeyName = "key";
            String mapValueName = "value";
            INode ann = f.getAnnotation(INode.class);
            if (useAnnotations && ann != null) {
                if (ann.description() != null
                        && !ann.description().trim().isEmpty())
                    node.addComment(ann.description());
                if (ann.mapKeyName() != null
                        && !ann.mapKeyName().trim().isEmpty())
                    mapKeyName = ann.mapKeyName();
                if (ann.mapValueName() != null
                        && !ann.mapValueName().trim().isEmpty())
                    mapValueName = ann.mapValueName();
            }

            Class<?> cType = f.getType();
            if (addClassNameAsComment && obj != null)
                node.addComment(cType.getSimpleName());

            if (cType.isArray()) {
                int length = Array.getLength(obj);
                for (int i = 0; i < length; i++) {
                    Object aObj = Array.get(obj, i);
                    if (aObj == null)
                        continue;
                    if (addClassNameAsComment)
                        node.addComment(cType.getComponentType().getSimpleName());
                    enumSerialize(node.nodeC(f.getName()), aObj);
                }

                continue;
            }

            if (obj instanceof Map) {
                Map map = (Map) obj;
                Iterator itr = map.keySet().iterator();

                while (itr.hasNext()) {
                    Object key = itr.next();
                    Object val = map.get(key);
                    if (addClassNameAsComment) {
                        String s1 = key != null ? key.getClass().getSimpleName()
                                : "null";
                        String s2 = key != null ? val.getClass().getSimpleName()
                                : "null";
                        node.addComment("<" + s1 + ", " + s2 + ">");
                    }
                    XmlNode nd = node.nodeC(f.getName());
                    if (addClassNameAsComment && key != null)
                        nd.addComment(key.getClass().getSimpleName());
                    enumSerialize(nd.nodeC(mapKeyName), key);
                    if (addClassNameAsComment && val != null)
                        nd.addComment(val.getClass().getSimpleName());
                    enumSerialize(nd.nodeC(mapValueName), val);

                }
                continue;
            }

            if (obj instanceof Iterable) {
                Iterator itr = ((Iterable) obj).iterator();
                while (itr.hasNext()) {
                    Object iObj = itr.next();
                    if (iObj == null)
                        continue;
                    if (addClassNameAsComment)
                        node.addComment(iObj.getClass().getSimpleName());
                    enumSerialize(node.nodeC(f.getName()), iObj);
                }
                continue;
            }

            XmlNode nd = node.nodeC(f.getName());
            nd.annotation = ann;

            if (obj != null && Reflections.isSimpleField(obj.getClass(), false)) {
                String sVal = obj.toString();
                if (writeIntf != null)
                    sVal = writeIntf.serialize(f, obj, sVal);
                nd.setText(sVal);
                continue;
            }

            if (obj != null && cType.isMemberClass())
                enumSerialize(nd, obj);

            if (handleUnknownField != null)
                nd.setText(handleUnknownField.handleUnknownField(f, obj));

        }
        return null;
    }
}
