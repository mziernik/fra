package com.model.dataset;

import com.cache.CachedData;
import com.json.JObject;
import com.lang.LDatabase;
import com.model.dataset.intf.DataSetException;
import com.utils.CSV.CSVWriter;
import com.utils.collections.Pair;
import com.utils.collections.Pairs;
import com.utils.collections.TList;
import com.utils.collections.Triple;
import com.webapi.core.WebApiRequest;
import com.xml.XML;
import com.xml.XmlException;
import java.io.IOException;
import java.io.StringWriter;
import java.lang.reflect.Field;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.util.*;

public class DsUtils {

    public static CachedData export(AbstractDataSet<?, ?, ?> dataSet, WebApiRequest req, JObject jExport) throws IOException, XmlException {

        switch (jExport.getStr("format").toLowerCase()) {

            case "xls": {
                throw new UnsupportedOperationException(LDatabase.UNSUPPORTED_FORMAT.toString());
            }
            case "html": {
                throw new UnsupportedOperationException(LDatabase.UNSUPPORTED_FORMAT.toString());
            }

            case "csv": {
                StringWriter wr = new StringWriter();
                CSVWriter csv = new CSVWriter(wr);
                /*
                csv.writeNext(new Strings().map(columns.values(), (AbstractDsColumn<ROW, ?> o) -> {
                    return o.key;
                }).toArray());

                csv.writeNext(new Strings().map(columns.values(), (AbstractDsColumn<ROW, ?> o) -> {
                    return o.caption.toString();
                }).toArray());
                 */
//                for (JArray arr : jRows.getArrays())
//
//                    csv.writeNext(new Strings().map(arr, (JElement el) -> {
//                        return el.isValue() ? el.asString() : el.toString();
//                    }).toArray());

                csv.flush();
                csv.close();

                return new CachedData("bla", "bbb", "export.csv", 300, wr.toString().getBytes());
            }

            case "json": {

                JObject json = new JObject();

                /*   AbstractDsColumn[] cols = columns.values().toArray(new AbstractDsColumn[0]);

                JObject jcols = json.objectC("columns");
                for (AbstractDsColumn col : cols)
                    jcols.objectC(col.key).put("key", col.caption);

                json.put("rows", jRows);

                return new CachedData("bla", "bbb", "export.json", 300, json.toString().getBytes());*/
            }
            case "xml": {
                XML xml = new XML("<" + dataSet.key + "/>");
                /*
                AbstractDsColumn[] cols = columns.values().toArray(new AbstractDsColumn[0]);

                XmlNode xCols = xml.nodeC("columns");

                for (AbstractDsColumn col : cols)
                    xCols.nodeC(col.key).attr("key", col.caption);

                XmlNode xRows = xml.nodeC("rows");

                for (JArray arr : jRows.getArrays()) {
                    XmlNode xRow = xRows.addNode("row");

                    int i = 0;
                    for (JElement el : arr) {
                        AbstractDsColumn col = cols[i++];
                        XmlNode node = xRow.addNode(col.key);

                        if (el.isArray()) {
                            for (JElement e : el.asArray())
                                node.addNode(col.key).setText(e.isValue() ? e.asString() : e.toString());
                            continue;
                        }

                        node.setText(el.isValue() ? el.asString() : el.toString());
                    }

                }

                return new CachedData("bla", "bbb", "export.xml", 300, xml.toString().getBytes());*/
            }
        };

        return null;
    }

}

class DsMetaData {

    final static Map<Class<? extends AbstractDataSet>, DsMetaData> initData = new HashMap<>();
    final List<Triple<Field, Class<?>, Class<? extends AbstractDataSet<?, ?, ?>>>> list = new TList<>();

    static void load(AbstractDataSet<?, ?, ?> dataSet) throws Exception {

        DsMetaData data = initData.get(dataSet.getClass());

        if (data == null) {

            data = new DsMetaData(dataSet);
            initData.put(dataSet.getClass(), data);

            if (data.list.size() != dataSet._columns.size())
                throw new DataSetException("Incorrect init data size");

        }

        Iterator<Triple<Field, Class<?>, Class<? extends AbstractDataSet<?, ?, ?>>>> itr = data.list.iterator();

        for (DsColumn<?, ?, ?, ?> col : dataSet._columns) {
            Triple<Field, Class<?>, Class<? extends AbstractDataSet<?, ?, ?>>> t = itr.next();

            if (t.first != null)
                col.field = t.first;
            if (t.second != null)
                col.clazz = t.second;
            if (t.third != null)
                col.foreignClass = t.third;

            dataSet.initCol((DsColumn) col);

        }
    }

    public DsMetaData(AbstractDataSet<?, ?, ?> dataSet) throws Exception {

        Pairs<Field, DsColumn> cols = new Pairs<>();

        for (Field f : dataSet.getClass().getDeclaredFields()) {
            f.setAccessible(true);
            Object obj = f.get(dataSet);
            if ((obj instanceof DsColumn))
                cols.add(f, (DsColumn) obj);
        }

        for (DsColumn<?, ?, ?, ?> col : dataSet._columns) {

            Triple<Field, Class<?>, Class<? extends AbstractDataSet<?, ?, ?>>> t = new Triple<>(null, null, null);

            for (Pair<Field, DsColumn> pair : cols) {
                if (pair.second != col)
                    continue;

                Field f = pair.first;
                t.first = f;

                if (f.getGenericType() instanceof ParameterizedType) {
                    Type[] types = ((ParameterizedType) f.getGenericType()).getActualTypeArguments();
                    if (types != null && types.length >= 1)
                        if (types[0] instanceof Class)
                            t.second = (Class) types[0];
                        else if (types[0] instanceof ParameterizedType) {
                            ParameterizedType pt = (ParameterizedType) types[0];
                            t.second = (Class) pt.getRawType();
                        }

                    if (types != null && types.length >= 2
                            && types[1] instanceof Class
                            && AbstractDataSet.class.isAssignableFrom((Class<?>) types[1]))
                        t.third = (Class<? extends AbstractDataSet<?, ?, ?>>) types[1];
                }
            }
            list.add(t);
        }

    }

}
