package com.model.demo;

import com.cache.CachedData;
import com.exceptions.ThrowableException;
import com.model.repository.Column;
import com.model.repository.Record;
import com.model.repository.RecordCallback;
import com.model.repository.Repository;
import com.model.repository.intf.CRUDE;
import com.resources.FontAwesome;
import com.resources.core.Resources;
import com.utils.Utils;
import com.utils.collections.Pair;
import com.utils.collections.Quad;
import com.utils.collections.Triple;
import com.utils.date.TDate;
import com.utils.reflections.datatype.*;
import java.util.UUID;

public class RDemo1 extends Repository<Integer> {

    public final static String[] FORMATS = {"DOC", "PDF", "HTML", "CSS", "JS", "XML", "JSON"};

    public final static Column<Integer> ID = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.INT;
        c.key = "id";
        c.name = "ID";
        c.autoGenerated = true;
        c.required = true;
        c.readOnly = true;
        c.unique = true;
    });

    public final static Column<CachedData> FILE = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = BinaryDataType.file();
        c.key = "file";
        c.name = "Plik";
        c.daoName = null;
        c.required = true;
    });

    public final static Column<CachedData> IMAGE = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = BinaryDataType.image();
        c.key = "image";
        c.name = "Obrazek";
        c.daoName = null;
    });

    public final static Column<String> ON_DEMAND = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = EnumDataType.ofArray(FORMATS);
        c.key = "onDemand";
        c.name = "Na żądanie";

        c.serializer = (arg) -> "Duuuuuuuuuuża ilość tekstu " + Utils.random();
    });

    public final static Column<String> ONE_OF = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = EnumDataType.ofArray(FORMATS);
        c.key = "oneOf";
        c.name = "Jeden format";
    });

    public final static Column<String[]> SOME_OF = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = EnumsDataType.ofArray(FORMATS);
        c.key = "someOf";
        c.name = "Kilka formatów";
    });

    public final static Column<Pair<Boolean, String>> PAIR = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new PairDataType<>(DataType.BOOLEAN, DataType.STRING);
        c.key = "pair";
        c.name = "Para";
    });

    public final static Column<Triple<Boolean, String, Integer>> TRIPLE = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new TripleDataType<>(DataType.BOOLEAN, DataType.STRING, DataType.INT);
        c.key = "triple";
        c.name = "Potrójny";
    });

    public final static Column<Quad<Boolean, String, Integer, String>> QUADRO = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new QuadDataType<>(DataType.BOOLEAN, DataType.STRING, DataType.INT, DataType.STRING);
        c.key = "quad";
        c.name = "Poczwórny";
    });

    public final static Column<Pair<Boolean, String>[]> PAIR_LIST = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new ArrayDataType<>(new PairDataType<>(DataType.BOOLEAN, DataType.STRING));
        c.key = "pairList";
        c.name = "Lista par";
    });

    public final static Column<Triple<Boolean, String, Integer>[]> TRIPLE_LIST = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new ArrayDataType<>(new TripleDataType<>(DataType.BOOLEAN, DataType.STRING, DataType.INT));
        c.key = "tripleList";
        c.name = "Lista potrójnych";
        c.daoName = null;
    });

    public final static Column<Quad<Boolean, String, Integer, String>[]> QUAD_LIST = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new ArrayDataType<>(new QuadDataType<>(DataType.BOOLEAN,
                DataType.STRING, DataType.INT, DataType.STRING));
        c.key = "quadList";
        c.name = "Lista poczwórnych";
    });

    public final static Column<DataType> TYPE = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = new EnumDataType<DataType>(DataType.class, DataType.ALL.values(),
                dt -> dt.name,
                dt -> dt.description != null ? dt.description.toString() : dt.name);
        c.key = "type";
        c.name = "Typ danych";
    });

    public final static Column<String> TEST = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.STRING;
        c.key = "test";
        c.name = "Test";
        c.daoName = null;
        //   c.required = true;
        c.autoGenerated = true;
        c.readOnly = true;
        c.defaultValue = "";
    });

    public final static Column<UUID> UID = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.UUID;
        c.key = "uid";
        c.name = "UID";
        //   c.required = true;
        c.autoGenerated = true;
        c.readOnly = true;
        c.unique = true;
        c.hidden = true;
    });

    public final static Column<TDate> CREATED = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.TIMESTAMP;
        c.key = "created";
        c.name = "Utworzono";
        c.required = true;
        c.autoGenerated = true;
        c.readOnly = true;
    });

    public final static Column<String> KEY = new Column<String>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.KEY;
        c.key = "key";
        c.name = "Klucz";
        c.unique = true;
        //    c.required = true;
    });

    public final static Column<String> NAME = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.STRING;
        c.key = "name";
        c.name = "Nazwa";
        //     c.required = true;
    });
    /*
    public final static ForeignColumn<Integer, RCategory> PARENT = new ForeignColumn<>(c -> {
        c.repository = RTest.class;
        c.type = DataType.INT;
        c.key = "parent";
        c.name = "Rodzic";
    }, RCategory.ID);

    public final static ForeignColumns<Integer, RAttributeElement> ELEMENTS = new ForeignColumns<>(c -> {
        c.repository = RTest.class;
        c.type = DataType.INT.asArray();
        c.list = true;
        c.key = "elements";
        c.name = "Elementy";
    }, RAttributeElement.ID);

    public final static Column<String[]> DEF_VAL = new Column<>(c -> {
        c.repository = RTest.class;
        c.type = DataType.STRING.asArray();
        c.list = true;
        c.daoName = "def_val";
        c.key = "defVal";
        c.name = "Wartość domyślna";
    });
     */

    public final static Column<Boolean[]> REQUIRED = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.BOOLEAN.asArray();
        c.key = "required";
        c.name = "Wymagane";
    });

    // ToDo Dodać repozytorium ikon
    public final static Column<FontAwesome> ICON = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.ICON;
        c.key = "icon";
        c.name = "Ikona";
        c.required = false;
        // c.defaultValue = FontAwesome.ADJUST;
    });

    public final static Column<String> DESCRIPTION = new Column<>(c -> {
        c.repository = RDemo1.class;
        c.type = DataType.MEMO;
        c.key = "desc";
        c.daoName = "description";
        c.name = "Opis";
        c.max = 1000;
    });

    public RDemo1() {
        super(c -> {
            c.key = "demo1";
            c.daoName = null;
            c.name = "Demo 1";
            c.group = "Demo";
            c.primaryKey = ID;
            c.displayName = NAME;

            c.repoAction("addR", "Dodaj", ActionType.PRIMARY, FontAwesome.PLUS, null, data -> null);
            c.repoAction("remR", "Usuń", ActionType.WARNING, FontAwesome.TRASH, null, data -> null);

            c.recordAction("raddR", "Dodaj", ActionType.PRIMARY, FontAwesome.PLUS, null, data -> null);
            c.recordAction("rremR", "Usuń", ActionType.PRIMARY, FontAwesome.TRASH, null, data -> null);

            c.createCallback = (RecordCallback data) -> {

                data.field(NAME, rf -> {
                    rf.name = "NazwaXX";
                    rf.hint = "Hint - Nazwa XXX";
                    rf.value = "Nazwa " + new TDate().getTime();
                    rf.description = "Nowy opis";
                    rf.min = 3;
                    rf.max = 6;
                    rf.warning = "ACHTUNG!";
                });

                data.field(ON_DEMAND, rf -> {
                    rf.enumerate.put("aaa", "AAAA");
                    rf.enumerate.put("bbb", "BBBB");
                    rf.enumerate.put("ccc", "CCC");
                });

                data.field(DESCRIPTION, rf -> {
                    rf.value = "Opis " + new TDate().getTime();
                    rf.description = "Opis...... " + Utils.randomId();
                    rf.min = 3;
                    rf.max = 100000;
                });

            };

            c.editCallback = (RecordCallback data) -> {
                System.out.println("");
            };

            c.validateCallback = (RecordCallback data) -> {
                System.out.println("");
            };

        });

        onBeforeUpdate.listen(this, (records, all) -> {
            for (Record rec : records) {
                if (rec.crude == CRUDE.CREATE)
                    rec.set(ID, this.max(ID, 1) + 1);
                rec.set(TEST, status.getRevision() + " -> " + Utils.randomId()); //  rec.set(DESCRIPTION, getUpdatesCount() + " -> " + Utils.randomId());
            }
        });

        try {
            CachedData cdFile = new CachedData("src", "file", "plik.txt", 0, "Zawartość pliku".getBytes());
            CachedData cdImage = new CachedData("src", "file", "img.png", 0, Resources.get(RDemo1.class, "img.png").getData());

            localUpdate(null)
                    .set(ID, 1)
                    .set(FILE, cdFile)
                    .set(IMAGE, cdImage)
                    .set(KEY, "asfdsfds")
                    .set(NAME, "wereffg")
                    .set(ONE_OF, "XML")
                    .set(SOME_OF, new String[]{"CSS", "JS"})
                    .set(PAIR, new Pair<>(false, "dwa"))
                    .set(TRIPLE, new Triple<>(true, "trzy", 3))
                    .set(QUADRO, new Quad<>(false, "cztery", 4, "four"))
                    .set(PAIR_LIST, new Pair[]{
                new Pair<>(false, "X"),
                new Pair<>(true, "Y"),
                new Pair<>(false, "Z")
            }).set(TRIPLE_LIST, new Triple[]{
                new Triple<>(false, "a", 10),
                new Triple<>(true, "b", 11),
                new Triple<>(false, "c", 12)
            }).set(QUAD_LIST, new Quad[]{
                new Quad<>(false, "1", 1, "jeden"),
                new Quad<>(true, "2", 2, "dwa"),
                new Quad<>(false, "3", 3, "trzy")
            }).update();

        } catch (Exception e) {
            throw new ThrowableException(e);
        }
    }

}