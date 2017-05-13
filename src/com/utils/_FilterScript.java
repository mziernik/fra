package com.utils;

import com.utils.text.StrWriter;

import static com.utils._FilterScript.ItemOperator.*;

import com.database.Database;
import com.lang.LUtil;
import com.lang.core.LString;
import com.utils.collections.Strings;
import com.utils.collections.Triple;
import java.io.*;
import java.util.*;

public class _FilterScript implements Iterable<_FilterScript.ScriptItem> {

    public final List<ScriptItem> items = new LinkedList<>();
    public final FilterScripts scripts;

    public static interface IAddItemCallback {

        public boolean canAdd(ScriptItem item);
    }

    public static interface ISqlEscapeCallback {

        public String getQuery(ScriptItem item, String query, String value, Triple<String, String, String> params);
    }

    public _FilterScript(FilterScripts scripts) {
        this.scripts = scripts;
    }

    public static class FilterScripts extends LinkedList<_FilterScript> {

        private final IAddItemCallback addCallback;

        public FilterScripts(String text, IAddItemCallback addCallback) {
            this.addCallback = addCallback;
            if (text == null || text.trim().isEmpty())
                return;

            _FilterScript script = new _FilterScript(this);
            add(script);

            for (String s : text.split("\n")) {

                if (s.trim().startsWith("#"))
                    continue;

                Character sep = null;
                for (int i = 0; i < s.length(); i++)
                    if (s.charAt(i) > 32) {
                        if (sep == null)
                            sep = s.charAt(i);
                        if (sep != s.charAt(i)) {
                            sep = null;
                            break;
                        }
                    }

                if (sep != null) {
                    script = new _FilterScript(this);
                    add(script);
                    continue;
                }

                if (!s.trim().isEmpty())
                    script.add(s);
            }
        }

        private class ScriptQuery extends LinkedList<ScriptQuery> {

            public final String query;
            public final String joiner;
            private boolean not;

            public ScriptQuery(String query, String joiner) {
                this.query = query;
                this.joiner = joiner;
            }

            public List<String> getChildQueries() {
                final List<String> list = new LinkedList<>();
                new Object() {
                    void process(ScriptQuery sc) {
                        if (sc != null && sc.isEmpty())
                            list.add(sc.query);
                        for (ScriptQuery sq : sc)
                            process(sq);
                    }
                }.process(this);
                return list;
            }

        }

        private class SqlBuilder {

            final StrWriter sb = new StrWriter();
            private String space;
            private String intent;
            private final ISqlEscapeCallback callback;

            private SqlBuilder(ISqlEscapeCallback callback) {
                this.callback = callback;
            }

            private StrWriter space(int level) {
                sb.append(space);
                for (int i = 0; i < level; i++)
                    sb.append(intent);
                return sb;
            }

            private String toString(String space, String intent) {
                this.space = space;
                this.intent = intent;

                ScriptQuery main = new ScriptQuery("", "OR");

                for (_FilterScript script : FilterScripts.this) {
                    ScriptQuery qScript = new ScriptQuery("", "AND");
                    main.add(qScript);

                    for (ScriptItem item : script) {
                        ScriptQuery qItem = new ScriptQuery("", "OR");
                        qScript.add(qItem);
                        qItem.not = item.not;

                        for (String value : item.values) {
                            Strings lst = new Strings();
                            lst.add(item.name);

                            if (item.operator != null)
                                switch (item.operator) {
                                    case lessThan:
                                        lst.add("<");
                                        lst.add(value);
                                        break;
                                    case moreThan:
                                        lst.add(">");
                                        lst.add(value);
                                        break;
                                    case lessTanOrEquals:
                                        lst.add("<=");
                                        lst.add(value);
                                        break;
                                    case moreThanOrEquals:
                                        lst.add(">=");
                                        lst.add(value);
                                        break;

                                    case equals:
                                        if (item.caseSensitive) {
                                            lst.add("=");
                                            lst.add("'" + Database.escapeSQL(value) + "'");
                                        } else {
                                            lst.add("");
                                            lst.add("ILIKE('" + Database.escapeSQL(value) + "')");
                                        }
                                        break;
                                    case contains:
                                        lst.add("");
                                        lst.add("ILIKE('%" + Database.escapeSQL(value) + "%')");
                                        break;
                                }

                            String s = lst.toString(" ");
                            if (callback != null)
                                s = callback.getQuery(item, s, value,
                                        new Triple<>(lst.get(0), lst.get(1), lst.get(2)));

                            if (s == null || s.trim().isEmpty())
                                continue;

                            qItem.add(new ScriptQuery(s, ""));
                        }
                    }
                }

                new Object() {
                    void processSingleLine(ScriptQuery query) {
                        if (query.query.isEmpty() && query.isEmpty())
                            return;

                        sb.append(query.query);
                        if (!query.isEmpty()) {
                            boolean first = true;
                            for (ScriptQuery sq : query) {
                                if (!first)
                                    sb.append(" ").append(query.joiner).append(" ");

                                first = false;
                                if (sq.not)
                                    sb.append("NOT ");

                                sb.append("(");
                                processSingleLine(sq);
                                sb.append(")");
                            }
                            //    sb.append("\n");
                        }
                    }

                    void process(ScriptQuery query, int level) {
                        if (query.query.isEmpty() && query.isEmpty())
                            return;

                        sb.append(query.query);
                        if (!query.isEmpty()) {
                            //  sb.append("\n");

                            boolean first = true;
                            for (ScriptQuery sq : query) {
                                if (!first) {
                                    if (level < 2) {
                                        sb.append("\n");
                                        space(level);
                                    } else
                                        sb.append(" ");

                                    sb.append(query.joiner).append(" ");
                                }

                                if (sq.not)
                                    sb.append("NOT ");

                                if (sq.not
                                        || (query.size() > 1
                                        && level < 2
                                        && sq.getChildQueries().size() > 1))
                                    sb.append("(");

                                if (level < 1) {
                                    sb.append("\n");
                                    space(level + 1);
                                }
                                process(sq, level + 1);

                                first = false;
                                if (level < 1) {
                                    sb.append("\n");
                                    space(level);
                                }
                                if (sq.not
                                        || (query.size() > 1
                                        && level < 2
                                        && sq.getChildQueries().size() > 1))
                                    sb.append(")");
                            }
                            //    sb.append("\n");

                        }

                    }
                }.process(main, 0);


                /*
                 return error != null ? "Błąd: " + error
                 : name + (not ? " Not" : "")
                 + (equals ? " Equals" : " Like")
                 + (moreOrLess == null ? caseSensitive ? " CaseSensitive" : " IgnoreCase" : "")
                 + (moreOrLess != null ? moreOrLess ? " MoreThan" : " LessThan" : "")
                 + " " + values;
                 */
                return sb.toString();
            }

        }

        public String getSQL(ISqlEscapeCallback callback, String space, String intent) {
            return new SqlBuilder(callback).toString(space, intent);
        }
    }

    public static class CharReader {

        private final StringReader sr;

        public CharReader(String text) {
            sr = new StringReader(text);
        }

        public Character read() {
            try {
                int i = sr.read();
                if (i < 0)
                    return null;
                return (char) i;
            } catch (IOException ex) {
                return null;
            }
        }

    }

    public ScriptItem add(String text) {
        ScriptItem item = new ScriptItem(text);
        if (scripts.addCallback != null && !scripts.addCallback.canAdd(item))
            return item;
        this.items.add(item);
        return item;
    }

    public enum ItemOperator {

        equals,
        contains,
        moreThan,
        lessThan,
        moreThanOrEquals,
        lessTanOrEquals
    }

    public class ScriptItem {

        public String error;
        public boolean caseSensitive;
        public boolean not;
        public ItemOperator operator = null;
        public String name;
        public boolean textColumn = true;
        public final List<String> values = new LinkedList<>();
        private ItemStage stage = ItemStage.name;

        private ScriptItem(String line) {
            CharReader reader = new CharReader(line);
            StringBuilder sb = new StringBuilder();

            while (true) {
                Character c = reader.read();
                if (c == null) {
                    if (stage == ItemStage.values)
                        if (!sb.toString().trim().isEmpty())
                            values.add(sb.toString().trim());
                    break;
                }
                if (c < 32)
                    continue;
                // escapowanie
                if (c == '\\') {
                    unescape(c, reader, sb);
                    continue;
                }

                if (stage == ItemStage.name && isOperator(c)) {
                    if (!sb.toString().trim().isEmpty())
                        name = sb.toString().trim();
                    sb = new StringBuilder();
                    stage = ItemStage.operator;
                }

                if (stage == ItemStage.operator && c == ' ')
                    continue;

                if (stage == ItemStage.operator && isOperator(c)) {
                    switch (c) {
                        case '=':
                            if (operator != null && operator == equals)
                                caseSensitive = true;

                            if (operator == null)
                                operator = equals;
                            else if (operator == moreThan)
                                operator = moreThanOrEquals;
                            else if (operator == lessThan)
                                operator = lessTanOrEquals;

                            break;
                        case '~':
                            if (operator != null && operator == contains)
                                caseSensitive = true;
                            operator = contains;
                            break;
                        case '!':
                            not = !not;
                            break;

                        case '>':
                            operator = (operator == equals) ? moreThanOrEquals : moreThan;
                            break;
                        case '<':
                            operator = (operator == equals) ? lessTanOrEquals : lessThan;
                            break;
                    }
                    continue;
                }

                if (stage == ItemStage.operator && !isOperator(c)) {
                    stage = ItemStage.values;
                    sb = new StringBuilder();
                }

                if (stage == ItemStage.values && c == '|') {
                    if (!sb.toString().trim().isEmpty())
                        values.add(sb.toString().trim());
                    sb = new StringBuilder();
                    continue;
                }

                sb.append(c);
            }

            if (operator == null)
                error = LUtil.LACK_OF_OPERATOR.toString();
            if (values.isEmpty())
                error = LUtil.LACK_OF_VALUE.toString();
            if (name == null)
                error = LUtil.LACK_OF_NAME.toString();

            if (operator == moreThan && not)
                operator = lessThan;

            if (operator == lessThan && not)
                operator = moreThan;

            if (operator == moreThan || operator == lessThan) {
                caseSensitive = false;
                not = false;
                textColumn = false;
            }

        }

        @Override
        public String toString() {
            return error != null ? "Błąd: " + error
                    : name + (not ? " Not" : "")
                    + (operator == equals ? " Equals" : "")
                    + (operator == contains ? " Contains" : "")
                    + (textColumn ? caseSensitive ? " CaseSensitive" : " IgnoreCase" : "")
                    + (!textColumn && operator == moreThan ? " MoreThan" : "")
                    + (!textColumn && operator == lessThan ? " LessThan" : "")
                    + " " + values;
        }

        private void unescape(char c, CharReader sr, StringBuilder sb) {
            Character v = sr.read();
            if (v == null)
                return;

            switch (v) {
                case 'r':
                    sb.append('\r');
                    break;
                case 'n':
                    sb.append('\n');
                    break;
                case 't':
                    sb.append('\t');
                    break;
                case 'f':
                    sb.append('\f');
                    break;
                case 'b':
                    sb.append('\b');
                    break;
                case '\\':
                    sb.append('\\');
                    break;
                default:
                    if (isOperator(v))
                        sb.append(v);
                    else
                        error = LUtil.INVALID_ESCAPING.toString(v);

            }
        }

        private boolean isOperator(char c) {
            if (stage == ItemStage.values && c == '|')
                return true;

            char[] soperators = {'=', '!', '~', '<', '>'};
            if (stage == ItemStage.name || stage == ItemStage.operator)
                for (char w : soperators)
                    if (w == c)
                        return true;

            return false;
        }
    }

    @Override
    public Iterator<ScriptItem> iterator() {
        return items.iterator();
    }

    private enum ItemStage {

        name,
        operator,
        values
    }

}
