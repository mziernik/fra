package com.database;

import com.utils.Utils;
import com.utils.Is;
import com.context.index.Index;
import com.io.IOUtils;
import com.lang.LDatabase;
import com.utils.text.StrWriter;
import java.io.*;
import java.sql.SQLException;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Miłosz Ziernik
 * @date 10 listopada 2015
 * @encoding UTF-8
 */
public class Queries {

    public static class Query {

        public String value;
        public String name;

        public Query(String name, String value) {
            this.value = value;
            this.name = name;
            list.add(this);
        }
    }

    private final static List<Query> list = new LinkedList<>();

    public static String getStrF(String name) throws SQLException {
        for (Query q : list)
            if (q.name.equals(name))
                return q.value;
        throw new SQLException(LDatabase.QUERY_NOT_FOUND.toString(name));

    }

    public static void load() throws IOException {

        for (String s : Index.allFiles) {
            if (!s.toLowerCase().endsWith(".sql"))
                continue;

            try (InputStream in = Queries.class.getResourceAsStream("/" + s)) {
                String[] lines = IOUtils.readUtf(in).split("\n");

                boolean isComment = false;
                boolean isContent = false;
                String name = null;

                StringWriter writer = null;

                for (int i = 0; i < lines.length; i++) {
                    String line = lines[i];

                    if (line.contains("/*"))
                        isComment = true;

                    if (line.contains("*/")) {
                        isComment = false;
                        continue;
                    }

                    if (line.contains("@name")) {
                        if (name != null && writer != null)
                            new Query(name, writer.toString().trim());

                        name = line.substring(line.indexOf("@name")
                                + "@name".length(), line.length()).trim();
                        writer = new StringWriter();
                    }
                    if (!isComment && name != null) {
                        if (writer == null)
                            writer = new StringWriter();
                        writer.append(line).append("\n");
                    }
                }

                if (name != null && writer != null)
                    new Query(name, writer.toString().trim());

            }

        }

    }
}
