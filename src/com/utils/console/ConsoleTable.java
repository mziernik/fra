package com.utils.console;

import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class ConsoleTable {

    public final Map<String, String> entries = new LinkedHashMap<>();

    public void printContent(Appendable sb) throws IOException {
        int keyLen = 0;
        int valLen = 0;
        for (Map.Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "");
            String val = Utils.coalesce(en.getValue(), "");
            if (keyLen < key.length())
                keyLen = key.length();
            if (valLen < val.length())
                valLen = val.length();
        }

        int total = keyLen + valLen;

        for (Map.Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "");
            String val = Utils.coalesce(en.getValue(), "");

            sb.append(key);

            //  .append(key);
            for (int j = 0; j < keyLen - key.length(); j++)
                sb.append(" ");

            sb.append(val);

            sb.append("\n");
        }
    }

    public void printParams(String prefix, String sufix, String title) {

        int keyLen = 0;
        int valLen = 0;
        for (Map.Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "").trim();
            String val = Utils.coalesce(en.getValue(), "").trim();
            if (keyLen < key.length())
                keyLen = key.length();
            if (valLen < val.length())
                valLen = val.length();
        }

        int total = keyLen + valLen + 3 + prefix.length() + sufix.length();

        int lblLen = title.length();

        int span = total / 2 - lblLen / 2 - 1;

        StrWriter sb = new StrWriter();

        for (int i = 0; i < span; i++)
            sb.append("-");

        sb.append(" ").append(title).append(" ");

        for (int i = 0; i < span; i++)
            sb.append("-");

        sb.append("\n");

        for (Map.Entry<String, String> en : entries.entrySet()) {
            String key = Utils.coalesce(en.getKey(), "").trim();
            String val = Utils.coalesce(en.getValue(), "").trim();

            sb.append(prefix).append(key);

            //  .append(key);
            for (int j = 0; j < keyLen - key.length(); j++)
                sb.append(" ");

            sb.append(" : ");
            sb.append(val);

            for (int j = 0; j < valLen - val.length(); j++)
                sb.append(" ");

            sb.append(sufix);
            sb.append("\n");
        }

        for (int i = 0; i < total; i++)
            sb.append("-");

        sb.append("\n");

        new TConsole().append(sb.toString()).flush();
    }
}
