package com.mlogger.handlers;

import com.mlogger.*;
import com.utils.Is;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
import java.io.OutputStreamWriter;
import java.util.LinkedList;
import java.util.logging.Handler;
import java.util.logging.LogRecord;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import java.io.*;
import java.util.List;

/**
 * Miłosz Ziernik 2014/06/13
 */
public class TextFileHandler extends LogHandler {

    public File file;
    private String fileName;
    public File path;
    private BufferedWriter writer;

    public TextFileHandler(File path) throws FileNotFoundException {
        super();
        this.path = path;
    }

    @Override
    public void publish(LogElement le, LinkedList<Handler> handlers, LogRecord record) throws Exception {
        if (!(le instanceof Log))
            return;

        String fname = new TDate().toString("yy-MM-dd") + ".txt";

        if (fileName == null
                || !fileName.equals(fname)
                || file == null
                || writer == null) {

            fileName = fname;
            file = new File(path, fileName);
            file.getParentFile().mkdirs();
            writer = new BufferedWriter(new OutputStreamWriter(
                    new FileOutputStream(file, true), Utils.UTF8));
        }

        Log log = (Log) le;

        writer.append("\n")
                .append(log.date.value().toString(true))
                .append(" ")
                .append(log.kind.value().name())
                .append(log.tag.isEmpty() ? "" : " [" + new Strings().add(log.tag).toString(", ") + "] ")
                .append(log.value.isEmpty() ? "" : Utils.toString(log.value.value().value));

        if (Is.in(log.kind.value(), LogKind.ERROR, LogKind.WARNING, LogKind.EXCEPTION)) {

            for (LogElement.DataObj d : log.data.value())
                writer.append("\n")
                        .append("\t")
                        .append(d.name)
                        .append(": ")
                        .append(Utils.toString(d.value));

            for (List<String> list : log.errorStack.value()) {
                writer.append("\n");
                for (String s : list)
                    writer.append("\t").append(s).append("\n");
            }
        }

        writer.append("\n");

    }

    @Override
    public String toString() {
        return "file://" + file.getAbsolutePath();
    }

}
