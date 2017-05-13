package com.mlogger.storage;

import com.lang.core.LStr;
import com.model.dataset.DataSet;
import com.utils.TObject;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import java.io.IOException;
import java.util.Date;

public class WLogsStorage implements WebApi {

    @WebApiEndpoint
    public DataSet getAll() throws IOException {

        int limit = 1000;

        final TObject<LogsFile> lfile = new TObject<>();

        DataSet<SLog, String> dataSet = new DataSet<>("logs_storage", new LStr("Magazyn"));

        dataSet.column(String.class, "file", new LStr("Plik"),
                l -> lfile.get().file.getName());

        dataSet.column(String.class, "uid", new LStr("UID"),
                l -> lfile.get().fileUid.toString())
                .hidden(true);

        dataSet.column(Long.class, "id", new LStr("ID"),
                l -> l.id).hidden(true);

        dataSet.column(Date.class, "date", new LStr("Data"),
                log -> log.date.value());

        dataSet.column(String.class, "kind", new LStr("Rodzaj"),
                log -> log.kind.value().name().toLowerCase());

        dataSet.column(String.class, "tags", new LStr("Tagi"),
                log -> log.tag.value().toString());

        dataSet.column(String.class, "value", new LStr("Wartość"),
                log -> log.tag.value().toString());

        dataSet.column(Long.class, "len", new LStr("Rozmiar"),
                log -> log.attrsLength);

//        for (LogsFile file : LogsStorage.files) {
//            lfile.set(file);
//
//            file.readAll((SLog log) -> {
//                if (dataSet.jRows.size() >= limit)
//                    return false;
//                dataSet.fillRow(log);
//                return true;
//            });
//
//            if (dataSet.jRows.size() >= limit)
//                break;
//        }

        return dataSet;
    }
}
