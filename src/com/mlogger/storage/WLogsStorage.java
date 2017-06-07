package com.mlogger.storage;

import com.lang.core.LStr;
import com.mlogger.LogKind;
import com.model.dataset.DataSet;
import com.utils.TObject;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import com.utils.date.TDate;
import com.utils.reflections.DataType;
import com.utils.reflections.DataType.EnumDataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;
import java.io.IOException;
import java.util.Collection;
import java.util.Date;
import java.util.UUID;

public class WLogsStorage implements WebApi {

    @WebApiEndpoint
    public DataSet getAll() throws IOException {

        int limit = 1000;

        final TObject<LogsFile> lfile = new TObject<>();

        DataSet<SLog, String> dataSet = new DataSet<>("logs_storage", new LStr("Magazyn"));

        dataSet.column(String.class, "file", DataType.FILE_NAME, new LStr("Plik"),
                l -> lfile.get().file.getName());

        dataSet.column(UUID.class, "uid", DataType.UUID, new LStr("UID"),
                l -> lfile.get().fileUid)
                .hidden(true);

        dataSet.column(Long.class, "id", DataType.LONG, new LStr("ID"),
                l -> l.id).hidden(true);

        dataSet.column(TDate.class, "date", DataType.TIMESTAMP, new LStr("Data"),
                log -> log.date.value());

        dataSet.column(LogKind.class, "kind", new EnumDataType<>(LogKind.class), new LStr("Rodzaj"),
                log -> log.kind.value());

        dataSet.column(TList.class, "tags", DataType.LIST, new LStr("Tagi"),
                null /*   log -> log.tag.value()*/);

        dataSet.column(String.class, "value", DataType.STRING, new LStr("Wartość"),
                log -> log.tag.value().toString());

        dataSet.column(Long.class, "len", DataType.SIZE, new LStr("Rozmiar"),
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
