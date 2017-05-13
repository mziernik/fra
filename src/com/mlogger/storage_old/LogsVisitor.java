package com.mlogger.storage_old;

import com.mlogger.Log;
import com.mlogger.storage_old.LogsFile;
import com.mlogger.storage_old.LogsFileMeta;

public interface LogsVisitor {

    public boolean visit(final LogsFileMeta logs);

    public boolean onRead(final LogsFileMeta logs, final Log log);
}
