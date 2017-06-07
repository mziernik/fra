package com.service.cron;

import com.cron.Cron;
import com.cron.CronTask;
import com.lang.LService;
import com.model.dataset.DataSet;
import com.utils.reflections.DataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;

public class WCron implements WebApi {

    @WebApiEndpoint
    public DataSet getAll() {
        DataSet<CronTask, String> result = new DataSet<>("cron", LService.TASK_SCHEDULER);

        result.column(String.class, "id", DataType.KEY, LService.ID, task -> task.id)
                .hidden(true)
                .primaryKey();

        result.column(String.class, "name", DataType.STRING, LService.NAME, task -> task.name);

        result.column(Boolean.class, "enabled", DataType.BOOLEAN, LService.ACTIVE, task -> task.enabled)
                .hidden(true)
                .searchable(true);

        result.column(Integer.class, "schedules", DataType.INT, LService.SCHEDULER, task -> task.getSchedulesCount());

        result.column(String.class, "active", DataType.STRING, LService.ACTIVE_2,
                task -> (task.getActiveThreads().size() + " / " + task.maxSimultaneouslyThreads));

        result.column(String.class, "queque", DataType.STRING, LService.QUEUE,
                task -> (task.getQueueThreads().size() + " / " + task.maxQueueSize));

        result.column(Integer.class, "errors", DataType.INT, LService.ERRORS,
                task -> task.getErrorsCount());

        result.fillRows(Cron.getTasks());

        return result;
    }
}
