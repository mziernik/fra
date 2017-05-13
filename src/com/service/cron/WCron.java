package com.service.cron;

import com.cron.Cron;
import com.cron.CronTask;
import com.lang.LService;
import com.model.dataset.DataSet;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;

public class WCron implements WebApi {

    @WebApiEndpoint
    public DataSet getAll() {
        DataSet<CronTask, String> result = new DataSet<>("cron", LService.TASK_SCHEDULER);

        result.column(String.class, "id", LService.ID, task -> task.id)
                .hidden(true)
                .primaryKey();

        result.column(String.class, "name", LService.NAME, task -> task.name);

        result.column(Boolean.class, "enabled", LService.ACTIVE, task -> task.enabled)
                .hidden(true)
                .searchable(true);

        result.column(Integer.class, "schedules", LService.SCHEDULER, task -> task.getSchedulesCount());

        result.column(String.class, "active", LService.ACTIVE_2,
                task -> (task.getActiveThreads().size() + " / " + task.maxSimultaneouslyThreads));

        result.column(String.class, "queque", LService.QUEUE,
                task -> (task.getQueueThreads().size() + " / " + task.maxQueueSize));

        result.column(Integer.class, "errors", LService.ERRORS,
                task -> task.getErrorsCount());

        result.fillRows(Cron.getTasks());

        return result;
    }
}
