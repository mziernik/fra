package com.service.cron;

import com.cron.Cron;
import com.cron.CronTask;
import com.lang.LService;
import com.model.repository.DynamicRepo;
import com.model.repository.Repository;
import com.utils.reflections.datatype.DataType;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;

public class WCron implements WebApi {

    @WebApiEndpoint
    public Repository getAll() {
        DynamicRepo<CronTask, String> result = new DynamicRepo<>("cron", LService.TASK_SCHEDULER);

        result.column(String.class, "id", DataType.KEY, LService.ID, task -> task.id)
                .config(c -> c.hidden = true);

        result.column(String.class, "name", DataType.STRING, LService.NAME, task -> task.name);

        result.column(Boolean.class, "enabled", DataType.BOOLEAN, LService.ACTIVE, task -> task.enabled)
                .config(c -> c.hidden = true);

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
