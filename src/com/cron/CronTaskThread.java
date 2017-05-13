package com.cron;

import com.utils.Utils;
import com.utils.Is;
import com.events.ServiceEvent;
import com.lang.LCron;
import com.mlogger.Log;
import com.mlogger.LogDefaults;
import com.utils.date.TDate;
import com.utils.date.Timestamp;
import java.lang.reflect.Method;

public class CronTaskThread<Result> extends Thread implements Runnable {

    public final CronSchedule[] schedules;
    public final CronTask task;
    public final ServiceEvent event;
    public final Method subtask;

    static CronTaskThread start(boolean async, CronTask task, Method subtask, CronSchedule[] schedules) throws Exception {
        if (schedules == null)
            schedules = new CronSchedule[0];

        boolean canRun = task.onBeforeExecute(schedules);

        CronTaskThread thr = new CronTaskThread(task, subtask, schedules);
        if (!canRun)
            synchronized (task.queueThreads) {
                if (CCron.logEvents())
                    Log.event("Cron", "Dodaję zadanie " + task.name + " do kolejki");

                task.queueThreads.add(thr);
                return thr;
            }

        if (async)
            thr.start();
        else
            thr.runSync();

        return thr;
    }

    private CronTaskThread(CronTask task, Method subtask, CronSchedule[] schedules) {
        super();
        this.schedules = schedules;
        this.task = task;
        this.subtask = subtask;
        setName("Cron Task: " + task.id);
        LogDefaults defs = Log.currentThreadDefaults();
        defs.tag("Cron");
        event = new ServiceEvent("Cron", task.name)
                .tag("cron")
                .attribute("cron|id", LCron.TASK_ID.toString(), task.id)
                .attribute("cron|subtask", LCron.SUBTASK.toString(), subtask != null ? subtask.getName() : null)
                .attribute("cron|manual", LCron.MANUALLY.toString(), Utils.boolToStr(schedules.length == 0));
    }

    @Override
    public void run() {
        try {
            runSync();
        } catch (Throwable e) {
            // w trybie oddzielnego wątku, błedy obsłużone są w metodzie CronTaskThread.run
            // tutaj wszystko powinno być zignorowane
        }
    }

    public Result runSync() throws Exception {
        synchronized (task.activeThreads) {
            task.activeThreads.add(this);
        }

        Timestamp ts = new Timestamp();

        if (schedules != null)
            for (CronSchedule cs : schedules)
                event.attribute("cron|schedule", LCron.SCHEDULE.toString(), cs.name);

        event.attribute("cron|start", LCron.STARTED.toString(), new TDate().toString(true));

        try {
            return (Result) task.run(this, event, subtask);
        } finally {
            synchronized (task.activeThreads) {
                task.activeThreads.remove(this);
            }
            event.attribute("cron|end", LCron.FINISHED.toString(), new TDate().toString(true));
            event.attribute("cron|duration", LCron.DURATION.toString(), ts.diff().toString());
            event.execute();
        }

    }

}
