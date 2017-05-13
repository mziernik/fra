package com.cron.intfs;

import com.cron.CronSchedule;
import com.cron.CronTask;

public interface CronException {

    public void onTaskException(CronTask task, CronSchedule[] schedules, Throwable e);
}
