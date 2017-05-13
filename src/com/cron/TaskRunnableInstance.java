package com.cron;

import com.events.ServiceEvent;

/**
 * @author Miłosz Ziernik
 * @date 29 lipca 2015
 * @encoding UTF-8
 */
public class TaskRunnableInstance {

    final CronTask task;
    final CronSchedule[] schedules;
    final ServiceEvent event;

    TaskRunnableInstance(CronTask task, CronSchedule[] schedules, ServiceEvent event) {
        this.task = task;
        this.schedules = schedules;
        this.event = event;
    }

}
