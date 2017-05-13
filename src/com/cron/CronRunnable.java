package com.cron;

import com.events.ServiceEvent;

public abstract class CronRunnable<Result> {

    public final CronTask task;
    public final CronSchedule[] schedules;
    public final ServiceEvent event;

    public CronRunnable(String id, String name) {
        this.task = new CronTask(id, name, this);
        this.schedules = null;
        this.event = null;
    }

    /**
     * Alternatywny konstruktor. Jeśli taki jest zadeklarowany, w momencie
     * uruchomienia zadania utworzona zostanie nowa instancja klasy
     * rozszerzającej CronRunnable
     *
     * @param task
     */
    public CronRunnable(TaskRunnableInstance runnable) {
        this.task = runnable.task;
        this.schedules = runnable.schedules;
        this.event = runnable.event;
    }

    protected abstract Result runTask() throws Exception;

    protected void onException(Throwable exception) {

    }

    public static <T extends CronTask> T getInstance(Class<? extends CronRunnable> runnable) {
        return CronTask.getInstance(runnable);
    }

}
