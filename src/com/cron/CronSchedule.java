package com.cron;

import com.html.core.tag.form.Form;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.mlogger.Log;

/**
 * Miłosz Ziernik 2014/01/20
 */
public abstract class CronSchedule<BaseType> {

    public boolean enabled = true;
    public String comment;
    public final String name;
    protected final CronTask task;
    //-----------
    int executeCount;
    int errorsCount;

    int maxErrorsCount = 0;
    int maxExecutesCount = 0;
    boolean removeIfMaxExecutesExceeted = false;

    public abstract String getTypeName();

    public abstract void buildEditRow(Tr tr, BaseType value);

    public abstract void buildInfoTable(Table tbl);

    public abstract void buildEditSection(Form form);

    protected void onExecute(CronTaskThread thread) {
        ++executeCount;
        if (maxExecutesCount > 0 && executeCount >= maxExecutesCount) {
            Log.event("Cron", task.name + " -> " + name
                    + ": Przekroczono maksymalną dopuszczaną ilość wywołąń.\n"
                    + (removeIfMaxExecutesExceeted
                            ? "Uswuwam" : "Wyłączam") + " harmonogram");
            enabled = false;
            if (removeIfMaxExecutesExceeted)
                remove();
        }
    }

    protected void onError(CronTaskThread thread, Throwable e) {
        ++errorsCount;
        if (maxErrorsCount > 0 && errorsCount >= maxErrorsCount) {
            Log.event("Cron", task.name + " -> " + name
                    + ": Przekroczono maksymalną dopuszczaną ilość błędów.\n"
                    + "Wyłączam harmonogram");
            enabled = false;
        }
    }

    public CronSchedule(CronTask task, String name) {
        this.task = task;
        this.name = name;
        synchronized (task.schedules) {
            task.schedules.add(this);
        }
    }

    public boolean canExecute() {
        if (!enabled)
            return false;
        return true;
    }

    public void remove() {
        synchronized (task.schedules) {
            task.schedules.remove(this);
        }
    }

    public CronSchedule setMaxExecutesCount(int maxExecutesCount, boolean removeIfExceeted) {
        this.maxExecutesCount = maxExecutesCount;
        this.removeIfMaxExecutesExceeted = removeIfExceeted;
        return this;
    }

    public CronSchedule setMaxErrorsCount(int maxErrorsCount) {
        this.maxErrorsCount = maxErrorsCount;
        return this;
    }

}
