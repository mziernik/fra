package com.cron.schedules;

import com.cron.CronTask;
import com.cron.intfs.CronTimeRange;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.lang.LCron;
import java.util.*;

public class IntervalSchedule extends CronSimpleSchedule<CronTimeRange> {

    public int interval; // sekundy
    public int delay;
    public boolean intervalIncludeExecuteTime = true; // dolicz czas wykonania zadania do czasu interwalu
    public final List<CronTimeRange> activeTimeRanges = new LinkedList<>();  // zakresy godzin aktywnosci

    private final long created = System.currentTimeMillis();
    private long lastExec = System.currentTimeMillis();

    public IntervalSchedule(CronTask task, String name, int intervalSeconds) {
        super(task, name);
        this.interval = intervalSeconds;
    }

    @Override
    public boolean canExecute() {
        if (!super.canExecute())
            return false;

        if (delay > 0 && System.currentTimeMillis() - created < delay * 1000)
            return false;

        if (System.currentTimeMillis() - lastExec < interval * 1000)
            return false;

        lastExec = System.currentTimeMillis();

        //   Log.event("CRON", "Interval pass");
        return true;
    }

    @Override
    public void buildInfoTable(Table tbl) {
        tbl.tbodyTr().setCells(LCron.INTERVAL.toString(), interval > 0 ? interval + " " + LCron.SECONDS.toString() : "");
        tbl.tbodyTr().setCells(LCron.DELAY.toString(), delay > 0 ? delay + " " + LCron.SECONDS.toString() : "");
        super.buildInfoTable(tbl);

    }

    @Override
    public String getTypeName() {
        return LCron.INTERVAL.toString();
    }

    @Override
    public void buildEditRow(Tr tr, CronTimeRange value) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
