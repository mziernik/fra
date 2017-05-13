package com.cron.schedules;

import com.cron.CronTask;
import com.html.core.tag.table.Tr;
import com.lang.LCron;

public class CountDownSchedule extends CronSimpleSchedule<Integer> {

    private Long time;
    private Long left;
    private final Object sync = new Object();

    public void start(long timeLeft) {
        synchronized (sync) {
            stop();
            left = timeLeft;
            time = System.currentTimeMillis();
        }
    }

    public void stop() {
        synchronized (sync) {
            time = null;
            left = null;
        }
    }

    public long timeLeft() {
        synchronized (sync) {
            return left == null ? -1 : left - (System.currentTimeMillis() - time);
        }
    }

    public CountDownSchedule(CronTask task, String name) {
        super(task, name);
    }

    @Override
    public boolean canExecute() {
        if (!super.canExecute())
            return false;

        synchronized (sync) {
            if (time != null && left != null && timeLeft() <= 0) {
                stop();
                return true;
            }
        }
        return false;
    }

    @Override
    public String getTypeName() {
        return LCron.COUNTING_DOWN.toString();
    }

    @Override
    public void buildEditRow(Tr tr, Integer value) {
        throw new UnsupportedOperationException("Not supported yet.");
    }
}
