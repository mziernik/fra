package com.cron.intfs;

import com.utils.date.time.Interval;

public class CronTimeRange {

    public Interval from;
    public Interval to;

    public CronTimeRange(Interval from, Interval to) {
        this.from = from;
        this.to = to;
    }
}
