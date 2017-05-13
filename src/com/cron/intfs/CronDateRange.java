package com.cron.intfs;

import com.utils.date.TDate;
import java.util.*;

public class CronDateRange {

    public Date from;
    public Date to;

    public CronDateRange(Date from, Date to) {
        this.from = from;
        this.to = to;
    }

    @Override
    public String toString() {
        return new TDate(from).toString(false) + " - " + new TDate(to).toString(false);
    }
}
