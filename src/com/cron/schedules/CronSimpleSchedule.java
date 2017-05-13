package com.cron.schedules;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.cron.CronSchedule;
import com.cron.CronTask;
import com.cron.intfs.CronDateRange;
import com.html.core.tag.form.Form;
import com.html.core.tag.table.Table;
import com.lang.LCron;
import java.util.*;

public abstract class CronSimpleSchedule<BaseType> extends CronSchedule<BaseType> {

    public int initializeDelay; // opoznienie, po którym mogą zostać wykonane zadania  
    public final Set<Integer> daysOfWeek = new TreeSet<>(); // 1..7
    public final Set<Integer> daysOfMonth = new TreeSet<>(); // 1..31
    public final Set<Integer> daysOfYear = new TreeSet<>(); // 1..365
    public final Set<Integer> months = new TreeSet<>(); // 1..12
    public final Set<Integer> weeksOfMonth = new TreeSet<>(); // 1..5
    public final Set<Integer> weeksOfYear = new TreeSet<>(); // 1..54

    public final List<CronDateRange> activeDateRanges = new LinkedList<>();  // zakresy dat aktywnosci
    public final List<CronDateRange> excludeDateRanges = new LinkedList<>();

    public CronSimpleSchedule(CronTask task, String name) {
        super(task, name);
    }

    @Override
    public boolean canExecute() {
        if (!super.canExecute())
            return false;

        TDate now = new TDate();

        for (CronDateRange cdr : excludeDateRanges)
            if (now.between(cdr.from, cdr.to))
                return true;

        if (!daysOfWeek.isEmpty())
            if (!daysOfWeek.contains(now.getCalendar().get(Calendar.DAY_OF_WEEK) - 1))
                return false;

        if (!daysOfMonth.isEmpty())
            if (!daysOfMonth.contains(now.getCalendar().get(Calendar.DAY_OF_MONTH) + 1))
                return false;

        if (!daysOfYear.isEmpty())
            if (!daysOfYear.contains(now.getCalendar().get(Calendar.DAY_OF_YEAR) + 1))
                return false;

        if (!months.isEmpty())
            if (!months.contains(now.getCalendar().get(Calendar.MONTH) + 1))
                return false;

        if (!weeksOfMonth.isEmpty())
            if (!weeksOfMonth.contains(now.getCalendar().get(Calendar.WEEK_OF_MONTH) + 1))
                return false;

        if (!weeksOfYear.isEmpty())
            if (!weeksOfYear.contains(now.getCalendar().get(Calendar.WEEK_OF_YEAR) + 1))
                return false;

        for (CronDateRange cdr : activeDateRanges)
            if (now.between(cdr.from, cdr.to))
                return true;

        if (activeDateRanges.isEmpty())
            return true;

        return false;
    }

    public BaseType setDaysOfWeek(int... days) {
        if (days != null) {
            daysOfWeek.clear();
            daysOfWeek.addAll(Utils.asIntList(days));
        }
        return (BaseType) this;
    }

    @Override
    public void buildEditSection(Form form) {

    }

    @Override
    public void buildInfoTable(Table tbl) {

        if (!activeDateRanges.isEmpty())
            tbl.tbodyTr().setCells(LCron.ACTIVE_BETWEEN.toString(),
                    new Strings().addAll(activeDateRanges).toString(", "));

        if (!excludeDateRanges.isEmpty())
            tbl.tbodyTr().setCells(LCron.LOCKED_BETWEEN.toString(),
                    new Strings().addAll(excludeDateRanges).toString(", "));

        if (!daysOfWeek.isEmpty())
            tbl.tbodyTr().setCells(LCron.DAYS_OF_WEEK.toString(),
                    new Strings().addAll(daysOfWeek).toString(", "));

        if (!daysOfMonth.isEmpty())
            tbl.tbodyTr().setCells(LCron.DAYS_OF_MONTH.toString(),
                    new Strings().addAll(daysOfMonth).toString(", "));

        if (!daysOfYear.isEmpty())
            tbl.tbodyTr().setCells(LCron.DAYS_OF_YEAR.toString(),
                    new Strings().addAll(daysOfYear).toString(", "));

        if (!weeksOfMonth.isEmpty())
            tbl.tbodyTr().setCells(LCron.WEEKS_OF_MONTH.toString(),
                    new Strings().addAll(weeksOfMonth).toString(", "));

        if (!weeksOfYear.isEmpty())
            tbl.tbodyTr().setCells(LCron.WEEKS_OF_YEAR.toString(),
                    new Strings().addAll(weeksOfYear).toString(", "));

        if (!months.isEmpty())
            tbl.tbodyTr().setCells(LCron.MONTHS.toString(),
                    new Strings().addAll(months).toString(", "));
    }
}
