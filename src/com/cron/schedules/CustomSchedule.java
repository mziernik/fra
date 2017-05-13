package com.cron.schedules;

import com.utils.collections.Strings;
import com.utils.date.TDate;
import com.cron.CronSchedule;
import com.cron.CronTask;
import com.html.core.tag.form.Form;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.lang.LCron;
import java.util.*;

public class CustomSchedule extends CronSchedule<TDate> {

    public final List<TDate> dates = new LinkedList<>();

    public CustomSchedule(CronTask task, String name, Date... dates) {
        super(task, name);
        if (dates != null)
            for (Date d : dates)
                this.dates.add(new TDate(d));
    }

    @Override
    public boolean canExecute() {
        if (!super.canExecute())
            return false;

        TDate now = new TDate().clearMilliseconds();

        for (TDate date : dates)
            if (date.clearMilliseconds().equals(now))
                //  Log.debug("CRON", "pass: " + time + " = " + ct);
                return true;

        return false;
    }

    @Override
    public void buildInfoTable(Table tbl) {
        Strings list = new Strings();
        for (TDate d : dates)
            list.add(d.toString(false));

        tbl.tbodyTr().setCells(LCron.DATES.toString(), list.toString(", "));
    }

    @Override
    public String getTypeName() {
        return LCron.SINGLE.toString();
    }

    @Override
    public void buildEditSection(Form form) {

    }

    @Override
    public void buildEditRow(Tr tr, TDate value) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

}
