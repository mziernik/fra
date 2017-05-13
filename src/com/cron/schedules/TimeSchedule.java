package com.cron.schedules;

import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.cron.CronTask;
import com.html.core.tag.form.Form;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.html.js.Eval;
import com.lang.LCron;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.*;

public class TimeSchedule extends CronSimpleSchedule<Interval> {

    public final List<Interval> times = new LinkedList<>();
    // godziny, w ktorych zostanie wykonane zadanie w odniesieniu do harmonogramu dziennego

    public TimeSchedule(CronTask task, String name, Interval... times) {
        super(task, name);

        if (times != null)
            for (Interval time : times)
                this.times.add(time);

    }

    @Override
    public boolean canExecute() {
        if (!super.canExecute())
            return false;

        Interval time = new Interval(0).setTime(new Date());

        for (Interval ct : times)
            if (ct.isSame(time, Unit.SECONDS))
                //  Log.debug("CRON", "pass: " + time + " = " + ct);
                return true;

        return false;
    }

    @Override
    public void buildInfoTable(Table tbl) {
        Strings lst = new Strings();
        for (Interval t : times)
            lst.add(t.format().precision(Unit.HOURS).base(Unit.HOURS).toString(true));

        tbl.tbodyTr().setCells(LCron.HOURS.toString() + ":", lst.toString(", "));
        super.buildInfoTable(tbl);
    }

    @Override
    public void buildEditRow(Tr tr, Interval value) {

        if (value == null)
            value = new Interval(0).setTime(new Date());

        String id = "_" + Utils.randomId(5);
        tr.td().inputNumber()
                .name("hr" + id)
                .value(value.getPart(Unit.HOURS))
                .min(0).max(23)
                .style()
                .width("50px");

        tr.td().inputNumber()
                .name("mn" + id)
                .value(value.getPart(Unit.MINUTES))
                .min(0).max(59)
                .style()
                .width("50px");

        tr.td().inputNumber()
                .name("sc" + id)
                .value(value.getPart(Unit.SECONDS))
                .min(0).max(59)
                .style()
                .width("50px");

        tr.td().inputButton()
                .value("X")
                .onClick(new Eval("removecrow(this)"));
    }

    @Override
    public void buildEditSection(Form form) {

        form.br();
        Table tbl = form.table().id("ctbl");
        tbl.tbodyTr().setCells(LCron.HOUR.toString(), LCron.MINUTE.toString(), LCron.SECOND.toString());

        for (Interval t : times)
            buildEditRow(tbl.tbodyTr(), t);

    }

    public void addSection(int idx) {

    }

    @Override
    public String getTypeName() {
        return LCron.HOURS.toString();
    }

}
