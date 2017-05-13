package com.cron;

import com.utils.Utils;
import com.utils.Is;
import com.utils.reflections.Reflections;
import com.servlet.controller.Page;
import com.exceptions.http.Http403ForbiddenException;
import com.utils.date.Timestamp;
import com.utils.date.TDate;
import com.mlogger.Log;
import com.mlogger.LogKind;
import com.context.AppContext;
import com.cron.intfs.CronException;
import com.cron.schedules.*;
import com.events.ServiceEvent;
import com.events.ServiceEvent.EventType;
import com.exceptions.EError;
import com.lang.LCron;
import com.servlet.handlers.*;
import com.user.right.UserRight;
import com.utils.date.time.Interval;
import com.utils.reflections.TClass;
import java.lang.reflect.*;
import java.util.*;
import java.util.Map.Entry;

/**
 * Miłosz Ziernik 2014/01/20
 */
public final class CronTask<Result> implements Iterable<CronSchedule>, Comparable<CronTask> {

    public boolean enabled = true;
    public String name;
    public final String id;
    public boolean canExecuteByUser = true;
    public boolean canModifyByUser = true;
    final LinkedList<TDate> executes = new LinkedList<>();
    final LinkedList<Throwable> errors = new LinkedList<>();
    final CronRunnable runnable;
    final Constructor<? extends CronRunnable> constructor;

    public boolean isPublic = true;
    public int maxSimultaneouslyThreads = 1;
    public int maxQueueSize = 0; // jeśli przekroczona zostanie maksymalna wliczba jednoczesnych wątkow,
    // to zadanie trafi do kolejki (o ile to możliwe). Jeśli liczba

    public final HashMap<String, Object> data = new HashMap<>();
    public CronException onException;

    final List<CronSchedule> schedules = new LinkedList<>();
    public int retryCount = 0; // ilosc ponowien w przypadku wystapienia bledu
    public int retryDelay = 0; // opoznienie kolejnych prob ponowienia
    public final Set<UserRight> rights = new LinkedHashSet<>(); // list rol, ktore mogą zarządzać zadaniem
    //-----------
    int executeCount;
    int errorsCount;

    int maxErrorsCount = 0;
    int maxExecutesCount = 0;
    boolean removeIfMaxExecutesExceeted = false;

    final LinkedList<CronTaskThread<?>> activeThreads = new LinkedList<>();
    final LinkedList<CronTaskThread<?>> queueThreads = new LinkedList<>();

    private final Map<Method, Subtask> subtasks = new LinkedHashMap<>();

    public static <T extends CronTask> T getInstance(Class<? extends CronRunnable> runnable) {
        for (CronTask ct : Cron.tasks)
            if (ct.runnable != null && ct.runnable.getClass() == runnable)
                return (T) ct;

        for (CronTask ct : Cron.tasks)
            if (new TClass(ct.getClass()).instanceOf(runnable))
                return (T) ct;

        return null;
    }

    public static CronTask getInstance(String id) {
        if (id != null)
            for (CronTask t : Cron.getTasks())
                if (t.id.equals(id))
                    return t;
        return null;
    }

    CronTask(String id, String name, CronRunnable runnable) {
        this.name = name;
        this.runnable = runnable;

        Utils.checkId(id, true, ".-");
        this.id = id;

        Class<? extends CronRunnable> rcls = runnable.getClass();

        try {
            constructor = rcls.getConstructor(TaskRunnableInstance.class);
        } catch (Exception e) {
            throw new UnsupportedOperationException(LCron.CRONTASK_LACK_OF_CTOR.toString(rcls.getName()));
        }
        for (Method method : rcls.getDeclaredMethods()) {
            Subtask ann = method.getAnnotation(Subtask.class);
            if (ann == null)
                continue;

            if (method.getParameterCount() > 0)
                throw new UnsupportedOperationException(
                        LCron.CRONTASK_METHOD_CANT_ACCPET_PARAMS.toString(rcls.getName(), method.getName()));

            subtasks.put(method, ann);
        }

        if (!subtasks.isEmpty())
            Utils.sortMap(subtasks, new Comparator<Entry<Method, Subtask>>() {

                @Override
                public int compare(Entry<Method, Subtask> o1, Entry<Method, Subtask> o2) {

                    return o1 != null
                            && o2 != null
                            && o1.getValue() != null
                            && o2.getValue() != null
                            ? Integer.compare(o1.getValue().order(),
                                    o2.getValue().order())
                            : 0;

                }
            });

        /*
         if (CCron.logEvents() && BaseContext.initialized)
         Log.debug("CRON", "Nowe zadanie: " + name);
         */
        synchronized (Cron.tasks) {
            Cron.tasks.add(this);
        }
    }

    CronSchedule[] getExecuteSchedules() {

        List<CronSchedule> lst = new LinkedList<>();

        for (CronSchedule schedule : schedules)
            if (schedule.canExecute())
                lst.add(schedule);

        CronSchedule[] arr = new CronSchedule[lst.size()];
        lst.toArray(arr);
        return arr;
    }

    public boolean canExecute() {
        if (!enabled)
            return false;

        return true;
    }

    public CronTask setMaxExecutesCount(int maxExecutesCount, boolean removeIfExceeted) {
        this.maxExecutesCount = maxExecutesCount;
        this.removeIfMaxExecutesExceeted = removeIfExceeted;
        return this;
    }

    public CronTask setMaxErrorsCount(int maxErrorsCount) {
        this.maxErrorsCount = maxErrorsCount;
        return this;
    }

    public LinkedList<TDate> getExecutes() {
        LinkedList<TDate> dates = new LinkedList<>();
        dates.addAll(executes);
        return dates;
    }

    public LinkedList<CronTaskThread> getActiveThreads() {
        LinkedList<CronTaskThread> list = new LinkedList<>();
        synchronized (activeThreads) {
            list.addAll(activeThreads);
        }
        return list;
    }

    public LinkedList<CronTaskThread> getQueueThreads() {
        LinkedList<CronTaskThread> list = new LinkedList<>();
        synchronized (queueThreads) {
            list.addAll(queueThreads);
        }
        return list;
    }

    public boolean isRunning() {
        synchronized (activeThreads) {
            return !activeThreads.isEmpty();
        }
    }

    public LinkedList<Throwable> getErrors() {
        LinkedList<Throwable> list = new LinkedList<>();
        list.addAll(errors);
        return list;
    }

    public int getErrorsCount() {
        return errorsCount;
    }

    public TimeSchedule addTimeSchedule(String name, Interval... times) {
        return new TimeSchedule(this, name, times);
    }

    public CountDownSchedule addCountDownSchedule(String name) {
        return new CountDownSchedule(this, name);
    }

    public CustomSchedule addCustomSchedule(String name, Date... dates) {
        return new CustomSchedule(this, name, dates);
    }

    public IntervalSchedule addIntervalSchedule(String name, int intervalSeconds) {
        return new IntervalSchedule(this, name, intervalSeconds);
    }

    public void remove() {
        synchronized (Cron.instance.tasks) {
            Cron.instance.tasks.remove(this);
        }
    }

    public int getSchedulesCount() {
        return schedules.size();
    }

    public int getExecutesCount() {
        return executeCount;
    }

    @Override
    public Iterator<CronSchedule> iterator() {
        return schedules.iterator();
    }

    public CronTaskThread<Result> runUser(Page page, boolean async) throws Exception {
        return runUser(page, null, async);
    }

    public CronTaskThread<Result> runUser(Page page, Method subtask, boolean async) throws Exception {
        if (!page.user.rights.has(rights))
            throw new Http403ForbiddenException(LCron.CRONTASK_NO_PERMISSION_TO_RUN_TASK.toString());
        return run(subtask, async);
    }

    public CronTaskThread<Result> run(boolean async) throws Exception {
        return run(null, async);
    }

    public CronTaskThread<Result> run(Method subtask, boolean async) throws Exception {
        return CronTaskThread.start(async, this, subtask, new CronSchedule[0]);
    }

    boolean onBeforeExecute(CronSchedule[] schedules) {

        LinkedList<CronTaskThread> thrs = getActiveThreads();
        if (thrs.size() >= maxSimultaneouslyThreads)
            if (queueThreads.size() < maxQueueSize)
                return false;
            else
                throw new Error(LCron.CRONTASK_TOO_MANY_INSTANCES.toString(maxSimultaneouslyThreads));

        return true;
    }

    Result run(CronTaskThread thread, ServiceEvent event, Method subtask) throws Exception {
        if (AppContext.isTerminated())
            return null;

        Timestamp start = new Timestamp();
        try {

            while (executes.size() > 20)
                executes.remove(0);

            if (CCron.logEvents())
                Log.event(LCron.CRONTASK_STARTING_TASK.toString(name));

            if (runnable == null)
                throw new NullPointerException(this.getClass().getName() + ".runnable");

            CronRunnable<Result> current = constructor.newInstance(
                    new TaskRunnableInstance(runnable.task, schedules.toArray(
                            new CronSchedule[schedules.size()]), event));

            if (TestClass.isTestMode())
                event.attribute("cron|task|test_mode",
                        LCron.CRONTASK_TEST_MODE.toString(), null);

            if (subtask != null) {
                subtask.setAccessible(true);
                subtask.invoke(current);
                return null;
            }

            return current.runTask();

        } catch (Throwable e) {
            errors.add(e);
            ++errorsCount;
            while (errors.size() > 20)
                errors.remove(0);

            Log.error(e);

            event.type(e instanceof Error ? EventType.warning : EventType.error);
            event.details("cron|call_stack", LCron.CRONTASK_CALL_STACK.toString(),
                    EError.getStackTraceStr(e).toString("\n"));
            event.attribute("cron|exception", LCron.ERROR.toString(), EError.toString(e, true));
            event.setResult(EError.toString(e, false));

            runnable.onException(e);
            for (CronException ce : Cron.exceptionHandlers)
                ce.onTaskException(CronTask.this, thread.schedules, e);

            if (onException != null)
                onException.onTaskException(CronTask.this, thread.schedules, e);

            if (thread != null)
                for (CronSchedule sch : thread.schedules)
                    sch.onError(thread, e);

            if (maxErrorsCount > 0 && errorsCount >= maxErrorsCount) {
                Log.event("Cron", name
                        + ": " + LCron.CRONTASK_TOO_MANY_ERRORS.toString() + "\n"
                        + LCron.CRONTASK_STOPPING_TASK.toString());
                enabled = false;
            }

            throw e;
        } finally {
            ++executeCount;
            executes.add(new TDate());
            if (CCron.logEvents())
                new Log(LogKind.EVENT)
                        .tag("Cron")
                        .value(LCron.CRONTASK_TASK_DONE.toString(name))
                        .comment(LCron.TIME.toString() + ": " + start.diff())
                        .send();

            if (thread != null && thread.schedules != null)
                for (CronSchedule sch : thread.schedules)
                    sch.onExecute(thread);

            if (maxExecutesCount > 0 && executeCount >= maxExecutesCount) {
                Log.event("Cron", name
                        + ": " + LCron.CRONTASK_TOO_MANY_CALLS.toString() + "\n"
                        + (removeIfMaxExecutesExceeted
                                ? LCron.DELETING.toString() : LCron.TURNING_OFF.toString())
                        + " " + LCron.TASK.toString());
                enabled = false;
                if (removeIfMaxExecutesExceeted)
                    remove();
            }
        }
    }

    @Override
    public int compareTo(CronTask o) {
        return Utils.collator.compare(id, o != null ? o.id : null);
    }

    public Set<Entry<Method, Subtask>> getSubtasks() {
        return subtasks.entrySet();
    }

}
