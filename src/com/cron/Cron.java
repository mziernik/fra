package com.cron;

import com.thread.TThread;
import com.context.intf.ContextInitStage;
import com.cron.intfs.CronException;
import com.mlogger.Log;
import java.util.*;

/**
 * Miłosz Ziernik 2014/01/20
 */
public final class Cron {

    public final static Cron instance = new Cron();
    final static TreeSet<CronTask> tasks = new TreeSet<>();
    public final static List<CronException> exceptionHandlers = new LinkedList<>();

    static CronExecutor executor;

    public static void initialize() {
        if (executor != null && executor.isRunning())
            return;

        if (executor != null)
            executor.interrupt();

        executor = new CronExecutor();
        executor.start();
    }

    private Cron() {

    }

    private static class CronExecutor extends TThread {

        public CronExecutor() {
            super("Cron");
        }

        @Override
        protected void run() throws Exception {

            synchronized (instance) {
                instance.notifyAll();
            }

            long prevSecondsStamp = 0;
            // zaakceptowane w danej sekundzie
            Set<CronTask> accepted = new HashSet<>();

            while (true)
                try {

                    long totalSeconds = System.currentTimeMillis() / 1000;
                    if (totalSeconds != prevSecondsStamp) {
                        prevSecondsStamp = totalSeconds;
                        accepted.clear();
                    }

                    synchronized (tasks) {
                        for (CronTask<?> task : tasks) {

                            if (accepted.contains(task))
                                continue;

                            LinkedList<CronTaskThread> active = task.getActiveThreads();
                            LinkedList<CronTaskThread> queue = task.getQueueThreads();

                            if (!queue.isEmpty() && active.size() < task.maxSimultaneouslyThreads) {
                                CronTaskThread thr = null;
                                synchronized (task.queueThreads) {
                                    thr = task.queueThreads.pollFirst();
                                }

                                if (thr != null) {
                                    thr.start();
                                    continue;
                                }

                            }

                            if (!task.canExecute())
                                continue;

                            CronSchedule[] schedules = task.getExecuteSchedules();
                            if (schedules == null || schedules.length == 0)
                                continue;

                            accepted.add(task);
                            CronTaskThread.start(true, task, null, schedules);
                        }
                    }

                    Thread.sleep(1000);
                } catch (InterruptedException ie) {
                    return;
                } catch (Throwable e) {
                    Log.error(e);
                }
        }

    }

    public static void stop() {
        if (executor != null)
            executor.interrupt();
    }

    public static List<CronTask> getTasks() {
        List<CronTask> list = new LinkedList<>();
        synchronized (tasks) {
            list.addAll(tasks);
        }
        return list;
    }

    /*
     static {
     CronTask task = Cron.addTask("long_task", "Testowe zadanie", new CronRunnable() {

     @Override
     public void runTask(CronTask task, CronSchedule[] schedules) throws Exception {

     Thread.sleep(100);
     }
     });

     task.maxSimultaneouslyThreads = 1;
     task.maxQueueSize = 100;

     TimeSchedule time = task.addTimeSchedule("5 sekund po inicjalizacji kontekstu");
     time.activeDateRanges.add(new CronDateRange(
     new TDate().addDays(-1), new TDate().addWeeks(1)));

     time.times.add(new TTime(new TDate().addSeconds(5)));

     IntervalSchedule intv = task.addIntervalSchedule("Co 15 sekund");
     intv.delay = 30;

     task.addCustomSchedule("za 20 s", new TDate().addSeconds(20));

     }
     */
}
