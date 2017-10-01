package com.service.status;

import com.context.intf.ContextInitStage;
import com.context.intf.ContextInitialized;
import com.json.JArray;
import com.json.JObject;
import com.lang.core.LStr;
import com.model.RRepoState;

import com.model.repository.Record;
import com.model.repository.ReposTransaction;
import com.sun.management.OperatingSystemMXBean;
import com.thread.LoopThread;
import com.utils.Utils;
import static com.utils.Utils.formatSize;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.reflections.datatype.DataType;
import com.webapi.WNotifications;
import java.lang.management.*;

public class ServiceMonitor extends LoopThread {

    private static RuntimeMXBean mxBean;
    private static OperatingSystemMXBean osBean;
    private static com.sun.management.ThreadMXBean thBean;

    //-------------------------------------
    public static String warningColor = "yellow";
    public static String errorColor = "red";
    public static Double warningLevel = 0.75d;
    public static Double errorLevel = 0.9d;

    //------------------------------------------------------------------------
    public final static StatusGroup SYSTEM = new StatusGroup("system", "System", "System");

    public final static StatusItem<Long> UPTIME = SYSTEM.item(DataType.DURATION, "uptime", "Czas działania");
    public final static StatusItem<String> CPU = SYSTEM.item(DataType.STRING, "cpu", "CPU", "Użycie procesora");

    public final static StatusGroup MEMORY = SYSTEM.group("memory", "Pamięć");
    private final static StatusItem<String> MEM_APP = MEMORY.item(DataType.STRING, "app", "Application");
    private final static StatusItem<String> MEM_SYS = MEMORY.item(DataType.STRING, "sys", "System");

    public final static StatusGroup THREADS = SYSTEM.group("thread", "Pamięć");
    private final static StatusItem<Integer> THR_ACT = THREADS.item(DataType.INT, "active", "Aktywne");
    private final static StatusItem<Integer> THR_RUNNABLE = THREADS.item(DataType.INT, "runnable", "Działające");
    private final static StatusItem<Integer> THR_BLOCKED = THREADS.item(DataType.INT, "blocked", "Zablokowane");
    private final static StatusItem<Integer> THR_WAITING = THREADS.item(DataType.INT, "waiting", "Oczekujące");
    private final static StatusItem<Integer> THR_NEW = THREADS.item(DataType.INT, "new", "Nowe");
    private final static StatusItem<Integer> THR_TERMINATED = THREADS.item(DataType.INT, "terminated", "Zatrzymane");

    private static String color(long current, long max) {

        if (max == 0)
            return null;

        double d = (double) current / (double) max;

        if (errorColor != null && errorLevel != null && errorLevel > 0 && d > errorLevel)
            return errorColor;

        if (warningColor != null && warningLevel != null && warningLevel > 0 && d > warningLevel)
            return warningColor;

        return null;
    }

    private ServiceMonitor() {
        super("ServiceMonitor", 1000);
        setPriority(Thread.MIN_PRIORITY);
    }

    @ContextInitialized(stage = ContextInitStage.afterConfigLoaded,
            ifAvailable = "javax.websocket.CloseReason")
    static void init() {
        new ServiceMonitor().start();
    }

    @Override
    protected boolean onBeforeRun() {

        mxBean = ManagementFactory.getRuntimeMXBean();
        //private final static java.lang.management.ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
        osBean = ManagementFactory
                .getPlatformMXBean(OperatingSystemMXBean.class);

        thBean = (com.sun.management.ThreadMXBean) ManagementFactory.getThreadMXBean();
        return true;
    }

    //ToDo: Wykonywać pętlę tylko jesli konsola deweloperska jest włączona lub kotoś monitoruje stan przez WebApi
    @Override
    protected void loop() throws Exception {

        if (RThreads.instance == null || !RRepoState.canBroadcast(RThreads.instance))
            return;

        UPTIME.value(mxBean.getUptime());
        CPU.comment("[" + osBean.getAvailableProcessors() + "]");

        CPU.value(Utils.formatPercent(osBean.getProcessCpuLoad()) + " / "
                + Utils.formatPercent(osBean.getSystemCpuLoad()));

//        json.arrayC("cpu").add(osBean.getProcessCpuLoad() * 100d)
//                .add(osBean.getSystemCpuLoad() * 100d);
        {
            long total = osBean.getTotalPhysicalMemorySize();
            long used = total - osBean.getFreePhysicalMemorySize();
            MEM_SYS.value(formatSize(used) + " / " + formatSize(total))
                    .comment("free: " + formatSize(total - used))
                    .color(color(used, total));
        }
        {

            long total = Runtime.getRuntime().totalMemory();
            long free = Runtime.getRuntime().freeMemory();
            long max = Runtime.getRuntime().maxMemory();

            if (max <= 0 || max == Long.MAX_VALUE)
                max = total;

            long used = total - free;

            MEM_APP.value(formatSize(used) + " / " + formatSize(total) + " / " + formatSize(max))
                    .comment("free: " + formatSize(free))
                    .color(color(used, total));

        }

        /*
        out.arrayC("perm")
        .item(perm.getUsed())
        .item(perm.getCommitted())
        .item(perm.getMax())
        .item(perm.getInit()); */
 /*        int mod = (15 * 1000 / delay);
        if (StatusResource.hdd.enabled && counter % mod
        == 0) {
        StatusItem sHdd = status.status("$h", "HDD");
        for (File f : File.listRoots())
        if (f.getTotalSpace() > 0) {
        String path = f.getPath();
        if (path == null)
        path = "";
        if (path.endsWith(":\\"))
        path = path.substring(0, path.length() - 2);
        long used = f.getTotalSpace() - f.getFreeSpace();
        long total = f.getTotalSpace();
        sHdd.item(path, path, formatSize(used) + " / " + formatSize(total))
        .expire(30)
        .comment("free: " + formatSize(f.getUsableSpace()))
        .color(color(used, total));
        }
        }
         */
        ThreadGroup threadGroup = Thread.currentThread().getThreadGroup();

        while (threadGroup.getParent() != null)
            threadGroup = threadGroup.getParent();

        Thread[] threads = new Thread[threadGroup.activeCount()];
        threadGroup.enumerate(threads, true);

        int tnew = 0;
        int trun = 0;
        int tblc = 0;
        int tterm = 0;
        int twait = 0;

        ReposTransaction trans = new ReposTransaction();

        for (Thread th : threads) {
            if (th == null || th.getState() == null) // tu może wystąpić null
                continue;

            ThreadGroup gr = th.getThreadGroup();
            ThreadInfo info = thBean.getThreadInfo(th.getId());
//
//            jth.array().addAll(
//                    th.getId(),
//                    th.getPriority(),
//                    th.getState().name().toLowerCase(),
//                    gr != null ? gr.getName() : null,
//                    th.isAlive(),
//                    th.isDaemon(),
//                    th.isInterrupted(),
//                    th.getName(),
//                    thBean.getThreadCpuTime(th.getId()) / 100000,
//                    thBean.getThreadUserTime(th.getId()) / 100000,
//                    thBean.getThreadAllocatedBytes(th.getId()),
//                    info != null ? info.getBlockedCount() : 0,
//                    info != null ? info.getWaitedCount() : 0
//            );

            switch (th.getState()) {
                case NEW:
                    ++tnew;
                    break;
                case RUNNABLE:
                    ++trun;
                    break;
                case BLOCKED:
                    ++tblc;
                    break;
                case TERMINATED:
                    ++tterm;
                    break;
                case WAITING:
                case TIMED_WAITING:
                    ++twait;
                    break;
            }

            Record rec = trans.createOrUpdate(RThreads.instance, th.getId());
            rec.set(RThreads.ID, th.getId());
            rec.set(RThreads.PRIORITY, th.getPriority());
            rec.set(RThreads.STATE, th.getState().name().toLowerCase());
            rec.set(RThreads.NAME, th.getName());
            rec.set(RThreads.GROUP, gr != null ? gr.getName() : null);
            rec.set(RThreads.ALIVE, th.isAlive());
            rec.set(RThreads.DAEMON, th.isDaemon());
            rec.set(RThreads.INTERRUPTED, th.isInterrupted());
            rec.set(RThreads.CPU_TIME, thBean.getThreadCpuTime(th.getId()) / 100000);
            rec.set(RThreads.USER_TIME, thBean.getThreadUserTime(th.getId()) / 100000);
            rec.set(RThreads.ALLOCATED, thBean.getThreadAllocatedBytes(th.getId()));
            rec.set(RThreads.BLOCKED, info != null ? (int) info.getBlockedCount() : 0);
            rec.set(RThreads.WAITED, info != null ? (int) info.getWaitedCount() : 0);
        }

        trans.commit(true);

        int act = Thread.activeCount();

        THR_WAITING.value(twait);
        THR_TERMINATED.value(tterm);
        THR_RUNNABLE.value(trun);
        THR_BLOCKED.value(tblc).color(tblc > 0 ? warningColor : null);
        THR_ACT.value(act);
        THR_NEW.value(tnew);

    }

    static {
        WNotifications.notifySources.put("service.status",
                new LStr("Status systemu i usługi"));

        WNotifications.notifySources.put("service.threads",
                new LStr("Wątki"));
    }

}
