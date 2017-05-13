package com.mlogger.status;

import com.context.intf.ContextInitStage;
import com.context.intf.ContextInitialized;
import com.json.JArray;
import com.json.JObject;
import com.lang.core.LStr;
import com.sun.management.OperatingSystemMXBean;
import com.thread.LoopThread;
import com.utils.Utils;
import com.utils.Is;
import static com.utils.Utils.formatSize;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.webapi.WNotifications;
import com.webapi.core.WebApiBroadcast;
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
    //--------------------------------------

    public final static StatusGroup service = StatusGroup.ROOT.group("svr", "Service");
    private static StatusItem cpu;
    private final static StatusItem uptime = service.item("Uptime");
    private final static StatusGroup memory = service.group("mem", "Memory usage");

    private final static StatusItem memApp = memory.item("Application");
    private final static StatusItem memSys = memory.item("System");

    private final static StatusGroup threads = service.group("ths", "Threads");
    private final static StatusItem thrAll = threads.item("All");
    private final static StatusItem thrRunnable = threads.item("Runnable");
    private final static StatusItem thrBlocked = threads.item("Blocked");
    private final static StatusItem thrWaiting = threads.item("Waiting");
    private final static StatusItem thrNew = threads.item("New");
    private final static StatusItem thrTerminated = threads.item("Terminated");

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

        JObject json = new JObject();

        uptime.value(new Interval(mxBean.getUptime(), Unit.MILLISECONDS)
                .displayPrecision(Unit.SECONDS)
                .toString());

        json.put("uptime", uptime.value);

        if (cpu == null)
            cpu = service.item("CPU [" + osBean.getAvailableProcessors() + "]");

        cpu.value(Utils.formatPercent(osBean.getProcessCpuLoad()) + " / "
                + Utils.formatPercent(osBean.getSystemCpuLoad()));

        json.arrayC("cpu").add(osBean.getProcessCpuLoad() * 100d)
                .add(osBean.getSystemCpuLoad() * 100d);

        {
            long total = osBean.getTotalPhysicalMemorySize();
            long used = total - osBean.getFreePhysicalMemorySize();

            json.objectC("mem").arrayC("sys").add(used).add(total);

            memSys.value(formatSize(used) + " / " + formatSize(total))
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

            json.objectC("mem").arrayC("app").add(used).add(total).add(max);

            memApp.value(formatSize(used) + " / " + formatSize(total) + " / " + formatSize(max))
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

        JObject jthreads = new JObject();
        JArray jth = jthreads.arrayC("list");

        for (Thread th : threads)
            if (th != null && th.getState() != null) { // tu może wystąpić null
                ThreadGroup gr = th.getThreadGroup();
                ThreadInfo info = thBean.getThreadInfo(th.getId());

                jth.array().addAll(
                        th.getId(),
                        th.getPriority(),
                        th.getState().name().toLowerCase(),
                        gr != null ? gr.getName() : null,
                        th.isAlive(),
                        th.isDaemon(),
                        th.isInterrupted(),
                        th.getName(),
                        thBean.getThreadCpuTime(th.getId()) / 100000,
                        thBean.getThreadUserTime(th.getId()) / 100000,
                        thBean.getThreadAllocatedBytes(th.getId()),
                        info != null ? info.getBlockedCount() : 0,
                        info != null ? info.getWaitedCount() : 0
                );

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
            }

        int act = Thread.activeCount();
        json.objectC("thr")
                .put("act", act)
                .put("run", trun)
                .put("blc", tblc)
                .put("wait", twait)
                .put("term", tterm);

        jthreads.add("stats", json.object("thr"));

        thrAll.value(act);
        thrRunnable.value(trun);
        thrBlocked.value(tblc).color(tblc > 0 ? warningColor
                : null);
        thrWaiting.value(twait);
        thrNew.value(tnew);
        thrTerminated.value(tterm);

        new WebApiBroadcast("service.status", json).send();
        new WebApiBroadcast("service.threads", jthreads).send();
    }

    static {
        WNotifications.notifySources.put("service.status",
                new LStr("Status systemu i usługi"));

        WNotifications.notifySources.put("service.threads",
                new LStr("Wątki"));
    }

}
