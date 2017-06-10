package com.thread;

import com.events.Dispatcher;
import com.exceptions.ServiceException;
import com.intf.runnable.Runnable2;
import com.intf.runnable.RunnableEx;
import com.mlogger.Log;
import com.thread.RunLater.Task;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Map.Entry;

/**
 * Klasa wykonuje zadania w osobnym wątku po zadanym czasie. Jeśli źródło
 * zadania się powtarza, zostanie zastąpione
 *
 * @author user
 */
public class RunLater {

    private static RunLaterThread instance;
    private final static HashMap<Object, Task> map = new HashMap<>();

    public static interface TaskRunnable<T> {

        void run(Task<T> task, T source) throws Throwable;
    }

    public static Task<Object> add(int delayMs, RunnableEx runnable) {
        return add(new Object(), delayMs, (Task<Object> task, Object source) -> {
            runnable.run();
        });
    }

    public static <T> Task<T> add(T object, int delayMs, TaskRunnable<T> runnable) {
        if (object == null || runnable == null)
            return null;

        synchronized (map) {
            Task task = new Task(object, delayMs, runnable, map.get(object));
            map.put(object, task);

            if (instance == null) {
                instance = new RunLaterThread();
                instance.start();
            }

            return task;
        }
    }

    private static class RunLaterThread extends TThread {

        private RunLaterThread() {
            super("AsyncTaskRunner");
        }

        @Override
        protected void run() throws Exception {

            while (isRunning())
                try {
                    Thread.sleep(10);

                    final long now = System.currentTimeMillis();

                    LinkedList<Task<?>> queue = new LinkedList<>();
                    synchronized (map) {
                        HashMap<Object, Task> clone = (HashMap<Object, Task>) map.clone();
                        for (Entry<Object, Task> en : clone.entrySet()) {
                            Task it = en.getValue();
                            if (it.created + it.delayMs < now) {
                                queue.add(it);
                                map.remove(en.getKey());
                            }
                        }
                    }

                    for (Task<?> task : queue)
                        try {
                            task.allTasks.addAll(queue);
                            task.run();
                        } catch (Throwable e) {
                            task.onError(e);
                        }

                } catch (Throwable e) {
                    Log.error(e);
                }

        }

    }

    public static class Task<T> {

        public final T source;
        public final int delayMs;
        public final TaskRunnable runnable;
        public final long created = System.currentTimeMillis();
        public final Task<T> overrided;
        public final LinkedList<Task<?>> allTasks = new LinkedList<>();
        private boolean cancelled;
        public final Dispatcher<Runnable2<Task<T>, Throwable>> onError = new Dispatcher<>();

        private Task(T source, int delayMs, TaskRunnable runnable, Task<T> overrided) {
            this.source = source;
            this.delayMs = delayMs;
            this.runnable = runnable;
            this.overrided = overrided;
        }

        public void cancel() {
            cancelled = true;
        }

        public void run() {
            if (cancelled)
                return;
            try {
                runnable.run(this, source);
            } catch (RuntimeException | Error ex) {
                throw ex;
            } catch (Throwable e) {
                throw new ServiceException(e);
            }

        }

        private void onError(Throwable e) {
            if (onError.isEmpty()) {
                Log.error(e);
                return;
            }

            onError.dispatch(this, intf -> intf.run(this, e));
        }

    }
}
