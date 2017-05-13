package com.database.async;

import com.database.queries.MultipleQuery;
import com.database.queries.builder.QueryBuilder;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.util.Objects;

public class AsyncQuery {

    public final Object context;
    public final AsyncDatabase executor;
    private boolean executed;
    public final int delay;
    public final long created = System.currentTimeMillis();
    public final AsyncQueryResult callback;
    public final AsyncQueryProvider provider;
    public final boolean priority;

    public MultipleQuery mqry = null;

    AsyncQuery(AsyncDatabase executor, Object context, Interval delay,
            boolean priority, AsyncQueryProvider provider, AsyncQueryResult callback) {
        super();
        this.delay = delay != null ? (int) delay.getTime(Unit.MILLISECONDS) : 0;
        this.context = context;
        this.executor = Objects.requireNonNull(executor);
        this.callback = callback;
        this.provider = provider;
        this.priority = priority;
        executor.add(this);
    }

    MultipleQuery getQuery() throws Exception {
        if (mqry == null) {
            mqry = executor.db.multipleQuery();
            if (provider != null)
                provider.build(mqry);
        }
        return mqry;
    }

}
