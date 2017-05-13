package com.servlet.views.events;

import com.utils.Utils;
import com.utils.Is;
import java.util.Map;

public abstract class ViewEvent<TData extends ViewEventData> {

    public final String id = Utils.randomId(10);

    protected abstract void doProcessEvent(Map<String, String> data) throws Exception;

    protected abstract void onEvent(TData data) throws Exception;

}
