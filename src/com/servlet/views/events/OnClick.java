package com.servlet.views.events;

import com.servlet.views.events.OnClick.OnClickData;
import java.util.Map;

public abstract class OnClick extends ViewEvent<OnClickData> {

    @Override
    protected void doProcessEvent(Map<String, String> data) throws Exception {

    }

    public static class OnClickData extends ViewEventData {

    }

}
