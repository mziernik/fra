package com.mlogger.data;

import java.io.IOException;
import java.lang.reflect.Field;

public interface ObjectProvider {

    public Object getObject(final Class<?> cls, final long id) throws IOException, InstantiationException, IllegalAccessException;
}
