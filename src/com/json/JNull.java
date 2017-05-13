package com.json;

import com.utils.text.StrWriter;
import java.io.IOException;

/**
 * Miłosz Ziernik 2014/04/15
 */
public class JNull extends JElement {

    public JNull() {
        super();
    }

    @Override
    public boolean remove() {
        return parent != null ? parent.doRemove(this) : false;
    }

    @Override
    public void move(JCollection destination) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void moveChildren(JCollection destination) {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("null");
    }

    @Override
    public boolean isEmpty() {
        return true;
    }
}
