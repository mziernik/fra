package com.script.evaluator.element;

import com.script.evaluator.Pos;
import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import java.util.ArrayList;
import com.utils.Is;

public class EArray extends ECollection {

    public EArray(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("[");
        super.getContent(writer);
        writer.append("]");
    }

    @Override
    public EValue eval() {
        ArrayList<Object> list = new ArrayList<>();
        for (Element el : children)
            Is.notNullV(el.eval(), v -> list.add(v.value()));

        return new EValue(parent, list, pos);
    }

}
