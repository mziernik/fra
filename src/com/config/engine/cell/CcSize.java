package com.config.engine.cell;

import com.config.engine.ConfigCell;
import com.config.engine.DataType;
import com.json.*;

import com.utils.Size;
import com.utils.Size.SizeUnit;

public class CcSize extends ConfigCell<Size, Size> {

    public CcSize(CharSequence name) {
        super(DataType.SIZE, Size.class, name);
        units((map) -> {
            for (SizeUnit u : SizeUnit.values())
                map.put(u.name().toLowerCase(), u.name());
        });

        selectedUnit = SizeUnit.KB.name();
    }

    @Override
    public Size doParse(JElement json) throws Exception {

        if (json.isArray() && json.asArray().size() == 2) {
            JArray arr = json.asArray();
            Number val = arr.element(0).asValue().asNumber();
            String unit = arr.element(1).asValue().asString();
            return new Size(val.longValue(), SizeUnit.valueOf(unit));

        }
        return new Size(json.asValue().asNumber().longValue());
    }

    @Override
    protected String getDisplayValueRAW(Size value, boolean publicValue) {
        return value != null ? value.toStringFrmt() : null;
    }

    @Override
    public JElement serialize(Size value) {
        if (value == null)
            return new JNull();
        return new JArray().add(value.getValue(value.unit())).add(value.unit().name());
    }

}
