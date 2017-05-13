package com.filter;

import com.json.JArray;
import com.json.JObject;
import com.lang.core.LString;
import java.util.*;

/**
 * @author Błażej Palmąka 2017/01/16
 */
public abstract class AbstractFilter<TSelf extends AbstractFilter> {

    //***************************************************
    //*                    ZMIENNE                      *
    //***************************************************
    private static int count = 0;
    final Integer order;

    public final String key;
    public final LString caption;
    public FFilterType type;
    public boolean hidden;

    private final Map<String, LString> options;            // lista opcji (tylko dla typu ENUM)
    private final Set<FCondition> conditions;             // obsługiwane warunki
    public final List<String> defValues;                  // domyślne wartości

    //***************************************************
    //*                  KONSTRUKTOR                    *
    //***************************************************
    public AbstractFilter(String key, LString caption) {
        this.order = count++;
        this.key = key;
        this.caption = caption;

        this.type = FFilterType.TEXT;
        this.hidden = false;
        this.conditions = new LinkedHashSet<>();
        this.defValues = new LinkedList<>();
        this.options = new LinkedHashMap<>();
    }

    //***************************************************
    //*                    METODY                       *
    //***************************************************
    public static interface FilterOptionsEdit {

        public void action(Map<String, LString> options);
    }

    public TSelf condition(FCondition... conditions) {
        this.conditions.addAll(Arrays.asList(conditions));
        return (TSelf) this;
    }

    public TSelf hide(boolean hidden) {
        this.hidden = hidden;
        return (TSelf) this;
    }

    public TSelf type(FFilterType type) {
        this.type = type;
        return (TSelf) this;
    }

    public TSelf options(FilterOptionsEdit filterOptionsEditor) {
        filterOptionsEditor.action(options);
        return (TSelf) this;
    }

    public TSelf addOption(String name, LString value) {
        this.options.put(name, value);
        return (TSelf) this;
    }

    @Override
    public String toString() {
        return "Filter{" + "key=" + key + ", displayName=" + caption.toString() + '}';
    }

    public JObject toJson() {
        return toJson(true);
    }

    public boolean supportedCondition(FCondition cond) {
        return conditions.contains(cond);
    }

    public JObject toJson(boolean compactMode) {
        JObject json = new JObject();
        json.options.compactMode(compactMode);

        String strType = (type == FFilterType.TEXT_ARRAY ? "text" : type.name().toLowerCase());

        json._put("key", key);
        json._put("name", caption.toString());
        json._put("ftype", strType);
        json._put("hidden", hidden);

        if (type == FFilterType.ENUM) {
            JArray opts = new JArray();
            json._put("options", opts);

            for (String optKey : options.keySet())
                opts.add(new JObject()
                        .add("key", optKey)
                        .add("name", options.get(optKey))
                );
        }

        // Lista warunków
        JArray jConditions = json.arrayC("conds");
        List<FCondition> tmpCond = (!conditions.isEmpty() ? new LinkedList<>(conditions) : FCondition.getForType(type));
        for (FCondition cond : tmpCond)
            jConditions.add(cond.name().toLowerCase());

        // Lista domyślnych wartości
        JArray jDefValues = json.arrayC("defValues");
        jDefValues.addAll(defValues);

        return json;
    }
}
