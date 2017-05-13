package com.database.elements;

import com.utils.collections.TList;
import java.util.LinkedHashMap;

public class MetaCatalog {

    public final MetaDbStructure struct;
    public final LinkedHashMap<String, MetaSchema> schemas = new LinkedHashMap<>();

    public final String name;

    public MetaCatalog(MetaDbStructure struct, String name) {
        this.struct = struct;
        this.name = name;
        if (struct.mainCatalog == null)
            struct.mainCatalog = this;
        struct.catalogs.put(name, this);
    }

    @Override
    public String toString() {
        return name;
    }

}
