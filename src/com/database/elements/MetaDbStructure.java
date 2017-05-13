package com.database.elements;

import java.util.LinkedHashMap;

public class MetaDbStructure {

    public MetaCatalog mainCatalog;
    public final LinkedHashMap<String, MetaCatalog> catalogs = new LinkedHashMap<>();

}
