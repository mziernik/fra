package com.config;

import com.config.engine.ConfigNode;
import com.config.engine.field.CfBool;
import com.config.engine.field.CfInt;
import com.config.engine.field.CfStringList;
import com.config.engine.interfaces.Cfg;
import static com.lang.LConfig.*;

public class CContent extends ConfigNode {

    public CContent() {
        super(CService.class, "content", CONTENT__CONTENT);
    }
    @Cfg
    public final static CfBool compactMode = new CfBool("html.compact",
            CONTENT__COMPACT,
            false)
            .description(CONTENT__COMPACT__DESCRIPTION);

    @Cfg
    public final static CfBool compactJS = new CfBool("html.compact_js",
            CONTENT__COMPACT_JS,
            true)
            .description(CONTENT__COMPACT_JS__DESCRIPTION);

    @Cfg
    public final static CfBool xhtmlMode = new CfBool("html.xhtml",
            CONTENT__XHTML_MODE,
            false);

    @Cfg
    public final static CfBool addHrefUid = new CfBool("html.href_uid",
            CONTENT__ADD_HREF_UID,
            false)
            .description(CONTENT__ADD_HREF_UID__DESCRIPTION);

    @Cfg
    public final static CfStringList compressionMimeTypes = new CfStringList("econtent.compression_mime_types",
            CONTENT__COMPRESSION_MIME_TYPES,
            "text/*", "application/javascript", "*html*", "*xml*");

    @Cfg
    public final static CfBool eTags = new CfBool("cache.etag",
            CONTENT__ETAGS,
            true);

    @Cfg
    public final static CfBool errorPageMemoryInfo = new CfBool("error_page.memory_info",
            CONTENT__ERROR_PAGE_MEMORY_INFO,
            false);

    @Cfg
    public final static CfInt maxTableResults = new CfInt("max_rows_count",
            CONTENT__MAX_ROWS_COUNT,
            100);

    @Cfg
    public final static CfBool searchIndexHtml = new CfBool("index_html",
            CONTENT__SEARCH_INDEX_HTML,
            true);

//    /*
//     public static boolean addHrefUid() {
//     return addHrefUid.value(!debugMode());
//     }
//
//     public static boolean compactMode() {
//     return compactMode.value(!debugMode());
//     }
//     */
}
