package com.resources;

import com.resources.core.html.ImgFile;
import com.resources.core.html.ScriptFile;

/**
 * Miłosz Ziernik 2013/02/22
 */
/**
 * Zasoby serwera - style, skrypty obrazki itp
 */
public abstract class Res {

    public final static ImgFile close16png = new ImgFile("/res/img/16/close.png");
    public final static ImgFile apply16png = new ImgFile("/res/img/16/apply.png");
    public final static ImgFile add16png = new ImgFile("/res/img/16/add.png");
    public final static ImgFile addUser16png = new ImgFile("/res/img/16/add_user.png");
    public final static ImgFile history16png = new ImgFile("/res/img/16/history.png");
    public final static ImgFile edit16png = new ImgFile("/res/img/16/edit.png");
    public final static ImgFile help16png = new ImgFile("/res/img/16/help.png");
    public final static ImgFile remove16png = new ImgFile("/res/img/16/remove.png");
    public final static ImgFile save16png = new ImgFile("/res/img/16/save.png");
    public final static ImgFile excel16png = new ImgFile("/res/img/16/excel.png");
    public final static ImgFile filter16png = new ImgFile("/res/img/16/filter.png");
    public final static ImgFile refresh16png = new ImgFile("/res/img/16/refresh.png");
    public final static ImgFile run16png = new ImgFile("/res/img/16/run.png");

    //----------------------
    public final static ImgFile error48png = new ImgFile("/res/img/error.png");
    public final static ImgFile info48png = new ImgFile("/res/img/info.png");
    public final static ImgFile warning48png = new ImgFile("/res/img/warning.png");
    public final static ImgFile question48png = new ImgFile("/res/img/question.png");
    //----------------------

    public final static ScriptFile utils = new ScriptFile("/res/utils.js");
    public final static ScriptFile jQuery = new ScriptFile("/res/jquery/jquery.js");

    public final static ScriptFile ace = new ScriptFile("/res/ace/ace.js", "/res/ace/ace_langs.js");

    public final static ScriptFile awesome = new ScriptFile("/res/font-awesome/font-awesome.css");

    public final static ScriptFile filesSaver = new ScriptFile("/res/FileSaver.js");

    public final static Bootstrap bootstrap = new Bootstrap(null);

    public final static ScriptFile dsTable = new ScriptFile(
            "/res/ds-table/ds-table.js",
            "/res/ds-table/ds-table-cell.js",
            "/res/ds-table/ds-table-column.js",
            "/res/ds-table/ds-table-columns.js",
            "/res/ds-table/ds-table-row.js",
            "/res/ds-table/ds-table-rows.js",
            "/res/ds-table/ds-table-context-menu.js",
            "/res/ds-table/ds-table.css",
            "/res/similarity.js")
            .depends(utils);

    public final static ScriptFile dsTree = new ScriptFile(
            "/res/ds-tree/ds-tree.js",
            "/res/ds-tree/ds-tree.css")
            .depends(utils);

    public final static ScriptFile dsModal = new ScriptFile(
            "/res/ds-modal/ds-modal.js",
            "/res/ds-modal/ds-modal.css")
            .depends(utils);

    public final static ScriptFile layer = new ScriptFile(
            "/res/layer/layer.js",
            "/res/layer/layer.css")
            .depends(utils);

    public final static ScriptFile checkboxx = new ScriptFile(
            "/res/checkbox/checkbox-x.js",
            "/res/checkbox/checkbox-x.css"
    );

    public final static ScriptFile popup = new ScriptFile(
            "/res/popup/popup.js",
            "/res/popup/popup.css")
            .depends(utils);

    public final static ScriptFile upload = new ScriptFile(
            "/res/upload.js");

    public final static ScriptFile samples = new ScriptFile(
            "/res/samples/samples.js",
            "/res/samples/samples.css");

    public final static ScriptFile sweetAlert = new ScriptFile(
            "/res/swal/sweetalert.js",
            "/res/swal/sweetalert.css")
            .depends(Bootstrap.class);

    public final static ScriptFile enhsplitter = new ScriptFile(
            "/res/jquery/enhsplitter/enhsplitter.js",
            "/res/jquery/enhsplitter/enhsplitter.css")
            .depends(jQuery);

    public final static ScriptFile syntaxhighlighter = new ScriptFile(
            "/res/syntaxhighlighter/shCore.js",
            "/res/syntaxhighlighter/shCore.css");

    public static class Bootstrap extends ScriptFile {

        public Bootstrap(String theme) {
            super("/res/bootstrap/js/bootstrap.min.js",
                    "/res/bootstrap/css/bootstrap.css",
                    "/res/bootstrap/css/bootstrap-theme.css");
            depends(jQuery);
        }

    }

}
