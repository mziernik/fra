package com.html.modules;

import com.html.core.Html;
import com.html.core.tag.Body;
import com.html.core.tag.Tag;
import com.html.core.tag.formatting.Pre;
import com.html.core.tag.intfs.BuildListener;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.programming.Script;
import com.html.js.Eval;
import com.resources.Res;

/**
 * @author Miłosz Ziernik
 * @date 29 października 2015
 * @encoding UTF-8
 */
public class SyntraxHighliter implements Tag<Pre> {

    public static enum Brush {

        actionscript3("shBrushAS3.js"),
        bash("shBrushBash.js"),
        shell("shBrushBash.js"),
        coldfusion("shBrushColdFusion.js"),
        csharp("shBrushCSharp.js"),
        cpp("shBrushCpp.js"),
        c("shBrushCpp.js"),
        css("shBrushCss.js"),
        delphi("shBrushDelphi.js"),
        pascal("shBrushDelphi.js"),
        diff("shBrushDiff.js"),
        patch("shBrushDiff.js"),
        erlang("shBrushErlang.js"),
        groovy("shBrushGroovy.js"),
        javascript("shBrushJScript.js"),
        java("shBrushJava.js"),
        javafx("shBrushJavaFX.js"),
        perl("shBrushPerl.js"),
        php("shBrushPhp.js"),
        plain("shBrushPlain.js"),
        text("shBrushPlain.js"),
        powershell("shBrushPowerShell.js"),
        python("shBrushPython.js"),
        ruby("shBrushRuby.js"),
        scala("shBrushScala.js"),
        sql("shBrushSql.js"),
        vb("shBrushVb.js"),
        xml("shBrushXml.js");

        final String script;

        private Brush(String script) {
            this.script = script;
        }

    }

    private final Pre script;

    public SyntraxHighliter(Parent parent, Brush brush, Object code) {
        script = new Pre(parent);
        script.cls("brush: " + brush.name() + ", toolbar: false;");
        Html html = parent.getHTML();
        html.head.link(Res.syntaxhighlighter);

        html.head.link("/res/syntaxhighlighter/" + brush.script);
        html.head.link("/res/syntaxhighlighter/shThemeDefault.css");
        html.body.node.addBuildListener(new BuildListener<Body>() {

            @Override
            public boolean onBeforeBuildTag(Body tag) {
                tag.script((new Eval("SyntaxHighlighter.all();")));
                return true;
            }
        });
        script.text(code);
    }

    @Override
    public Pre getElement() {
        return script;
    }

}
