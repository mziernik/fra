package com.html;

import com.config.CService;
import com.context.AppContext;
import com.context.index.Index;
import com.exceptions.EError;
import com.exceptions.ErrorMessage;
import com.html.core.Html;
import com.html.core.tag.Element;
import com.html.core.tag.programming.Script;
import com.intf.runnable.RunnableEx2;
import com.json.Escape;
import com.json.JElement;
import com.json.JObject;
import com.json.JSON;
import com.resources.core.Resources;
import com.resources.core.html.ResourceFile;
import com.resources.core.html.ScriptFile;
import com.servlet.controller.ControllerEndpoint;
import com.servlet.controller.Page;
import com.servlet.handlers.temporary.TempRequest;
import com.servlet.requests.HttpRequest;
import com.utils.Str;
import com.utils.Utils;
import com.utils.TObject;
import com.utils.collections.MapList;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import com.utils.date.TDate;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import com.utils.hashes.Hashes;
import com.utils.text.StrWriter;
import java.io.IOException;
import java.util.LinkedHashSet;
import java.util.LinkedList;

public class HtmlPackage {

    private final Page page;
    private final LinkedHashSet<String> files = new LinkedHashSet();
    private final MapList<String, String> langFiles = new MapList();

    public HtmlPackage(Page page) throws IOException {
        this.page = page;

        page.link("/res/spa/spa-loader.js");
        add("res/utils.js");
        add("res/spa/spa-core.js");
        add("res/spa/spa-controller.js");

        JObject meta = new JObject();
        meta.options.quotaNames(false).compactMode(true);
        meta.put("timestamp", new TDate().toString(true));
        meta.put("mode", CService.mode.value().name().toLowerCase());
        meta.put("language", CService.language.value().key);
        meta.put("username", page.session != null && page.session.user != null
                ? page.session.user.username : null);

        page.head.script("(window.spaVendor = window.spaVendor || {}).meta = " + meta);

        page.endpoint().beforeReturnHtml.add((ControllerEndpoint<?> ctrl, Element tag, int status) -> {

            final String hash = Hashes.idHash12(new Strings(files).toString("\n"));

            TempRequest req = TempRequest.getByExtra("spa-files-vendor", hash);

            if (!CService.devMode() && req != null) {
                Script scr = ((Html) tag).head.linkJS(req.getUrl());
                scr.async(true);
                return true;
            }

            req = new TempRequest("vendor.js", new Interval(30, AppContext.devMode
                    ? Unit.SECONDS : Unit.MINUTES), HtmlPackage.this) {
                @Override
                protected Class<? extends Page> getHandler(HttpRequest request) throws Exception {

                    String name = "vendor.js";

                    TObject<String> currentFile = new TObject<>();

                    try {
                        TList<String> langs = langFiles.get("pl");
                        final JObject jLang = new JObject();
                        jLang.options.compactMode(true).singleLine(true);
                        for (String langFile : langs) {
                            JObject json = JSON.parse(Resources.getF(page.getClass(), langFile, true)).asObject();
                            for (JElement el : json)
                                jLang.put(el.getName(), el);
                        }

                        final StrWriter writer = new StrWriter();

                        RunnableEx2<String, Object> append = (String fileName, Object content) -> {
                            currentFile.set(fileName);

                            if (writer.isEmpty())
                                writer.append("(window.spaVendor = window.spaVendor || {}).files = {\n");
                            else
                                writer.append(",\n");

                            writer.append("\t")
                                    .append(Escape.escape(fileName))
                                    .append(": ");

                            if (content instanceof JElement)
                                ((JElement) content).write(writer);
                            else
                                new Escape()
                                        .singleQuota(true)
                                        .toString(content, writer);
                        };

                        if (!jLang.isEmpty())
                            append.run("language", jLang);

                        for (String file : files)
                            append.run(file, Resources.getF(page.getClass(), file, true).getStringUtf8());

                        writer.append("\n};");
//
//                    req = new TempResource(
//                            new Interval(30, AppContext.devMode
//                                    ? Unit.SECONDS
//                                    : Unit.MINUTES),
//                            name, writer.toString().getBytes(Utils.UTF8));

                        extra.put("template-page", hash);

                        request.returnFile(writer.toString().getBytes(Utils.UTF8), name, null, eTag);

                    } catch (Throwable e) {
                        ErrorMessage ee = EError.format(e);
                        String msg = "(window.spaVendor = window.spaVendor || {}).error = "
                                + Escape.escape(currentFile.ifNonEmpty(v -> v + ": ")
                                        + ee.title + ": " + ee.message);
                        request.returnFile(msg.getBytes(Utils.UTF8), name, null, eTag);
                        throw e;

                    }
                    return null;
                }
            };

            Script scr = ((Html) tag).head.linkJS(req.getUrl());
            scr.async(true);
            return true;

        });
    }

    public HtmlPackage addDir(Package pckg) throws IOException {
        final String name = pckg.getName().replace(".", "/") + "/";

        TList<String> list = new TList<>();

        for (String s : Index.allFiles)
            if (s.startsWith(name)) {
                String f = s.toLowerCase();

                if (f.endsWith(".scss"))
                    s = new Str(s).removeSufix(".scss").sufix(".css").toString();

                if (f.contains("language.") && f.endsWith(".json")) {
                    String l = f.substring(f.indexOf("language.") + "language.".length());
                    if (l.endsWith(".json"))
                        langFiles.add(l.substring(0,
                                l.length() - ".json".length()).trim(), f);
                }

                if (f.endsWith(".html")
                        || f.endsWith(".css")
                        || f.endsWith(".scss")
                        || f.endsWith(".js")
                        || f.endsWith(".jsx")
                        || f.endsWith(".ts"))
                    list.add(s.substring(name.length()));
            }

        files.addAll(Utils.sortFileNames(list));

        return this;
    }

    public HtmlPackage add(ScriptFile file) throws IOException {
        files.addAll(file.files.keySet());
        return this;
    }

    public HtmlPackage add(String file) throws IOException {
        files.add(file);
        return this;
    }
}
