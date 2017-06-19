package com.lang.core;

import com.cache.CachedData;
import com.config.CService;
import com.json.*;
import com.lang.core.Language.LangEntry;
import com.model.repository.DynamicRepo;
import com.model.repository.Repository;
import com.net.http.HttpClient;
import com.net.http.HttpRequest;
import com.net.http.HttpResponse;
import com.script.ConfFile;
import com.servlet.Handlers;
import com.servlet.interfaces.Arg;
import com.utils.Url;
import com.utils.Is;
import com.utils.collections.TList;
import com.utils.reflections.datatype.DataType;
import com.webapi.core.*;
import java.io.IOException;

public class WLanguage implements WebApi {

    @WebApiEndpoint()
    public JObject getAll() {
        JObject json = new JObject();
        for (Language lang : Languages.allLanguages.values())
            json.put(lang.key, lang.name);
        return json;
    }

    @WebApiEndpoint()
    public JObject getTranslation(WebApiRequest req,
            @Arg(name = "language", required = false) String langId,
            @Arg(name = "groups", required = false) String[] groups) {

        Languages.refreshExternals();
        JObject json = new JObject();

        Language lang = !Is.empty(langId) ? Languages.getF(langId) : req.controller.language.get();
        boolean hasGroup = groups != null && groups.length > 0;

        for (LanguageItem lse : Languages.allItems.values())
            if (!hasGroup || Is.in(lse.group, groups))
                json.objectC(lse.group).put(lse.key, lse.getEntry(lang));
        return json;
    }

    @WebApiEndpoint()
    public JObject getCurrent(WebApiRequest req) {

        Language lang = req.controller.language.get();
        return new JObject()
                .put("key", lang.key)
                .put("name", lang.key);
    }

    @WebApiEndpoint()
    public void setCurrent(WebApiRequest req,
            @Arg(name = "id") String id) {

        Language lang = Languages.getF(id);
        req.controller.language.set(lang);
        if (req.http != null && req.http.session != null)
            req.http.session.language.set(lang);

    }

    @WebApiEndpoint()
    public JObject add(@Arg(name = "key") String key,
            @Arg(name = "name") String name) throws Exception {

        Language lang = Handlers.language.getInstance().create(key, name);
        return new JObject()
                .put("key", lang.key)
                .put("name", lang.name);
    }

    @WebApiEndpoint()
    public void remove(@Arg(name = "lang") String key) throws Exception {
        Handlers.language.getInstance().remove(Languages.getF(key));
    }

    @WebApiEndpoint()
    public JObject editEntry(@Arg(name = "language") String id,
            @Arg(name = "key") String key,
            @Arg(name = "value") String value,
            @Arg(name = "complete") Boolean complete) throws Exception {

        Language lang = Languages.getF(id);
        LanguageItem item = Languages.getItemF(key);

        LangEntry entry = item.get(lang);
        if (entry == null)
            entry = lang.new LangEntry(item, value, complete);

        entry.value = value;
        entry.complete = complete;

        Handlers.language.getInstance().editEntry(entry);

        return new JObject()
                .put("key", entry.item.key)
                .put("language", entry.language.key)
                .put("complete", entry.complete)
                .put("value", entry.value);
    }

    @WebApiEndpoint
    public CachedData export(@Arg(name = "language") String id) throws IOException {
        return new CachedData("aaa", "vvv", "sdgdgdfsg", 300, "dfgdfsgdsgf".getBytes());
    }

    @WebApiEndpoint(name = "import", channel = WebApiChannelType.HTTP)
    public void import_() throws Exception {

    }

    @WebApiEndpoint()
    public Repository<String> getEntries(
            @Arg(name = "langKey") String langKey,
            @Arg(name = "refKey", required = false) String refKey,
            @Arg(name = "incomplete", required = false) Boolean incomplete) {

        Languages.refreshExternals();

        DynamicRepo<LanguageItem, String> repo = new DynamicRepo<>(c -> {
            c.key = "langEntries";
            c.name = "Entries";
        });

        Language lang = Languages.getF(langKey);
        Language ref = Is.empty(refKey) ? null : Languages.getF(refKey);

        repo.column(String.class, c -> {
            c.type = DataType.KEY;
            c.key = "id";
            c.name = new LStr("Id");
        }, e -> e.id);

        repo.column(String.class, c -> {
            c.key = "group";
            c.type = DataType.STRING;
            c.name = new LStr("Group");
        }, e -> e.group);

        repo.column(String.class, c -> {
            c.key = "key";
            c.type = DataType.KEY;
            c.name = new LStr("Key");
        }, e -> e.key);

        if (ref != null)
            repo.column(String.class, c -> {
                c.key = "ref";
                c.type = DataType.STRING;
                c.name = new LStr(ref.name);
            }, e -> {
                LangEntry en = e.get(ref);
                return en != null ? en.value : null;
            });

        repo.column(String.class, c -> {
            c.key = "val";
            c.type = DataType.STRING;
            c.name = new LStr(lang.name);
        }, e -> {
            LangEntry en = e.get(lang);
            return en != null ? en.value : null;
        });

        repo.column(Boolean.class, c -> {
            c.key = "complete";
            c.type = DataType.BOOLEAN;
            c.name = new LStr("Zatwierdzone");
        }, e -> {
            LangEntry en = e.get(lang);
            return en != null ? en.complete : false;
        });

        //---------------------------------------------------------------------
        repo.fillRows(Languages.allItems.values());

        return repo;
    }

    @WebApiEndpoint
    public final WGoogleTranslate googleTranslate = new WGoogleTranslate();

    public static class WGoogleTranslate implements WebApi {

        @WebApiEndpoint(cancelable = true)
        public JObject autoTransplate(WebApiRequest request,
                @Arg(name = "sl") String sl,
                @Arg(name = "tl") String tl
        ) throws IOException, InterruptedException, Exception {

            Language src = Languages.getF(sl);
            Language dst = Languages.getF(tl);

            TList<LangEntry> items = new TList<>();

            for (LanguageItem li : Languages.allItems.values()) {
                LangEntry sen = li.get(src);
                LangEntry den = li.get(dst);

                if (sen != null && (den == null || (!den.complete && Is.empty(den.value))))
                    items.add(sen);
            }

            HttpClient httpClient = new HttpClient();

            int idx = 1;
            for (LangEntry en : items) {
                if (request.isCancelled())
                    break;

                HttpResponse resp = doTranslate(httpClient, src.key, dst.key, en.value);

                if (resp.code() != 200 || !"application/json".equals(resp.contentType()))
                    throw new Error("Error " + resp.code());

                JArray arr = JArray.parse(resp.readStr());
                arr = arr.getArrays().first();
                if (arr == null)
                    throw new Error("Unknown response");
                arr = arr.getArrays().first();
                if (arr == null)
                    throw new Error("Unknown response");

                JValue jval = arr.getValues().first();
                if (jval == null)
                    throw new Error("Unknown response");

                String val = jval.asString().trim();

                if (val.isEmpty())
                    throw new Error("Empty value");

                for (int i = 1; i < 10; i++)
                    val = val.replace("% " + i, "%" + i).replace(i + "%", "%" + i);

                if (request.isCancelled())
                    break;

                request.event("translate", "progress",
                        new JObject()
                                .put("index", idx)
                                .put("total", items.size())
                                .put("srcPhrase", en.value)
                                .put("dstPhrase", val));

                Handlers.language.getInstance().editEntry(dst.new LangEntry(en.item, val, true));

                if (CService.devMode()) {

                    Thread.sleep(1000);
                    if (idx > 10)
                        break;
                }

                ++idx;
            }

            return new JObject();
        }

        @WebApiEndpoint()
        public JObject translate(WebApiRequest request,
                @Arg(name = "sl") String sl,
                @Arg(name = "tl") String tl,
                @Arg(name = "text") String text
        ) throws IOException {
            HttpClient httpClient = new HttpClient();

            HttpResponse resp = doTranslate(httpClient, sl, tl, text);
            String readStr = resp.readStr();

            JObject result = new JObject().put("code", resp.code());

            if (resp.code() == 200 && "application/json".equals(resp.contentType()))
                return result.put("result", JSON.parse(readStr));

            return result.put("result", readStr);
        }

        public HttpResponse doTranslate(HttpClient httpClient,
                String sl,
                String tl,
                String text
        ) throws IOException {

            Url url = new Url("https://translate.googleapis.com/translate_a/single")
                    .param("client", "gtx")
                    .param("sl", sl)
                    .param("tl", tl)
                    .param("dt", "t")
                    .param("text", text);

            HttpRequest req = httpClient.request(url);

            req.header("User-Agent", "Mozilla/5.0 (Windows NT 10.0; WOW64) "
                    + "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Safari/537.36");
            req.header("Accept", "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8");
            req.header("Accept-Language", "pl-PL,pl;q=0.8,en-US;q=0.6,en;q=0.4");
            req.header("Cache-Control", "no-cache");
            req.header("Pragma", "no-cache");
            req.header("Referer", "https://www.google.pl/");

            return req.send();

        }

        @WebApiEndpoint
        public JObject getLanguages() throws IOException {

            HttpClient httpClient = new HttpClient();

            HttpRequest req = httpClient.request(new Url("https://ssl.gstatic.com/inputtools/js/ln/13/pl.js"));

            HttpResponse resp = req.send();

            String readStr = resp.readStr();

            JObject json = new ConfFile(readStr).process();
            return json.objectD("window").objectD("LanguageDisplays");
        }
    }
}
