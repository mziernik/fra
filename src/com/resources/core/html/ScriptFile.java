package com.resources.core.html;

import com.utils.Utils;
import com.config.CContent;
import com.html.core.tag.intfs.Parent;
import com.utils.Url;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Miłosz Ziernik
 * @date 05 listopada 2015
 * @encoding UTF-8
 */
public class ScriptFile extends ResourceFile {

    public final Map<String, String> files = new LinkedHashMap<>();

    public ScriptFile(String... files) {
        if (files == null)
            return;
        for (String f : files)
            this.files.put(f, null);
    }

    public ScriptFile file(String normalFilePath) {
        return file(normalFilePath, null);
    }

    public ScriptFile file(String normalFilePath, String compactFilePath) {
        files.put(normalFilePath, compactFilePath);
        return this;
    }

    public void link(Parent parent) {
        for (ScriptFile sf : dependencies)
            if (sf != this)
                sf.link(parent);

        Boolean compact = parent.getHTML().compact;

        if (compact == null)
            compact = CContent.compactMode.value(false);

        for (Map.Entry<String, String> en : files.entrySet()) {
            String full = en.getKey();
            String min = en.getValue();

            String ff = compact ? min : full;
            if (ff == null)
                ff = Utils.coalesce(full, min);

            if (ff == null)
                continue;

            // IdxRes idx = IdxRes.getByName(ff);
            super.link(parent, new Url(ff));

        }
    }

    @Override
    public ScriptFile depends(Class<? extends ScriptFile> other) {
        super.depends(other);
        return this;
    }

    @Override
    public ScriptFile depends(ScriptFile other) {
        super.depends(other);
        return this;
    }

}
