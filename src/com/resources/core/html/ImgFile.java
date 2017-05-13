package com.resources.core.html;

import com.html.core.tag.image.Img;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;
import com.utils.Url;

/**
 * @author Miłosz Ziernik
 * @date 05 listopada 2015
 * @encoding UTF-8
 */
public class ImgFile extends ResourceFile {

    public final Url href;

    public ImgFile(final String href) {
        this.href = new Url(href);
    }

    public Visual createTag(Parent parent) {
        return new Img(parent).src(href);
    }

    // metoda do przeciążenia, deklarowany jest tag, w którym plik zasobu został użyty
    public void setTag(Visual tag) {

    }
}
