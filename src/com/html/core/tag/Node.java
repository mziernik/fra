package com.html.core.tag;

import com.utils.reflections.Reflections;
import com.html.core.tag.form.Form;
import com.html.core.tag.intfs.*;
import java.util.*;

public class Node<TTag extends Element> {

    public final TTag self;
    final Set<BuildListener<TTag>> beforeBuildEvents = new LinkedHashSet<>();
    final Set<TagChangeListener> changeListeners = new LinkedHashSet<>();
    public final Element.BuildOptions options;

    public Node(TTag tag) {
        this.self = tag;
        options = self.new BuildOptions();
    }

    public void addBuildListener(BuildListener<TTag> event) {
        beforeBuildEvents.add(event);
    }

    public void addChangeListener(TagChangeListener listener) {
        changeListeners.add(listener);
    }

    public void visitTag(TagVisitor visitor) {
        self._visitTag(visitor);
    }

    void onCreateTag(Element child) {
        for (TagChangeListener listener : changeListeners)
            listener.onCreateTag(child);

        Tag parent = this.self.getParent();
        if (parent != null)
            parent.getElement().node.onCreateTag(child);

    }

    public void setName(String tagName) {
        self.name = tagName;
    }

    public void add(Element tag) {
        self.children.add(tag);
        tag.parent = self;
        onCreateTag(tag);
    }

}
