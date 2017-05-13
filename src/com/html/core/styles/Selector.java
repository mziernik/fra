package com.html.core.styles;

import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.dict.*;
import com.html.core.tag.Element;
import com.servlet.UserAgent;
import com.servlet.UserAgent.Browser;
import com.utils.text.WritableContent;
import java.util.*;

public final class Selector<TTag extends Element> implements WritableContent {

    public static interface StyleBuilderCallback {

        void setStyle(Selector builder, String name, String value);
    }

    public final Map<String, String> styles = new TreeMap<>();
    public final List<Selector<TTag>> selectors = new LinkedList<>();
    protected final TTag tag;
    final String[] names;

    public Selector(final TTag tag, String... names) {
        this.tag = tag;
        this.names = names;
    }

    public Selector<TTag> selector(String... names) {
        Selector<TTag> css = new Selector<>(tag, names);
        selectors.add(css);
        return css;
    }

    @Override
    public void getContent(StrWriter writer) {
        getContent(writer, false);
    }

    private StrWriter getContent(StrWriter writer, boolean isSelector) {
        if (styles.isEmpty() && selectors.isEmpty())
            return writer;

        if (!isSelector)
            writer.lineBreak().intent();

        boolean firstSelector = true;
        for (Selector<TTag> sel : selectors) {
            if (!firstSelector)
                writer.lineBreak(" ").intent();

            firstSelector = false;

            boolean first = true;
            if (sel.names != null)
                for (String name : sel.names) {
                    if (!first)
                        writer.append(",").lineBreak().intent();
                    writer.append(name);
                    first = false;
                }

            writer.append(" {");

            writer.nextLevel(() -> {
                writer.lineBreak("").intent();
                sel.getContent(writer, true);
            });

            writer.lineBreak().intent();
            writer.append("}");
        }

        boolean first = true;
        for (Map.Entry<String, String> sel : styles.entrySet()) {

            if (!first)
                writer.lineBreak(first ? "" : " ").intent();;

            first = false;
            // sw.append(compact ? "" : intent + "  ");
            writer.append(sel.getKey())
                    .append(": ")
                    .append(sel.getValue())
                    .append(";");
        }

        if (!isSelector)
            writer.lineBreak().intent(writer.getLevel() - 1);
        return writer;
    }

    @Override
    public String toString() {
        if (styles.isEmpty() && selectors.isEmpty())
            return null;

        StrWriter writer = new StrWriter();
        getContent(writer);
        return writer.toString();
    }

    public StyleBuilderCallback callback;

    public Selector add(String name, String value) {
        styles.put(name, value);
        return this;
    }

    //=======================================================================================
    public Selector setStyle(final String name, final String value,
            final String... def) {
        if (name == null || value == null)
            return this;

        add(name, value);
        if (callback != null)
            callback.setStyle(this, name, value);
        return this;
    }

    public final TTag tag() {
        return tag;
    }

    public final Selector<TTag> animation(final String value) {
        setStyle("-webkit-animation", value);
        return setStyle("animation", value);
    }

    public final Selector<TTag> animationName(final String value) {
        return setStyle("animation-name", value);
    }

    public final Selector<TTag> animationDuration(final String value) {
        return setStyle("animation-duration", value);
    }

    public final Selector<TTag> animationTimingFunction(final String value) {
        return setStyle("animation-timing-function", value);
    }

    public final Selector<TTag> animationDelay(final String value) {
        return setStyle("animation-delay", value);
    }

    public final Selector<TTag> animationIterationCount(final String value) {
        return setStyle("animation-iteration-count", value);
    }

    public final Selector<TTag> animationDirection(final String value) {
        return setStyle("animation-direction", value);
    }

    public final Selector<TTag> animationPlayState(final String value) {
        return setStyle("animation-play-state", value);
    }

    public final Selector<TTag> appearance(final String value) {
        return setStyle("appearance", value);
    }

    public final Selector<TTag> backfaceVisibility(final String value) {
        return setStyle("backface-visibility", value);
    }

    public final Selector<TTag> background(final String value) {
        return setStyle("background", value);
    }

    public final Selector<TTag> backgroundAttachment(final String value) {
        return setStyle("background-attachment", value);
    }

    public final Selector<TTag> backgroundColor(final String value) {
        return setStyle("background-color", value);
    }

    public final Selector<TTag> backgroundColor(final Color value) {
        return setStyle("background-color", value.name());
    }

    public final Selector<TTag> backgroundImage(final String value) {
        return setStyle("background-image", value);
    }

    public final Selector<TTag> backgroundPosition(final String value) {
        return setStyle("background-position", value);
    }

    public final Selector<TTag> backgroundRepeat(final String value) {
        return setStyle("background-repeat", value);
    }

    public final Selector<TTag> backgroundClip(final String value) {
        return setStyle("background-clip", value);
    }

    public final Selector<TTag> backgroundOrigin(final String value) {
        return setStyle("background-origin", value);
    }

    public final Selector<TTag> backgroundSize(final String value) {
        return setStyle("background-size", value);
    }

    public final Selector<TTag> border(final String value) {
        return setStyle("border", value);
    }

    public final Selector<TTag> borderBottom(final String value) {
        return setStyle("border-bottom", value);
    }

    public final Selector<TTag> borderBottomColor(final String value) {
        return setStyle("border-bottom-color", value);
    }

    public final Selector<TTag> borderBottomColor(final Color value) {
        return setStyle("border-bottom-color", value.name());
    }

    public final Selector<TTag> borderBottomStyle(final String value) {
        return setStyle("border-bottom-style", value);
    }

    public final Selector<TTag> borderBottomWidth(final String value) {
        return setStyle("border-bottom-width", value);
    }

    public final Selector<TTag> borderCollapse(final String value) {
        return setStyle("border-collapse", value);
    }

    public final Selector<TTag> borderColor(final String value) {
        return setStyle("border-color", value);
    }

    public final Selector<TTag> borderColor(final Color value) {
        return setStyle("border-color", value.name());
    }

    public final Selector<TTag> borderLeft(final String value) {
        return setStyle("border-left", value);
    }

    public final Selector<TTag> borderLeftColor(final String value) {
        return setStyle("border-left-color", value);
    }

    public final Selector<TTag> borderLeftStyle(final String value) {
        return setStyle("border-left-style", value);
    }

    public final Selector<TTag> borderLeftWidth(final String value) {
        return setStyle("border-left-width", value);
    }

    public final Selector<TTag> borderRight(final String value) {
        return setStyle("border-right", value);
    }

    public final Selector<TTag> borderRightColor(final String value) {
        return setStyle("border-right-color", value);
    }

    public final Selector<TTag> borderRightColor(final Color value) {
        return setStyle("border-right-color", value.name());
    }

    public final Selector<TTag> borderRightStyle(final String value) {
        return setStyle("border-right-style", value);
    }

    public final Selector<TTag> borderRightWidth(final String value) {
        return setStyle("border-right-width", value);
    }

    public final Selector<TTag> borderSpacing(final String value) {
        return setStyle("border-spacing", value);
    }

    public final Selector<TTag> borderStyle(final String value) {
        return setStyle("border-style", value);
    }

    public final Selector<TTag> borderTop(final String value) {
        return setStyle("border-top", value);
    }

    public final Selector<TTag> borderTopColor(final String value) {
        return setStyle("border-top-color", value);
    }

    public final Selector<TTag> borderTopColor(final Color value) {
        return setStyle("border-top-color", value.name());
    }

    public final Selector<TTag> borderTopStyle(final String value) {
        return setStyle("border-top-style", value);
    }

    public final Selector<TTag> borderTopWidth(final String value) {
        return setStyle("border-top-width", value);
    }

    public final Selector<TTag> borderWidth(final String value) {
        return setStyle("border-width", value);
    }

    public final Selector<TTag> borderBottomLeftRadius(final String value) {
        return setStyle("border-bottom-left-radius", value);
    }

    public final Selector<TTag> borderBottomRightRadius(final String value) {
        return setStyle("border-bottom-right-radius", value);
    }

    public final Selector<TTag> borderImage(final String value) {
        return setStyle("border-image", value);
    }

    public final Selector<TTag> borderImageOutset(final String value) {
        return setStyle("border-image-outset", value);
    }

    public final Selector<TTag> borderImageRepeat(final String value) {
        return setStyle("border-image-repeat", value);
    }

    public final Selector<TTag> borderImageSlice(final String value) {
        return setStyle("border-image-slice", value);
    }

    public final Selector<TTag> borderImageSource(final String value) {
        return setStyle("border-image-source", value);
    }

    public final Selector<TTag> borderImageWidth(final String value) {
        return setStyle("border-image-width", value);
    }

    public final Selector<TTag> borderRadius(final String value) {
        return setStyle("border-radius", value);
    }

    public final Selector<TTag> borderTopLeftRadius(final String value) {
        return setStyle("border-top-left-radius", value);
    }

    public final Selector<TTag> borderTopRightRadius(final String value) {
        return setStyle("border-top-right-radius", value);
    }

    public final Selector<TTag> bottom(final String value) {
        return setStyle("bottom", value);
    }

    public final Selector<TTag> box(final String value) {
        return setStyle("box", value);
    }

    public final Selector<TTag> boxAlign(final String value) {
        return setStyle("box-align", value);
    }

    public final Selector<TTag> boxDirection(final String value) {
        return setStyle("box-direction", value);
    }

    public final Selector<TTag> boxFlex(final String value) {
        return setStyle("box-flex", value);
    }

    public final Selector<TTag> boxFlexGroup(final String value) {
        return setStyle("box-flex-group", value);
    }

    public final Selector<TTag> boxLines(final String value) {
        return setStyle("box-lines", value);
    }

    public final Selector<TTag> boxOrdinalGroup(final String value) {
        return setStyle("box-ordinal-group", value);
    }

    public final Selector<TTag> boxOrient(final String value) {
        return setStyle("box-orient", value);
    }

    public final Selector<TTag> boxPack(final String value) {
        return setStyle("box-pack", value);
    }

    public final Selector<TTag> boxSizing(final BoxSizing value) {
        return setStyle("box-sizing", getValue(value));
    }

    public final Selector<TTag> boxShadow(final String value) {
        return setStyle("box-shadow", value);
    }

    public final Selector<TTag> captionSide(final String value) {
        return setStyle("caption-side", value);
    }

    public final Selector<TTag> clear(final Clear value) {
        return setStyle("clear", getValue(value));
    }

    public final Selector<TTag> clip(final String value) {
        return setStyle("clip", value);
    }

    public final Selector<TTag> color(final String value) {
        return setStyle("color", value);
    }

    public final Selector<TTag> color(final Color value) {
        return setStyle("color", getValue(value));
    }

    public final Selector<TTag> column(final String value) {
        return setStyle("column", value);
    }

    public final Selector<TTag> columnCount(final String value) {
        return setStyle("column-count", value);
    }

    public final Selector<TTag> columnFill(final String value) {
        return setStyle("column-fill", value);
    }

    public final Selector<TTag> columnGap(final String value) {
        return setStyle("column-gap", value);
    }

    public final Selector<TTag> columnRule(final String value) {
        return setStyle("column-rule", value);
    }

    public final Selector<TTag> columnRuleColor(final String value) {
        return setStyle("column-rule-color", value);
    }

    public final Selector<TTag> columnRuleColor(final Color value) {
        return setStyle("column-rule-color", value.name());
    }

    public final Selector<TTag> columnRuleStyle(final String value) {
        return setStyle("column-rule-style", value);
    }

    public final Selector<TTag> columnRuleWidth(final String value) {
        return setStyle("column-rule-width", value);
    }

    public final Selector<TTag> columnSpan(final String value) {
        return setStyle("column-span", value);
    }

    public final Selector<TTag> columnWidth(final String value) {
        return setStyle("column-width", value);
    }

    public final Selector<TTag> columns(final String value) {
        return setStyle("columns", value);
    }

    public final Selector<TTag> content(final String value) {
        return setStyle("content", value != null ? "\"" + value + "\"" : null);
    }

    public final Selector<TTag> counterIncrement(final String value) {
        return setStyle("counter-increment", value);
    }

    public final Selector<TTag> counterReset(final String value) {
        return setStyle("counter-reset", value);
    }

    public final Selector<TTag> cursor(final Cursor value) {
        return setStyle("cursor", getValue(value));
    }

    public final Selector<TTag> direction(final String value) {
        return setStyle("direction", value);
    }

    public final Selector<TTag> display(final Display value) {
        return setStyle("display", getValue(value));
    }

    public final Selector<TTag> emptyCells(final String value) {
        return setStyle("empty-cells", value);
    }

    public final Selector<TTag> flex(final Flex flex) {
        return setStyle("flex", getValue(flex));
    }

    public final Selector<TTag> flexGrow(final Integer value) {
        return setStyle("flex-grow", Utils.toString(value));
    }

    public final Selector<TTag> flexDirection(final FlexDirection direction) {
        return setStyle("flex-direction", getValue(direction));
    }

    public final Selector<TTag> float_(final SFloat value) {
        return setStyle("float", getValue(value));
    }

    public final Selector<TTag> font(final String value) {
        return setStyle("font", value);
        //'Consolas', 'Courier New', monospace"
    }

    public final Selector<TTag> fontFamily(final String value) {
        return setStyle("font-family", value);
    }

    public Selector fontFamilyMonospace() {
        return fontFamily("'Consolas', 'Lucida Console', 'Courier New', monospace");
    }

    public Selector fontFamilySans() {
        return fontFamily("'Verdana', 'DejaVu Sans', 'Tahoma', 'Arial', sans");
    }

    public final Selector<TTag> fontSize(final String value) {
        return setStyle("font-size", value);
    }

    public final Selector<TTag> fontStyle(final String value) {
        return setStyle("font-style", value);
    }

    public final Selector<TTag> fontVariant(final String value) {
        return setStyle("font-variant", value);
    }

    public final Selector<TTag> fontWeight(final FontWeight value) {
        return setStyle("font-weight", getValue(value));
    }

    public final Selector<TTag> fontFace(final String value) {
        return setStyle("@font-face", value);
    }

    public final Selector<TTag> fontSizeAdjust(final String value) {
        return setStyle("font-size-adjust", value);
    }

    public final Selector<TTag> fontStretch(final String value) {
        return setStyle("font-stretch", value);
    }

    public final Selector<TTag> gridColumns(final String value) {
        return setStyle("grid-columns", value);
    }

    public final Selector<TTag> gridRows(final String value) {
        return setStyle("grid-rows", value);
    }

    public final Selector<TTag> hangingPunctuation(final String value) {
        return setStyle("hanging-punctuation", value);
    }

    public final Selector<TTag> height(final String value) {
        return setStyle("height", value);
    }

    public final Selector<TTag> icon(final String value) {
        return setStyle("icon", value);
    }

    public final Selector<TTag> keyframes(final String value) {
        return setStyle("@keyframes", value);
    }

    public final Selector<TTag> left(final String value) {
        return setStyle("left", value);
    }

    public final Selector<TTag> letterSpacing(final String value) {
        return setStyle("letter-spacing", value);
    }

    public final Selector<TTag> lineHeight(final String value) {
        return setStyle("line-height", value);
    }

    public final Selector<TTag> listStyle(final String value) {
        return setStyle("list-style", value);
    }

    public final Selector<TTag> listStyleImage(final String value) {
        return setStyle("list-style-image", value);
    }

    public final Selector<TTag> listStylePosition(final String value) {
        return setStyle("list-style-position", value);
    }

    public final Selector<TTag> listStyleType(final String value) {
        return setStyle("list-style-type", value);
    }

    public final Selector<TTag> margin(final String value) {
        return setStyle("margin", value);
    }

    public final Selector<TTag> marginBottom(final String value) {
        return setStyle("margin-bottom", value);
    }

    public final Selector<TTag> marginLeft(final String value) {
        return setStyle("margin-left", value);
    }

    public final Selector<TTag> marginRight(final String value) {
        return setStyle("margin-right", value);
    }

    public final Selector<TTag> marginTop(final String value) {
        return setStyle("margin-top", value);
    }

    public final Selector<TTag> maxHeight(final String value) {
        return setStyle("max-height", value);
    }

    public final Selector<TTag> maxWidth(final String value) {
        return setStyle("max-width", value);
    }

    public final Selector<TTag> minHeight(final String value) {
        return setStyle("min-height", value);
    }

    public final Selector<TTag> minWidth(final String value) {
        return setStyle("min-width", value);
    }

    public final Selector<TTag> nav(final String value) {
        return setStyle("nav", value);
    }

    public final Selector<TTag> navDown(final String value) {
        return setStyle("nav-down", value);
    }

    public final Selector<TTag> navIndex(final String value) {
        return setStyle("nav-index", value);
    }

    public final Selector<TTag> navLeft(final String value) {
        return setStyle("nav-left", value);
    }

    public final Selector<TTag> navRight(final String value) {
        return setStyle("nav-right", value);
    }

    public final Selector<TTag> navUp(final String value) {
        return setStyle("nav-up", value);
    }

    public final Selector<TTag> opacity(final String value) {
        return setStyle("opacity", value);
    }

    public final Selector<TTag> outline(final String value) {
        return setStyle("outline", value);
    }

    public final Selector<TTag> outlineColor(final String value) {
        return setStyle("outline-color", value);
    }

    public final Selector<TTag> outlineColor(final Color value) {
        return setStyle("outline-color", value.name());
    }

    public final Selector<TTag> outlineOffset(final String value) {
        return setStyle("outline-offset", value);
    }

    public final Selector<TTag> outlineStyle(final String value) {
        return setStyle("outline-style", value);
    }

    public final Selector<TTag> outlineWidth(final String value) {
        return setStyle("outline-width", value);
    }

    public final Selector<TTag> overflow(final String value) {
        return setStyle("overflow", value);
    }

    public final Selector<TTag> overflowX(final String value) {
        return setStyle("overflow-x", value);
    }

    public final Selector<TTag> overflowY(final String value) {
        return setStyle("overflow-y", value);
    }

    public final Selector<TTag> padding(final String value) {
        return setStyle("padding", value);
    }

    public final Selector<TTag> paddingBottom(final String value) {
        return setStyle("padding-bottom", value);
    }

    public final Selector<TTag> paddingLeft(final String value) {
        return setStyle("padding-left", value);
    }

    public final Selector<TTag> paddingRight(final String value) {
        return setStyle("padding-right", value);
    }

    public final Selector<TTag> paddingTop(final String value) {
        return setStyle("padding-top", value);
    }

    public final Selector<TTag> pageBreak(final String value) {
        return setStyle("page-break", value);
    }

    public final Selector<TTag> pageBreakAfter(final String value) {
        return setStyle("page-break-after", value);
    }

    public final Selector<TTag> pageBreakBefore(final String value) {
        return setStyle("page-break-before", value);
    }

    public final Selector<TTag> pageBreakInside(final String value) {
        return setStyle("page-break-inside", value);
    }

    public final Selector<TTag> perspective(final String value) {
        return setStyle("perspective", value);
    }

    public final Selector<TTag> perspectiveOrigin(final String value) {
        return setStyle("perspective-origin", value);
    }

    public final Selector<TTag> position(final Position value) {
        return setStyle("position", getValue(value));
    }

    public final Selector<TTag> punctuationTrim(final String value) {
        return setStyle("punctuation-trim", value);
    }

    public final Selector<TTag> quotes(final String value) {
        return setStyle("quotes", value);
    }

    public final Selector<TTag> resize(final String value) {
        return setStyle("resize", value);
    }

    public final Selector<TTag> right(final String value) {
        return setStyle("right", value);
    }

    public final Selector<TTag> rotation(final String value) {
        return setStyle("rotation", value);
    }

    public final Selector<TTag> rotationPoint(final String value) {
        return setStyle("rotation-point", value);
    }

    public final Selector<TTag> tableLayout(final String value) {
        return setStyle("table-layout", value);
    }

    public final Selector<TTag> tabSize(final int value) {
        return setStyle("tab-size", Utils.toString(value));
    }

    public final Selector<TTag> target(final String value) {
        return setStyle("target", value);
    }

    public final Selector<TTag> targetName(final String value) {
        return setStyle("target-name", value);
    }

    public final Selector<TTag> targetNew(final String value) {
        return setStyle("target-new", value);
    }

    public final Selector<TTag> targetPosition(final String value) {
        return setStyle("target-position", value);
    }

    public final Selector<TTag> text(final String value) {
        return setStyle("text", value);
    }

    public final Selector<TTag> textAlign(final TextAlign value) {
        return setStyle("text-align", getValue(value));
    }

    public final Selector<TTag> textDecoration(final TextDecoration value) {
        return setStyle("text-decoration", getValue(value));
    }

    public final Selector<TTag> textIndent(final String value) {
        return setStyle("text-indent", value);
    }

    public final Selector<TTag> textJustify(final String value) {
        return setStyle("text-justify", value);
    }

    public final Selector<TTag> textOutline(final String value) {
        return setStyle("text-outline", value);
    }

    public final Selector<TTag> textOverflow(final String value) {
        return setStyle("text-overflow", value);
    }

    public final Selector<TTag> textShadow(final String value) {
        return setStyle("text-shadow", value);
    }

    public final Selector<TTag> textTransform(final String value) {
        return setStyle("text-transform", value);
    }

    public final Selector<TTag> textWrap(final String value) {
        return setStyle("text-wrap", value);
    }

    public final Selector<TTag> top(final String value) {
        return setStyle("top", value);
    }

    public final Selector<TTag> transform(final String value) {
        setStyle("transform", value);
        return setStyle("-webkit-transform", value);
    }

    public final Selector<TTag> transformOrigin(final String value) {
        return setStyle("transform-origin", value);
    }

    public final Selector<TTag> transformStyle(final String value) {
        return setStyle("transform-style", value);
    }

    public final Selector<TTag> transition(final String value) {
        return setStyle("transition", value);
    }

    public final Selector<TTag> transitionProperty(final String value) {
        return setStyle("transition-property", value);
    }

    public final Selector<TTag> transitionDuration(final String value) {
        return setStyle("transition-duration", value);
    }

    public final Selector<TTag> transitionTimingFunction(final String value) {
        return setStyle("transition-timing-function", value);
    }

    public final Selector<TTag> transitionDelay(final String value) {
        return setStyle("transition-delay", value);
    }

    public final Selector<TTag> userSelect(final String value) {
        setStyle("-webkit-user-select", value, "text", "none");
        setStyle("-khtml-user-select", value, "text", "none");
        setStyle("-moz-user-select", value, "text", "none");
        setStyle("-o-user-select", value, "text", "none");
        return setStyle("-ms-user-select", value, "text", "none");
    }

    public final Selector<TTag> verticalAlign(final String value) {
        return setStyle("vertical-align", value);
    }

    public final Selector<TTag> verticalAlign(final VerticalAlign value) {
        return setStyle("vertical-align", getValue(value));
    }

    public final Selector<TTag> visibility(final String value) {
        return setStyle("visibility", value);
    }

    public final Selector<TTag> width(final String value) {
        return setStyle("width", value);
    }

    public final Selector<TTag> whiteSpace(final WhiteSpace value) {
        return setStyle("white-space", getValue(value));
    }

    public final Selector<TTag> wordSpacing(final String value) {
        return setStyle("word-spacing", value);
    }

    public final Selector<TTag> wordBreak(final String value) {
        return setStyle("word-break", value);
    }

    public final Selector<TTag> wordWrap(final String value) {
        return setStyle("word-wrap", value);
    }

    public final Selector<TTag> zIndex(final String value) {
        return setStyle("z-index", value);
    }

    //=====================================================================================
    public static String gradient(UserAgent ua, String from, String to) {
        if (ua != null) {
            Browser br = ua.browser.getGroup();

            if (br == Browser.CHROME || br == Browser.SAFARI)
                return "-webkit-linear-gradient(" + from + ", " + to + ")";

            if (br == Browser.FIREFOX)
                return "-moz-linear-gradient(top, " + from + ", " + to + ")";

            if (br == Browser.OPERA)
                return "-o-linear-gradient(" + from + ", " + to + ")";

            if (br == Browser.IE)
                return "\"progid:DXImageTransform.Microsoft.gradient(startColorstr='"
                        + from + "', endColorstr='" + to + "')\"";
        }
        return "linear-gradient(" + from + ", " + to + ")";

    }

    public Selector gradientBackground(UserAgent ua, String from, String to) {
        String val = gradient(ua, from, to);

        if (ua != null && ua.browser.getGroup() == Browser.IE)
            add("-ms-filter", val);
        else
            backgroundImage(val);

        return this;
    }

    // zmiania nazwy np z inlineBlock na inline-block
    private String getValue(Enum _enum) {
        if (_enum == null)
            return null;

        String val = _enum.toString();

        StrWriter wr = new StrWriter();

        for (char c : val.toCharArray()) {

            if (c == '_')
                continue;

            if (Character.isUpperCase(c))
                wr.append("-").append(Character.toLowerCase(c));
            else
                wr.append(c);
        }
        return wr.toString();
    }

}
