package com.html.core.tag.intfs;

import com.html.core.styles.TextAlign;
import com.html.core.tag.*;
import com.html.core.tag.form.*;
import com.html.core.tag.form.input.*;
import com.html.core.tag.formatting.*;
import com.html.core.tag.image.Canvas;
import com.html.core.tag.image.Img;
import com.html.core.tag.list.Ol;
import com.html.core.tag.list.Ul;
import com.html.core.tag.programming.Script;
import com.html.core.tag.programming.TObject;
import com.html.core.tag.semantic.*;
import com.html.core.tag.table.Table;
import com.html.js.core.JsAction;
import com.resources.awesome.*;
import com.resources.core.html.ImgFile;
import com.utils.Url;

public interface Container<TTag extends Element>
        extends Tag<TTag>, Parent<TTag>, Visual<TTag>, InnerText<TTag> {

    default A a() {
        return new A(this);
    }

    default A a(Object innerText) {
        return new A(this).text(innerText);
    }

    default B b() {
        return new B(this);
    }

    default B b(Object innerText) {
        return new B(this).text(innerText);
    }

    default Br br() {
        return new Br(this);
    }

    default Div center() {
        return new Div(this).style().textAlign(TextAlign.center).tag();
    }

    default Button button() {
        return new Button(this);
    }

    default Button button(Object innerText) {
        return new Button(this).text(innerText);
    }

    default ImageButton imageButton(Url imgSrc, Object innerText) {
        ImageButton btn = new ImageButton(this, imgSrc);
        btn.label.text(innerText);
        return btn;
    }

    default ImageButton imageButton(ImgFile img, Object innerText) {
        ImageButton btn = new ImageButton(this, img);
        btn.label.text(innerText);
        return btn;
    }

    default AwesomeIcon awesomeIcon(Awesome icon, FaSize size) {
        return new AwesomeIcon(this, icon, size);
    }

    default AwesomeStack awesomeStack(FaSize size) {
        return new AwesomeStack(this, size);
    }

    default DataList dataList() {
        return new DataList(this);
    }

    default I i() {
        return new I(this);
    }

    default I i(Object innerText) {
        return new I(this).text(innerText);
    }

    default Img img() {
        return new Img(this);
    }

    default Img img(ImgFile img) {
        return new Img(this).src(img.href);
    }

    default InputButton inputButton() {
        return new InputButton(this);
    }

    default InputHidden inputHidden() {
        return new InputHidden(this);
    }

    default InputCheckBox inputCheckBox() {
        return new InputCheckBox(this);
    }

    default InputFile inputFile() {
        return new InputFile(this);
    }

    default InputNumber inputNumber() {
        return new InputNumber(this);
    }

    default InputImage inputImage() {
        return new InputImage(this);
    }

    default InputPassword inputPassword() {
        return new InputPassword(this);
    }

    default InputRadio inputRadio() {
        return new InputRadio(this);
    }

    default InputSubmit inputSubmit() {
        return new InputSubmit(this);
    }

    default InputText inputText() {
        return new InputText(this);
    }

    default Select select() {
        return new Select(this);
    }

    default Span span() {
        return new Span(this);
    }

    default Span span(Object innerText) {
        return new Span(this).text(innerText);
    }

    default TextArea textArea() {
        return new TextArea(this);
    }

    default TextArea textArea(Object innerText) {
        return new TextArea(this).text(innerText);
    }

    default TObject object() {
        return new TObject(this);
    }

    default Canvas canvas() {
        return new Canvas(this);
    }

    default Div div() {
        return new Div(this);
    }

    default Aside aside() {
        return new Aside(this);
    }

    default Div div(Object innerText) {
        return new Div(this).text(innerText);
    }

    default Fieldset fieldset(Object label) {
        return new Fieldset(this, label);
    }

    default Form form() {
        return new Form(this);
    }

    default H1 h1() {
        return new H1(this);
    }

    default H1 h1(Object innerText) {
        return new H1(this).text(innerText);
    }

    default H2 h2() {
        return new H2(this);
    }

    default H2 h2(Object innerText) {
        return new H2(this).text(innerText);
    }

    default H3 h3() {
        return new H3(this);
    }

    default H3 h3(Object innerText) {
        return new H3(this).text(innerText);
    }

    default H4 h4() {
        return new H4(this);
    }

    default H4 h4(Object innerText) {
        return new H4(this).text(innerText);
    }

    default H5 h5() {
        return new H5(this);
    }

    default H5 h5(Object innerText) {
        return new H5(this).text(innerText);
    }

    default H6 h6() {
        return new H6(this);
    }

    default H6 h6(Object innerText) {
        return new H6(this).text(innerText);
    }

    default Hr hr() {
        return new Hr(this);
    }

    default Ol ol() {
        return new Ol(this);
    }

    default P p() {
        return new P(this);
    }

    default P p(Object innerText) {
        return new P(this).text(innerText);
    }

    default Table table() {
        return new Table(this);
    }

    default Ul ul() {
        return new Ul(this);
    }

    default Iframe iframe() {
        return new Iframe(this);
    }

    default Iframe iframe(Url src) {
        return new Iframe(this).src(src);
    }

    default Script script() {
        return new Script(this);
    }

    default Script script(String content) {
        return new Script(this).text(content);
    }

    default Script script(JsAction... actions) {
        Script script = new Script(this);
        script.new Helper().setInnerActions(actions);
        return script;
    }

    default Article article() {
        return new Article(this);
    }

    default Header header() {
        return new Header(this);
    }

    default Header header(Object innerText) {
        return new Header(this).text(innerText);
    }

    default Footer footer() {
        return new Footer(this);
    }

    default Footer footer(Object innerText) {
        return new Footer(this).text(innerText);
    }

    default Code code() {
        return new Code(this);
    }

    default Code code(Object innerText) {
        return new Code(this).text(innerText);
    }

    default Blockquote blockquote() {
        return new Blockquote(this);
    }

    default Blockquote blockquote(Object innerText) {
        return new Blockquote(this).text(innerText);
    }

    default U u() {
        return new U(this);
    }

    default U u(Object innerText) {
        return new U(this).text(innerText);
    }

    default Del del() {
        return new Del(this);
    }

    default Del del(Object innerText) {
        return new Del(this).text(innerText);
    }

    default Q q() {
        return new Q(this);
    }

    default Q q(Object innerText) {
        return new Q(this).text(innerText);
    }

    default Pre pre() {
        return new Pre(this);
    }

    default Pre pre(Object innerText) {
        return new Pre(this).text(innerText);
    }

    default Small small() {
        return new Small(this);
    }

    default Small small(Object innerText) {
        return new Small(this).text(innerText);
    }

    default Nav nav() {
        return new Nav(this);
    }

    default Meter meter() {
        return new Meter(this);
    }

    default Progress progress() {
        return new Progress(this);
    }

    default Strong strong() {
        return new Strong(this);
    }

    default Strong strong(Object innerText) {
        return new Strong(this).text(innerText);
    }

    default Sub sub() {
        return new Sub(this);
    }

    default Sub sub(Object innerText) {
        return new Sub(this).text(innerText);
    }

    default Sup sup() {
        return new Sup(this);
    }

    default Sup sup(Object innerText) {
        return new Sup(this).text(innerText);
    }

}
