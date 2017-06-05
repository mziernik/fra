package com.script.evaluator.element;

import com.json.Escape;
import com.script.evaluator.EvaluationException;
import com.script.evaluator.Evaluator;
import com.script.evaluator.Pos;
import static com.script.evaluator.element.Element.BLOCKS;
import com.script.evaluator.operator.Operator;
import com.utils.Char;
import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.Objects;

public abstract class ECollection extends Element
        implements Iterable<Element> {

    public final LinkedList<Element> children = new LinkedList<>();

    public ECollection(ECollection parent, Pos pos) {
        super(parent, pos);
    }

    @Override
    public Iterator<Element> iterator() {
        return children.iterator();
    }

    public <T extends Element> T add(T element) {
        element.parent = this;
        children.add(element);
        return element;
    }

    @Override
    public void getContent(StrWriter writer) {

        if (this instanceof EBlock || this instanceof EObject || this == evaluator) {

            if (this != evaluator)
                writer.append("{");

            writer.nextLevel(() -> {
                boolean first = true;

                for (Element el : children) {
                    if (!first)
                        writer.append(";").br().intent();
                    el.getContent(writer);
                    first = false;
                }
            });
            if (this != evaluator)
                writer.br().intent().append("}");
            return;
        }

        boolean first = true;
        for (Element el : children) {
            if (!first)
                writer.append(", ");
            first = false;
            el.getContent(writer);
        }
    }

    Evaluator process() {
        processValues();
        splitExpressions();

        //  groupKeywords();
        processUnaryOperands();
        joinIntoArrays();
        groupByOperands();
        groupCalls();
        removeSeparators();
        /*
        //----------------------------------------------------------------------
        // jeśli grupa zawiera jeden element to usuwamy grupę a element przenosimy na jeje miejsce
        if (this instanceof EGroup && children.size() == 1) {
            Element el = children.get(0);

            el.parent = this;

            if (el instanceof EGroup) {
                children.clear();
                children.addAll(((EGroup) el).children);

                return evaluator;
            }

            int idx = parent.children.indexOf(this);

            parent.children.remove(idx);
            parent.children.add(idx, el);

            if (el instanceof ECollection)
                ((ECollection) el).process();
            return evaluator;
        }
         */

        //----------------------------------------------------------------------
        for (Element el : new ArrayList<>(children))
            if (el instanceof ECollection)
                ((ECollection) el).process();

        //----------- trimowanie: usuwanie zbędnego opakowania w grupy ---------
        {
            ArrayList<Element> list = new ArrayList<>(children);
            children.clear();

            for (Element el : list) {
                while (el instanceof EGroup && ((EGroup) el).children.size() == 1)
                    el = ((EGroup) el).children.get(0);

                children.add(el);
            }
        }

        return evaluator;
    }

    private void splitExpressions() {
        // podziel na wyrażenia

        if (children.isEmpty())
            return;

        LinkedList<Element> list = new LinkedList<>(children);
        ArrayList<Element> items = new ArrayList<>();
        children.clear();

        for (Element item : list) {
            if (item instanceof ESeparator && ((ESeparator) item).isNewLine()) {

                if (!items.isEmpty())
                    if (items.size() > 1)
                        add(new EGroup(this, item.pos)).children.addAll(items);
                    else
                        children.add(items.get(0));

                items.clear();
                continue;
            }
            items.add(item);

        }

        if (children.isEmpty()) {
            children.addAll(items);
            return;
        }

        if (!items.isEmpty())
            if (items.size() > 1)
                add(new EGroup(this, pos)).children.addAll(items);
            else
                children.add(items.get(0));

    }

    // konwersja EKeyword do innych obiektów
    private void processValues() {
        Element[] items = children.toArray(new Element[0]);
        children.clear();

        for (int i = 0; i < items.length; i++) {
            Element item = items[i];
            Element prev = i > 0 ? items[i - 1] : null;
            Element second = i < items.length - 1 ? items[i + 1] : null;
            Element third = i < items.length - 2 ? items[i + 2] : null;

            if (item instanceof EKeyword) {
                EKeyword kwd = (EKeyword) item;
                String value = kwd.value();

                if (kwd.isString) {
                    add(new EValue(this, Escape.unescape(item.sb.toString()), kwd.pos));
                    continue;
                }

                char c = value.charAt(0);
                if (c >= '0' && c <= '9') {
                    Object val = value.startsWith("0x") ? Long.parseUnsignedLong(value, 16) : null;
                    val = val != null && value.contains(".") ? new Double(value) : null;
                    if (val == null)
                        val = new Long(value);
                    add(new EValue(this, val, kwd.pos));
                    continue;
                }
                for (String s : evaluator.keywords)
                    if (value.equals(s)) {
                        children.add(item);
                        item = null;
                        break;
                    }

                if (item == null)
                    continue;

                if (evaluator.constants.containsKey(value)) {
                    add(new EValue(this, evaluator.constants.get(value),
                            kwd.pos)).constName = value;
                    continue;
                }

                if (value.equals("if")
                        && second != null
                        && second instanceof EGroup
                        && third != null
                        && third instanceof EBlock) {

                    i += 2;
                    add(new EIf(this, (EGroup) second, (EBlock) third, kwd.pos));
                    continue;
                }

                if (second instanceof EGroup) {
                    add(new EFunction(this, value, (EGroup) second, pos));
                    i++;
                    continue;
                }

                add(new EValue(this, null, kwd.pos)).varName = value;
                continue;
            }

            // zamień bloki na obiekty
            if (item instanceof EBlock) {
                add(new EObject(this, pos)).children.addAll(((EBlock) item).children);
                continue;
            }

            children.add(item);
        }

    }

    private void groupKeywords() {
        // ------------ etap 1: grupowanie słów kluczowych z pozostałymi elementami
        LinkedList<Element> list = new LinkedList<>(children);
        children.clear();

        while (!list.isEmpty()) {
            Element t = list.pollFirst();

            //   if (t.type != Type.KEYWORD) {
            if (!(t instanceof EKeyword)) {
                children.add(t);
                continue;
            }

            EGroup tk = add(new EGroup(this, null));
            tk.add(t);
            while (list.peekFirst() != null && list.peekFirst() instanceof EGroup)
                tk.children.add(list.pollFirst());

            if (list.peekFirst() != null)
                tk.children.add(list.pollFirst());

        }
    }

    private void processUnaryOperands() {
        //------------- etap 2: grupowanie operatorów pojedynczych, np (-1)
        LinkedList<Element> list = new LinkedList<>(children);
        children.clear();

        while (!list.isEmpty()) {

            Element val = list.pollLast();
            Element opr = list.pollLast();
            Element pre = list.peekLast();

            if (opr != null && opr instanceof EOperator
                    && (pre == null || pre instanceof EOperator)) {
                list.add(new EOperand(this, null, ((EOperator) opr).operator, val, null));
                continue;
            }

            children.add(0, val);
            if (opr != null)
                children.add(0, opr);
        }
    }

    private void joinIntoArrays() {
        if (!(this instanceof EArray)
                && !(this instanceof EFunction)
                && !(this instanceof EObject)) {

            //------------- etap 3: grupowanie argumentów, które nie są oddzielone operatorami w tablice
            Element[] arr = children.toArray(new Element[0]);
            children.clear();

            for (int i = 0; i < arr.length; i++) {

                Element prev = i > 0 ? arr[i - 1] : null;
                Element current = arr[i];
                Element next = i < arr.length - 1 ? arr[i + 1] : null;

                if (prev != null
                        && current != null
                        && next != null
                        && !(current instanceof EGroup)
                        && !(next instanceof EGroup)
                        && !(current instanceof ESeparator)
                        && !(next instanceof ESeparator)
                        && !(current instanceof EOperator)
                        && !(next instanceof EOperator)) {

                    EArray tt = add(new EArray(this, null));
                    tt.add(current);
                    tt.add(next);

                    for (int j = i + 2; j < arr.length; j++) {
                        if (arr[j] instanceof EOperator)
                            break;
                        i = j;
                        tt.children.add(arr[j]);
                    }
                    continue;
                }

                add(current);

            }

            //      TConsole.print("3. " + this);
        }

    }

    private void groupByOperands() {
        // ------------ etap 5: podziel na operandy --------------------------
        for (int priority = 7; priority >= 0; priority--) {

            Element[] arr = children.toArray(new Element[0]);

            for (int i = 0; i < arr.length; i++) {

                Element prev = i > 0 ? arr[i - 1] : null;
                Element current = Objects.requireNonNull(arr[i]);
                Element next = i < arr.length - 1 ? arr[i + 1] : null;

                if (current instanceof ESeparator || next instanceof ESeparator)
                    continue;

                if (current instanceof EOperator) {
                    Operator opr = ((EOperator) current).operator;
                    if (opr.priority != priority)
                        continue;

                    if (prev == null || next == null)
                        throw new EvaluationException(((EOperator) current).pos,
                                "Incorrect expression \"s\"", toString());

                    EOperand opnd = new EOperand(this, prev, opr, next, null);

                    children.remove(prev);
                    children.remove(current);
                    children.remove(next);

                    //   if (i < children.size())
                    children.add(i > 0 ? i - 1 : 0, opnd);
                    // else
                    //   children.add(opnd);

                    arr = children.toArray(new Element[0]);
                    --i;
                }
            }

            //      TConsole.print("5. " + this);
        }
    }

    public Evaluator parse(String file, String expression) {
        if (!children.isEmpty() || Is.empty(expression))
            return evaluator;

        ECollection parent = this;
        Element current = null;

        String[] lines = expression.split("\n");

        final LinkedList<char[]> blocks = new LinkedList<>();

        char comment = 0;

        for (int lineNumber = 1; lineNumber <= lines.length; lineNumber++) {
            char[] chars = lines[lineNumber - 1].toCharArray();

            char qutoted = 0; // pojedynczy lub podwójny cudzysłów

            if (lineNumber > 1)
                parent.add(new ESeparator(parent, '\n', new Pos(file, lineNumber, 1)));

            if (current instanceof EKeyword)
                current = null;

            for (int charNumber = 1; charNumber <= chars.length; charNumber++) {
                char c = chars[charNumber - 1];

                Pos pos = new Pos(file, lineNumber, charNumber);

                char next = charNumber < chars.length ? chars[charNumber] : 0;
                char prev = charNumber > 1 ? chars[charNumber - 2] : 0;

                // początek komentarza /*
                if (qutoted == 0 && c == '/' && next == '*') {
                    comment = '*';
                    ++charNumber;
                    continue;
                }

                // początek komentarza //
                if (qutoted == 0 && comment == 0 && c == '/' && next == '/') {
                    comment = '/';
                    current = null;
                    ++charNumber;
                    continue;
                }

                // początek komentarza #
                if (qutoted == 0 && comment == 0 && c == '#') {
                    comment = '#';
                    continue;
                }

                // koniec komentarza */
                if (qutoted == 0 && c == '*' && next == '/' && comment == '*') {
                    charNumber++;
                    comment = 0;
                    current = null;
                    continue;
                }

                if (comment != 0)
                    continue;

                // początek string-a
                if (qutoted == 0 && (c == '\'' || c == '"')) {
                    qutoted = c;
                    current = parent.add(new EKeyword(parent, pos));
                    current.isString = true;
                    continue;
                }

                // koniec string-a
                if (qutoted != 0 && prev != '\\' && c == qutoted) {
                    qutoted = 0;
                    current = null;
                    continue;
                }

                if (qutoted != 0) {
                    current.sb.append(c);
                    continue;
                }

                if (Char.isWhiteChar(c)) {
                    current = null;
                    continue;
                }

                if (c < 32)
                    throw new EvaluationException(pos, "Incorrect char \"%s\"", c);

                if (c > 127)
                    throw new EvaluationException(pos, "Incorrect char \"%s\"", c);

                for (char[] cc : BLOCKS) {

                    if (c == cc[0]) {
                        blocks.add(cc);
                        current = null;
                        parent = c == '{' ? parent.add(new EBlock(parent, pos))
                                : c == '[' ? parent.add(new EArray(parent, pos))
                                        : c == '(' ? parent.add(new EGroup(parent, pos))
                                                : null;
                        c = 0;
                        break;
                    }

                    if (c == cc[1]) {
                        char[] bb = blocks.pollLast();
                        if (bb != cc)
                            throw new EvaluationException(pos, "Unexpected char \"%s\"", c);
                        c = 0;
                        parent = parent.parent; // przywróć poprzedniego rodzica
                        current = null;
                        break;
                    }

                }

                if (c == 0)
                    continue;

                switch (c) {

                    case ';':
                    case ',':
                        parent.add(new ESeparator(parent, c, pos));
                        current = null;
                        continue;
                }

                for (Operator opr : evaluator.operators.values()) {
                    boolean var = true;
                    char[] symbol = opr.symbol.toCharArray();
                    int i = charNumber - 1;
                    for (int j = 0; j < symbol.length; j++)
                        var &= i + j < chars.length && chars[i + j] == symbol[j];

                    if (var) {
                        parent.add(new EOperator(parent, opr, pos));
                        current = null;
                        charNumber += opr.symbol.length() - 1;
                        c = 0;
                        break;
                    }
                }

                if (c == 0)
                    continue;

                if (current == null)
                    current = parent.add(new EKeyword(parent, pos));

                current.sb.append(c);
            }

            if (comment == '#' || comment == '/')
                comment = 0;

        }

        process();
        return evaluator;
    }

    private void removeSeparators() {
        LinkedList<Element> list = new LinkedList<>(children);
        children.clear();

        for (Element el : list)
            if (!(el instanceof ESeparator))
                children.add(el);
    }

    private void groupCalls() {
        Element[] arr = children.toArray(new Element[0]);

        for (int i = 0; i < arr.length; i++) {

            Element prev = i > 0 ? arr[i - 1] : null;
            Element current = Objects.requireNonNull(arr[i]);
            Element next = i < arr.length - 1 ? arr[i + 1] : null;
        }
    }
}
