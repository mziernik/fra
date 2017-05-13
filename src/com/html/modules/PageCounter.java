package com.html.modules;

import com.database.Database;
import com.config.CContent;
import com.html.core.styles.FontWeight;
import com.html.core.tag.A;
import com.html.core.tag.intfs.Container;
import com.html.core.tag.semantic.Div;
import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;
import com.utils.Url;
import java.sql.SQLException;

public class PageCounter {

    public Integer pageCount;
    public final int resultsPerPage;
    public final int totalResults;
    public final int currentPage;
    //---------------------------------
    public int maxNumbersPerLine = 19; // ilosc numerow stron na linie (najlepiej wartosc nieparzysta)
    public String cssClass = "page-counter";
    public String emptyText = "- Brak wyników -";
    public final Url href;
    public String pageParamName = "page";
    //---------------------------------
    public final boolean knowPageCount;

    public PageCounter(Page page, Database db, String table) throws SQLException {
        href = new Url(page.request.url.toString());

        int total = db.execute("SELECT count(1) FROM " + table).first().getInt(0);
        int current = page.params.getInt("page", 1);

        this.totalResults = total;
        this.resultsPerPage = CContent.maxTableResults.value();

        int cnt = (int) Math.ceil((double) totalResults / (double) resultsPerPage);
        if (cnt < 0)
            cnt = 0;

        pageCount = cnt;
        if (current > cnt)
            current = cnt;
        if (current <= 0)
            current = 1;

        this.currentPage = current;
        knowPageCount = true;
    }

    public PageCounter(int currentPage, final Integer totalResults) {
        HttpRequest req = HttpRequest.getInstance();
        href = req != null ? new Url(req.url) : new Url("?");

        this.resultsPerPage = CContent.maxTableResults.value();

        if (totalResults == null) {
            this.totalResults = 0;
            this.currentPage = currentPage;
            if (pageCount == null || pageCount < currentPage)
                pageCount = currentPage + 1;
            knowPageCount = false;

        } else {
            this.totalResults = totalResults;

            int cnt = (int) Math.ceil((double) totalResults / (double) resultsPerPage);
            if (cnt < 0)
                cnt = 0;

            pageCount = cnt;

            if (currentPage > cnt)
                currentPage = cnt;
            if (currentPage <= 0)
                currentPage = 1;

            this.currentPage = currentPage;
            knowPageCount = true;
        }
    }

    private A addNr(Container dPage, int i) {
        return addNr(dPage, String.valueOf(i));
    }

    private A addNr(Container dPage, String i) {
        String txt = i;
        if (i.equals("Dalej"))
            i = String.valueOf(currentPage + 1);

        A a = dPage.a(txt);

        Url url = new Url(href);
        url.params().remove(pageParamName);

        a.href(url.param(pageParamName, i));
        if (i.equals(String.valueOf(currentPage))) {
            a.text("[" + a.getText() + "]");
            a.style().fontWeight(FontWeight.bold);
        }
        return a;
    }

    public Div buildCounter(Container tag) {
        Div dPage = tag.div();
        dPage.cls(cssClass);

        if (pageCount == 0) {
            dPage.span().text(emptyText);
            return dPage;
        }

        dPage.span().text("Strona: ");

        if (pageCount <= maxNumbersPerLine)
            if (knowPageCount)
                for (int i = 1; i <= pageCount; i++)
                    addNr(dPage, i);
            else {
                for (int i = 1; i < pageCount; i++)
                    addNr(dPage, i);
                addNr(dPage, "Dalej");
            }
        else {
            int from = 2;
            int to = maxNumbersPerLine - 3;

            int center;
            while ((center = from + (to - from) / 2) != currentPage
                    && to < pageCount
                    && from > 1)
                if (center < currentPage) {
                    ++from;
                    ++to;
                } else {
                    --from;
                    --to;
                }

            if (from > 1) {
                addNr(dPage, 1);
                dPage.span().text("…");
            }

            for (int i = from; i <= to; i++)
                addNr(dPage, i);

            if (to < pageCount) {
                dPage.span().text("…");
                addNr(dPage, pageCount);
            }
        }
        return dPage;
    }

    public String sqlLimitQuery() {
        return "LIMIT " + resultsPerPage + " OFFSET " + ((currentPage - 1) * resultsPerPage);
    }
}
