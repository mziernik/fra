package com.servlet.requests;

import java.io.IOException;
import javax.servlet.*;

/**
 * @author Miłosz Ziernik
 * @date 27 października 2015
 * @encoding UTF-8
 */
public class RequestFilter {

    public void doFilter(ServletRequest sr, ServletResponse sr1, FilterChain filterChain)
            throws IOException, ServletException {
        filterChain.doFilter(sr, sr1);
    }
}
