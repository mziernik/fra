package com.servlet.interfaces;

import com.cache.CachedData;
import javax.servlet.http.Part;

public interface UploadTarget {

    // jesli part jest null-em, znaczy to, że żądanie zostało wysłane ajax-em
    public void onUploadFile(CachedData file, String fieldName, Part part) throws Exception;

}
