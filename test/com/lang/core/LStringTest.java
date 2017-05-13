/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.lang.core;

import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.webapi.core.WebApiRequest;
import java.util.stream.IntStream;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.*;

/**
 *
 * @author milosz
 */
public class LStringTest {
    

    @Test
    public void testGet() {
  
        String toString = new LStr("p %% z %1x").toString("1");
       
        
        System.out.println(toString);
        
    }

    
}
