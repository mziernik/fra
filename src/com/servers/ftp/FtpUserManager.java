/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package com.servers.ftp;

import org.apache.ftpserver.ftplet.*;

/**
 *
 * @author milosz
 */
public class FtpUserManager implements UserManager {

    @Override
    public User getUserByName(String string) throws FtpException {
        return new FtpUser(string);
    }

    @Override
    public String[] getAllUserNames() throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void delete(String string) throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public void save(User user) throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean doesExist(String string) throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public User authenticate(Authentication a) throws AuthenticationFailedException {
        return new FtpUser("anonymous");
    }

    @Override
    public String getAdminName() throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    @Override
    public boolean isAdmin(String string) throws FtpException {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}
