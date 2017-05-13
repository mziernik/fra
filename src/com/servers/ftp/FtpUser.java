package com.servers.ftp;

import com.utils.collections.TList;
import java.util.List;
import org.apache.ftpserver.ftplet.Authority;
import org.apache.ftpserver.ftplet.AuthorizationRequest;
import org.apache.ftpserver.ftplet.User;
import org.apache.ftpserver.usermanager.impl.WritePermission;

public class FtpUser implements User {

    private final String name;

    public FtpUser(String name) {
        this.name = name;
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public String getPassword() {
        return null;
    }

    @Override
    public List<? extends Authority> getAuthorities() {
        TList<Authority> list = new TList<Authority>();
        list.add(new WritePermission());
        return list;
    }

    @Override
    public List<? extends Authority> getAuthorities(Class<? extends Authority> type) {
        TList<Authority> list = new TList<Authority>();
        list.add(new WritePermission());
        return list;
    }

    @Override
    public AuthorizationRequest authorize(AuthorizationRequest ar) {
        return ar;
    }

    @Override
    public int getMaxIdleTime() {
        return -1;
    }

    @Override
    public boolean getEnabled() {
        return true;
    }

    @Override
    public String getHomeDirectory() {
        return "/";
    }

}
