package com.database;

@FunctionalInterface
public interface Transaction {

    public void run(Database db) throws Exception;
}
