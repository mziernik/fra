CREATE SCHEMA IF NOT EXISTS users;


CREATE TABLE users.user_status(
    key VARCHAR(30) PRIMARY KEY NOT NULL, 
    name VARCHAR(100) UNIQUE NOT NULL
);

INSERT INTO users.user_status (key, name) 
VALUES 
    ('R', 'Usunięty'),
    ('D', 'Nieaktywny'),
    ('A', 'Aktywny'),
    ('V', 'Weryfikacja e-mail'),
    ('C', 'Weryfikacja administratora');

--------------------------------------------------------------------------------

CREATE TABLE users.user_type(
    key VARCHAR(30) PRIMARY KEY NOT NULL, 
    name VARCHAR(100) UNIQUE NOT NULL
);

INSERT INTO users.user_type (key, name) 
VALUES 
    ('N', 'Standardowy'),
    ('V', 'Wirtualny'),
    ('A', 'API');

--------------------------------------------------------------------------------

CREATE TABLE users.group_type(
    key VARCHAR(30) PRIMARY KEY NOT NULL, 
    name VARCHAR(100) UNIQUE NOT NULL
);

INSERT INTO users.group_type (key, name) 
VALUES 
    ('N', 'Standardowa');

--------------------------------------------------------------------------------

CREATE TABLE users.users (
    user_id INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT, 
    external_token VARCHAR(100) UNIQUE, 
    username VARCHAR(100) UNIQUE NOT NULL, 
    display_name VARCHAR(200) NOT NULL, 
    password VARCHAR(100), 
    status VARCHAR(30) NOT NULL DEFAULT('A') REFERENCES users.user_status (key), 
    type CHAR NOT NULL DEFAULT('N') REFERENCES users.user_type (key),
    created TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP), 
    created_by VARCHAR(100), 
    email VARCHAR(100) UNIQUE, 
    first_name VARCHAR(100), 
    last_name VARCHAR(100), 
    ldap_auth BOOLEAN DEFAULT false, 
    password_expire TIMESTAMP, 
    password_changed TIMESTAMP, 
    last_login TIMESTAMP, 
    config VARCHAR
);

--------------------------------------------------------------------------------

CREATE TABLE users.rights(
    key VARCHAR(100) NOT NULL PRIMARY KEY, 
    name VARCHAR(300) NOT NULL UNIQUE, 
    description VARCHAR, 
    parent_right VARCHAR(100) REFERENCES users.rights (key)
);

--------------------------------------------------------------------------------

CREATE TABLE users.groups (
    key VARCHAR(100) NOT NULL PRIMARY KEY, 
    type CHAR NOT NULL  DEFAULT('N') REFERENCES users.group_type (key),   
    name VARCHAR NOT NULL, 
    description VARCHAR,
    embedded BOOLEAN NOT NULL, 
    enabled BOOLEAN NOT NULL DEFAULT true
);

--------------------------------------------------------------------------------

CREATE TABLE users.group_rights (
    group_key VARCHAR(100) NOT NULL REFERENCES users.groups (key), 
    right_key VARCHAR(100) NOT NULL REFERENCES users.rights (key), 
    refused BOOLEAN NOT NULL DEFAULT false,
    CONSTRAINT group_rights_pkey PRIMARY KEY (group_key, right_key)
);

--------------------------------------------------------------------------------

CREATE TABLE users.user_rights (
    user_id INTEGER NOT NULL REFERENCES users.users (user_id), 
    right_key VARCHAR(100) REFERENCES users.rights (key), 
    refused BOOLEAN NOT NULL DEFAULT false,
    CONSTRAINT user_rights_pkey PRIMARY KEY (user_id, right_key)
);

--------------------------------------------------------------------------------

CREATE TABLE users.user_groups (
    user_id INTEGER NOT NULL REFERENCES users.users (user_id), 
    group_key VARCHAR(100) NOT NULL REFERENCES users.groups (key),
    CONSTRAINT user_groups_pkey PRIMARY KEY (user_id, group_key)
);

--------------------------------------------------------------------------------

CREATE TABLE users.user_attributes (
    user_id INTEGER NOT NULL REFERENCES users.users (user_id), 
    key VARCHAR(100) NOT NULL,
    name VARCHAR(200) NOT NULL,
    value VARCHAR,
    CONSTRAINT user_attributes_pkey PRIMARY KEY (user_id, key)
);


--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

DROP TABLE IF EXISTS sessions;

CREATE TABLE sessions (
    session_id VARCHAR (32) NOT NULL PRIMARY KEY, 
    user_id INTEGER, -- tu nie może być klucza obcego
    user_pass VARCHAR(100),
    user_name VARCHAR,
    created TIMESTAMP DEFAULT CURRENT_TIMESTAMP NOT NULL, 
    expires INTEGER, 
    user_agent VARCHAR (200), 
    address VARCHAR (50), 
    destroyed TIMESTAMP, 
    alive TIMESTAMP
);


--------------------------------------------------------------------------------

DROP TABLE IF EXISTS local_storage;

CREATE TABLE local_storage (
    local_storage_id INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,  
    encrypted BOOLEAN NOT NULL, 
    key VARCHAR(100) NOT NULL UNIQUE, 
    value VARCHAR, 
    date TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP), 
);


--------------------------------------------------------------------------------

DROP TABLE IF EXISTS meta_data;

CREATE TABLE meta_data (
    key VARCHAR(100) NOT NULL PRIMARY KEY,
    value VARCHAR NOT NULL,
    last_modified TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP), 
);


DROP TABLE IF EXISTS config;

CREATE TABLE config (
    config_id INTEGER NOT NULL PRIMARY KEY AUTO_INCREMENT,
    key VARCHAR(200) UNIQUE NOT NULL,
    value VARCHAR, -- JSON
    variable VARCHAR,
    default BOOLEAN NOT NULL,
    last_modified TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP)
);


--insert into users._roles(key, name, parent_role_id) values ('2323', 'fd343', 1)

--SELECT * FROM users._roles