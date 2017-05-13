DROP SCHEMA IF EXISTS event;

CREATE SCHEMA event;

--------------------------------------------------------------------------------

CREATE TABLE event.events
(
  event_id INTEGER PRIMARY KEY AUTO_INCREMENT NOT NULL, 
  created TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP), 
  type CHAR NOT NULL,
  source VARCHAR,
  value VARCHAR,
  tags ARRAY, 
  username VARCHAR(200),
  address VARCHAR(200),
  user_id INTEGER,
  url VARCHAR
);


CREATE TABLE event.attributes
(
    event_id integer NOT NULL,
    tags ARRAY NOT NULL,
    name VARCHAR,
    value VARCHAR
);

CREATE INDEX attributes_event_id ON event.attributes(event_id);


CREATE TABLE event.details
(
    event_id integer NOT NULL,
    tags ARRAY NOT NULL,
    name VARCHAR,
    value VARCHAR,
    content_type VARCHAR
);

CREATE INDEX details_event_id ON event.details(event_id);

CREATE TABLE event.keys
(
    event_id integer NOT NULL,
    keys ARRAY NOT NULL,
    column_name VARCHAR
);

CREATE INDEX keys_event_id ON event.keys(event_id);
CREATE INDEX keys_keys ON event.keys(keys);

