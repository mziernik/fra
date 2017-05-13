CREATE TABLE translation_language
(
	key         VARCHAR(10) PRIMARY KEY,
	added       TIMESTAMP NOT NULL DEFAULT (CURRENT_TIMESTAMP),
	name        VARCHAR 
);

CREATE TABLE translation_entry
(
	id          INTEGER PRIMARY KEY AUTO_INCREMENT,
    item        VARCHAR(12) NOT NULL,
    language    VARCHAR(10) NOT NULL REFERENCES translation_language (key),
	changed     TIMESTAMP NULL DEFAULT (CURRENT_TIMESTAMP),	
	value       VARCHAR NOT NULL,
	complete    BOOLEAN NOT NULL,
    CONSTRAINT  translation_item_unq UNIQUE (item, language)
);