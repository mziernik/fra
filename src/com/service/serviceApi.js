function FrameworkApi(api) {
    "use strict";

    this.api = api;
    api.httpUrl = api.httpUrl || "http://localhost/$/api";
    api.wsUrl = api.wsUrl || "ws://localhost/$api";
    api.hash = 'tmFNag';

    this.cache = {
        list: function (data) {
            return api.call("cache/list", 'DVEReg', 'CRUD', data, {});
        },
        remove: function (data) {
            return api.call("cache/remove", 'ZO8EQw', 'CRUD', data, {
                id: ["string", true]
            });
        }
    };

    this.config = {
        exportConf: function (data) {
            return api.call("config/exportConf", 'EG4hdQ', 'CRUD', data, {
                usersValue: ["boolean", true]
            });
        },
        getDisplayValue: function (data) {
            return api.call("config/getDisplayValue", 'WTxf1g', 'CRUD', data, {
                "": ["object", true],
                id: ["string", true]
            });
        },
        getItem: function (data) {
            return api.call("config/getItem", 'JbkUvg', 'CRUD', data, {
                id: ["string", true]
            });
        },
        getList: function (data) {
            return api.call("config/getList", 'MXBsww', 'CRUD', data, {});
        },
        getStructure: function (data) {
            return api.call("config/getStructure", 'emKJzQ', 'CRUD', data, {});
        },
        getValue: function (data) {
            return api.call("config/getValue", 'w7iWIg', 'CRUD', data, {
                id: ["string", true]
            });
        },
        importConf: function (data) {
            return api.call("config/importConf", 'GvkWkQ', 'CRUD', data, {
                usersValue: ["boolean", true]
            });
        },
        save: function (data) {
            return api.call("config/save", 'gP0i6g', 'CRUD', data, {
                "": ["object", true],
                id: ["string", true]
            });
        },
        validate: function (data) {
            return api.call("config/validate", 'YfgSeQ', 'CRUD', data, {
                "": ["json", true],
                id: ["string", true]
            });
        }
    };

    this.cron = {
        getAll: function (data) {
            return api.call("cron/getAll", 'u4EVMA', 'CRUD', data, {});
        }
    };

    this.database = {
        execute: function (data) {
            return api.call("database/execute", '9BxXZA', 'CRUD', data, {
                id: ["string", true],
                query: ["string", true],
                limit: ["number", false]
            });
        },
        getDatabases: function (data) {
            return api.call("database/getDatabases", 'bBNcJA', 'CRUD', data, {});
        },
        getMeta: function (data) {
            return api.call("database/getMeta", 'bBZ5Ww', 'CRUD', data, {
                id: ["string", true]
            });
        },
        getSessions: function (data) {
            return api.call("database/getSessions", 'OPlFDg', 'CRUD', data, {});
        },
        invalidateSession: function (data) {
            return api.call("database/invalidateSession", 'p8e9Sw', 'CRUD', data, {
                id: ["string", true]
            });
        }
    };

    this.events = {
        filter: function (data) {
            return api.call("events/filter", 'igoZaQ', 'CRUD', data, {});
        },
        getDetails: function (data) {
            return api.call("events/getDetails", 'Y0A25w', 'CRUD', data, {
                id: ["number", true]
            });
        },
        getInitData: function (data) {
            return api.call("events/getInitData", 'kpllLQ', 'CRUD', data, {});
        },
        result: function (data) {
            return api.call("events/result", 'IG4PcA', 'CRUD', data, {
                offset: ["number", false]
            });
        }
    };

    this.http = {
        invalidateSession: function (data) {
            return api.call("http/invalidateSession", 'p8e9Sw', 'CRUD', data, {
                id: ["string", true]
            });
        },
        sessions: function (data) {
            return api.call("http/sessions", 'ibso2Q', 'CRUD', data, {});
        }
    };

    this.language = {
        googleTranslate: {
            autoTransplate: function (data) {
                return api.call("language/googleTranslate/autoTransplate", 'kqB6IQ', 'CRUD', data, {
                    sl: ["string", true],
                    tl: ["string", true]
                });
            },
            getLanguages: function (data) {
                return api.call("language/googleTranslate/getLanguages", 'wiHydA', 'CRUD', data, {});
            },
            translate: function (data) {
                return api.call("language/googleTranslate/translate", 'Ozy5FQ', 'CRUD', data, {
                    sl: ["string", true],
                    tl: ["string", true],
                    text: ["string", true]
                });
            }
        },
        add: function (data) {
            return api.call("language/add", 'jukPUA', 'CRUD', data, {
                key: ["string", true],
                name: ["string", true]
            });
        },
        editEntry: function (data) {
            return api.call("language/editEntry", 'NA3GNw', 'CRUD', data, {
                language: ["string", true],
                key: ["string", true],
                value: ["string", true],
                complete: ["boolean", true]
            });
        },
        export: function (data) {
            return api.call("language/export", 'Leat1Q', 'CRUD', data, {
                language: ["string", true]
            });
        },
        getAll: function (data) {
            return api.call("language/getAll", 'u4EVMA', 'CRUD', data, {});
        },
        getCurrent: function (data) {
            return api.call("language/getCurrent", 'cQej8A', 'CRUD', data, {});
        },
        getEntries: function (data) {
            return api.call("language/getEntries", 'QfrFlA', 'CRUD', data, {
                langKey: ["string", true],
                refKey: ["string", false],
                incomplete: ["boolean", false]
            });
        },
        getTranslation: function (data) {
            return api.call("language/getTranslation", 'IB1gjQ', 'CRUD', data, {
                language: ["string", false],
                groups: ["string[]", false]
            });
        },
        import: function (data) {
            return api.call("language/import", 'rz4Mcw', 'CRUD', data, {});
        },
        remove: function (data) {
            return api.call("language/remove", 'bvEP1A', 'CRUD', data, {
                lang: ["string", true]
            });
        },
        setCurrent: function (data) {
            return api.call("language/setCurrent", 'xLLaig', 'CRUD', data, {
                id: ["string", true]
            });
        }
    };

    this.log = {
        storage: {
            getAll: function (data) {
                return api.call("log/storage/getAll", 'u4EVMA', 'CRUD', data, {});
            }
        }
    };

    this.service = {
        dbModel: {
            edit: function (data) {
                return api.call("service/dbModel/edit", 'k5hbfQ', 'CRUD', data, {
                    "": ["object", true],
                    table: ["string", true],
                    key: [null, false]
                });
            },
            editMultiple: function (data) {
                return api.call("service/dbModel/editMultiple", 'irw3RQ', 'CRUD', data, {
                    "": ["array", true]
                });
            },
            export: function (data) {
                return api.call("service/dbModel/export", 'bVJXCg', 'CRUD', data, {});
            },
            getAll: function (data) { // Zwraca dane z wielu tabel
                return api.call("service/dbModel/getAll", 'iwFzYg', 'CRUD', data, {
                    "": ["array", true]
                });
            },
            list: function (data) { // Lista wszystkich rekordów w cache
                return api.call("service/dbModel/list", 'DVEReg', 'CRUD', data, {});
            },
            remove: function (data) {
                return api.call("service/dbModel/remove", 'Bzba3A', 'CRUD', data, {
                    table: ["string", true],
                    key: [null, true]
                });
            }
        },
        httpSession: {
            getAll: function (data) {
                return api.call("service/httpSession/getAll", 'u4EVMA', 'CRUD', data, {});
            }
        },
        notifications: {
            getPatterns: function (data) {
                return api.call("service/notifications/getPatterns", 'ISk94w', 'CRUD', data, {});
            },
            getSources: function (data) {
                return api.call("service/notifications/getSources", 'aTzqHw', 'CRUD', data, {});
            },
            hashChange: function (data) {
                return api.call("service/notifications/hashChange", 'qHTEcg', 'CRUD', data, {
                    hash: ["string", true]
                });
            },
            log: function (data) {
                return api.call("service/notifications/log", '2vFHMg', 'CRUD', data, {
                    "": ["object", true],
                    type: ["string", true],
                    value: ["string", true]
                });
            },
            register: function (data) {
                return api.call("service/notifications/register", 'SbXo2Q', 'CRUD', data, {
                    "": ["object", true]
                });
            }
        },
        session: {
            authorize: function (data) {
                return api.call("service/session/authorize", 'MVfGRQ', 'CRUD', data, {
                    "": ["json", true],
                    username: ["string", true]
                });
            },
            getCurrentSession: function (data) {
                return api.call("service/session/getCurrentSession", 'F9SuFg', 'CRUD', data, {});
            },
            getCurrentUser: function (data) {
                return api.call("service/session/getCurrentUser", 'XPeYNg', 'CRUD', data, {});
            }
        },
        getData: function (data) { // Zwraca wszystko co może się przydać
            return api.call("service/getData", 'VxzWSg', 'CRUD', data, {
                "": ["array", true]
            });
        }
    };

    this.system = {
        shell: function (data) { // cmd / bash
            return api.call("system/shell", 'jI2H9A', 'CRUD', data, {});
        }
    };

    this.test = {
        broadcast: function (data) {
            return api.call("test/broadcast", 'Y9a5lw', 'CRUD', data, {});
        },
        cancel: function (data) { // Test metody cancel webapi - jeśli anulowanie nie nastepi w ciągu ok 10s, usługa zgłosi błąd
            return api.call("test/cancel", 'oiMwfg', 'CRUD', data, {});
        },
        delay: function (data) {
            return api.call("test/delay", 'AYL1iA', 'CRUD', data, {});
        },
        dict: function (data) {
            return api.call("test/dict", '2ZinjA', 'CRUD', data, {
                offset: ["number", false]
            });
        },
        event: function (data) {
            return api.call("test/event", 'IxZv3Q', 'CRUD', data, {});
        },
        file: function (data) {
            return api.call("test/file", 'OPIP7g', 'CRUD', data, {});
        },
        hints: function (data) {
            return api.call("test/hints", 'aMOfyA', 'CRUD', data, {});
        },
        messages: function (data) {
            return api.call("test/messages", 'gqNXIw', 'CRUD', data, {});
        },
        numbers: function (data) {
            return api.call("test/numbers", 'Yuu1Hg', 'CRUD', data, {});
        },
        users: function (data) {
            return api.call("test/users", 'u4iCqg', 'CRUD', data, {});
        },
        usersUpdate: function (data) {
            return api.call("test/usersUpdate", 'yEWcAg', 'CRUD', data, {
                action: ["string", true]
            });
        }
    };

    this.tools = {
        eval: function (data) { // Evaluator
            return api.call("tools/eval", 'X8TCow', 'CRUD', data, {
                "": ["string", true]
            });
        }
    };

    this.user = {
        edit: function (data) {
            return api.call("user/edit", 'Ljxdcw', 'CRUD', data, {
                login: ["string", true]
            });
        },
        getAll: function (data) {
            return api.call("user/getAll", 'u4EVMA', 'CRUD', data, {});
        },
        getCurrent: function (data) {
            return api.call("user/getCurrent", 'cQej8A', 'CRUD', data, {});
        },
        getRoleGroups: function (data) {
            return api.call("user/getRoleGroups", '9KTDtA', 'CRUD', data, {});
        },
        getRoles: function (data) {
            return api.call("user/getRoles", 'Sjhb0Q', 'CRUD', data, {});
        },
        getStatuses: function (data) {
            return api.call("user/getStatuses", 'DKBuRA', 'CRUD', data, {});
        },
        getTypes: function (data) {
            return api.call("user/getTypes", 'm3XeWA', 'CRUD', data, {});
        },
        getUser: function (data) {
            return api.call("user/getUser", '91B1cA', 'CRUD', data, {
                id: ["number", true]
            });
        },
        getUserGroups: function (data) {
            return api.call("user/getUserGroups", 'kABomw', 'CRUD', data, {});
        },
        getUserStatuses: function (data) {
            return api.call("user/getUserStatuses", 'wVXd2w', 'CRUD', data, {});
        },
        getUserTypes: function (data) {
            return api.call("user/getUserTypes", '5eKGsg', 'CRUD', data, {});
        }
    };

    this.webSocket = {
        disconnect: function (data) {
            return api.call("webSocket/disconnect", 'GNNbEw', 'CRUD', data, {
                id: ["string", true]
            });
        },
        sessions: function (data) {
            return api.call("webSocket/sessions", 'ibso2Q', 'CRUD', data, {});
        }
    };

    api.initImpl(this);

}