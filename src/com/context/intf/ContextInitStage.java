package com.context.intf;

public enum ContextInitStage {

    classLoaded,
    allClassesLoaded,
    beforeConfigLoaded,
    afterConfigLoaded,
    beforeUsersLoaded,
    allDone
}
