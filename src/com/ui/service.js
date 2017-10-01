//# sourceURL=file:///service.js


(function (spa) {

    spa.beforeControllerChange = function (oldCtrl, newCtrl) {
        if (newCtrl && newCtrl.id)
            document.body.setAttribute("ctrl", newCtrl.id);
        else
            document.body.removeAttribute("ctrl");

        //  $id("mainNavBarTitle").txt(newCtrl !== null ? newCtrl.name : null);
    };

    spa.onLoad = () => {
        spa.container = $id("container");

        var currentUl;

        window.addEventListener("mouseout", (e) => {
            if (currentUl && (e.layerX < 0 || e.layerY < 0))
                currentUl.style.display = "none";
        });

        $id("left-menu-button-skin").onclick = (e) => {
            var skin = document.body.getAttribute("skin");
            skin = skin === "dark" ? "light" : "dark";
            Utils.setCookie("skin", skin);
            document.body.setAttribute("skin", skin);
            $id("left-menu-button-skin").cls(skin === "dark" ? "fa fa-lightbulb-o" : "fa fa-moon-o" );

        };

        $id("left-menu").forEach((e) => {
            if (e.nodeName !== "LI")
                return;

            var hideTimeout;

            e.children[0].onmousemove = e.children[0].onmouseenter = (d) => {
                if (currentUl)
                    currentUl.style.display = "none";
                currentUl = d.currentTarget.nextElementSibling;
                if (!currentUl)
                    return;

                currentUl.style.display = "inline-block";

                currentUl.onmouseout = (f) => {
                    //    if (f.fromElement === f.currentTarget)
                    hideTimeout = setTimeout((tag) => {
                        tag.style.display = "none";
                    }, 300, f.currentTarget);
                };

                currentUl.onmousemove = currentUl.onmouseenter = (f) => {
                    clearTimeout(hideTimeout);
                };
            };


        });


    };

    var api = window.api = new FrameworkApi(new WebApi());

    api.api.onEvent = function (controller, sourceName, hashes, callback, data) {
        if (callback && controller.visible)
            callback(data);
    };

})(window.spa);

