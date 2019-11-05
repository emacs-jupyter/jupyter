window.CommManager = require('@jupyter-widgets/base').shims.services.CommManager;
window.WidgetManager = require('./manager').WidgetManager;
window.EmacsJupyter = require('./emacs-jupyter').EmacsJupyter;
require('font-awesome/css/font-awesome.min.css');
require('@jupyter-widgets/controls/css/widgets.built.css');

document.addEventListener("DOMContentLoaded", function(event) {

    var widget = document.createElement("div");
    widget.setAttribute("id", "widget");
    document.body.appendChild(widget);
});
