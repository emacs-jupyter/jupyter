var base = require('@jupyter-widgets/base');
var output = require('@jupyter-widgets/output');
var controls = require('@jupyter-widgets/controls');
var PhosphorWidget = require('@phosphor/widgets').Widget;

var defineWidgetModules = function () {
    if(window.define) {
        window.define('@jupyter-widgets/output', [], function () { return output; });
        window.define('@jupyter-widgets/base', [], function () { return base; });
        window.define('@jupyter-widgets/controls', [], function () { return controls; });
    } else {
        setTimeout(defineWidgetModules, 50);
    }
};

// requirejs loading is async so it may not be available on this event
window.addEventListener("DOMContentLoaded", function () {
    defineWidgetModules();
});

var WidgetManager = exports.WidgetManager = function(kernel, area) {
    base.ManagerBase.call(this);
    this.kernel = kernel;
    this.area = area;
};
WidgetManager.prototype = Object.create(base.ManagerBase.prototype);

WidgetManager.prototype.loadClass = function(className, moduleName, moduleVersion) {
    return new Promise(function(resolve, reject) {
        if (moduleName === '@jupyter-widgets/controls') {
            resolve(controls);
        } else if (moduleName === '@jupyter-widgets/base') {
            resolve(base);
        } else if (moduleName === '@jupyter-widgets/output')
            resolve(output);
        else {
            var fallback = function(err) {
                var failedId = err.requireModules && err.requireModules[0];
                if (failedId) {
                    console.log('Falling back to unpkg.com for ' + moduleName + '@' + moduleVersion);
                    window.require(['https://unpkg.com/' + moduleName + '@' + moduleVersion + '/dist/index.js'], resolve, reject);
                } else {
                    throw err;
                }
            };
            window.require([moduleName + '.js'], resolve, fallback);
        }
    }).then(function(module) {
        if (module[className]) {
            return module[className];
        } else {
            return Promise.reject('Class ' + className + ' not found in module ' + moduleName + '@' + moduleVersion);
        }
    });
}

WidgetManager.prototype.display_view = function(msg, view, options) {
    var _this = this;
    return Promise.resolve(view).then(function(view) {
        PhosphorWidget.attach(view.pWidget, _this.area);
        view.on('remove', function() {
            console.log('View removed', view);
        });
        view.trigger('displayed');
        return view;
    });
};

WidgetManager.prototype._get_comm_info = function() {
    return this.kernel.requestCommInfo(this.comm_target_name).then(function(reply) {
        return reply.content.comms;
    });
};

WidgetManager.prototype._create_comm = function(targetName, commId, data, metadata) {
    // Construct a comm that already exists
    var comm = this.kernel.connectToComm(targetName, commId);
    if(data || metadata) {
        comm.open(data, metadata);
    }
    return Promise.resolve(new base.shims.services.Comm(comm));
}
