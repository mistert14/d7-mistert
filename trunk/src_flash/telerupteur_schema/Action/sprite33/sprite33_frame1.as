// Action script...

// [Initial MovieClip Action of sprite 33]
#initclip 1
_global.BUTTONWRAPPER = function ()
{
};
BUTTONWRAPPER.prototype = new MovieClip();
delete _global.__resolve;
BUTTONWRAPPER.prototype.addProperty("menu", function ()
{
    return (this._button.menu);
}, function (x)
{
    this._button.menu = x;
});
BUTTONWRAPPER.prototype.addProperty("tabEnabled", function ()
{
    return (this._button.tabEnabled);
}, function (x)
{
    this._button.tabEnabled = x;
});
BUTTONWRAPPER.prototype.addProperty("tabIndex", function ()
{
    return (this._button.tabIndex);
}, function (x)
{
    this._button.tabIndex = x;
});
BUTTONWRAPPER.prototype.addProperty("enabled", function ()
{
    return (this._button.enabled);
}, function (x)
{
    this._button.enabled = x;
});
BUTTONWRAPPER.prototype.addProperty("trackAsMenu", function ()
{
    return (this._button.trackAsMenu);
}, function (x)
{
    this._button.trackAsMenu = x;
});
BUTTONWRAPPER.prototype.addProperty("useHandCursor", function ()
{
    return (this._button.useHandCursor);
}, function (x)
{
    this._button.useHandCursor = x;
});
_global.__resolve = _global.__debugResolve;
#endinitclip
