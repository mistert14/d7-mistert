// Action script...

// [Action in Frame 1]
setProperty("", _visible, false);
stop ();

// [Action in Frame 2]
tellTarget(null)
{
    if (!hasOwnProperty("_physicsInit"))
    {
        _physicsInit = true;
        _time = getTimer() / 1000;
        _birth = _time;
        _delta = 0;
        _age = 0;
        if (!hasOwnProperty("_fx"))
        {
            _fx = 0;
        } // end if
        if (!hasOwnProperty("_fy"))
        {
            _fy = 0;
        } // end if
        if (!hasOwnProperty("_fxscale"))
        {
            _fxscale = 0;
        } // end if
        if (!hasOwnProperty("_fyscale"))
        {
            _fyscale = 0;
        } // end if
        if (!hasOwnProperty("_frotation"))
        {
            _frotation = 0;
        } // end if
        if (!hasOwnProperty("_falpha"))
        {
            _falpha = 0;
        } // end if
        if (!hasOwnProperty("_vx"))
        {
            _vx = 0;
        } // end if
        if (!hasOwnProperty("_vy"))
        {
            _vy = 0;
        } // end if
        if (!hasOwnProperty("_vxscale"))
        {
            _vxscale = 0;
        } // end if
        if (!hasOwnProperty("_vyscale"))
        {
            _vyscale = 0;
        } // end if
        if (!hasOwnProperty("_vrotation"))
        {
            _vrotation = 0;
        } // end if
        if (!hasOwnProperty("_valpha"))
        {
            _valpha = 0;
        } // end if
        if (!hasOwnProperty("_ax"))
        {
            _ax = 0;
        } // end if
        if (!hasOwnProperty("_ay"))
        {
            _ay = 0;
        } // end if
        if (!hasOwnProperty("_axscale"))
        {
            _axscale = 0;
        } // end if
        if (!hasOwnProperty("_ayscale"))
        {
            _ayscale = 0;
        } // end if
        if (!hasOwnProperty("_arotation"))
        {
            _arotation = 0;
        } // end if
        if (!hasOwnProperty("_aalpha"))
        {
            _aalpha = 0;
        } // end if
    } // end if
} // End of TellTarget

// [Action in Frame 3]
tellTarget(null)
{
    if (!hasOwnProperty("_physicsInit"))
    {
        call("/_phy:2");
    } // end if
    _tt = getTimer() / 1000;
    _delta = _tt - _time;
    _age = _tt - _birth;
    _time = _tt;
} // End of TellTarget

// [Action in Frame 4]
tellTarget(null)
{
    _ffx = (100 - _fx * _delta) / 100;
    _ffy = (100 - _fy * _delta) / 100;
    _ffxscale = (100 - _fxscale * _delta) / 100;
    _ffyscale = (100 - _fyscale * _delta) / 100;
    _ffrotation = (100 - _frotation * _delta) / 100;
    _ffalpha = (100 - _falpha * _delta) / 100;
    _vx = (_vx + _ax * _delta) * _ffx;
    _vy = (_vy + _ay * _delta) * _ffy;
    _vxscale = (_vxscale + _axscale * _delta) * _ffxscale;
    _vyscale = (_vyscale + _ayscale * _delta) * _ffyscale;
    _vrotation = (_vrotation + _arotation * _delta) * _ffrotation;
    _valpha = (_valpha + _aalpha * _delta) * _ffalpha;
    if (_vx)
    {
        setProperty("", _x, _x + _vx * _delta);
    } // end if
    if (_vy)
    {
        setProperty("", _y, _y + _vy * _delta);
    } // end if
    if (_vxscale)
    {
        setProperty("", _xscale, _xscale + _vxscale * _delta);
    } // end if
    if (_vyscale)
    {
        setProperty("", _yscale, _yscale + _vyscale * _delta);
    } // end if
    if (_vrotation)
    {
        setProperty("", _rotation, _rotation + _vrotation * _delta);
    } // end if
    if (_valpha)
    {
        setProperty("", _alpha, _alpha + _valpha * _delta);
    } // end if
} // End of TellTarget
