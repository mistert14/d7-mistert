// Action script...

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
