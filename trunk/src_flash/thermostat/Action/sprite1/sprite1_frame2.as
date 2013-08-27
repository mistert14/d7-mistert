// Action script...

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
