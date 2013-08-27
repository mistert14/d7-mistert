// Action script...

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
