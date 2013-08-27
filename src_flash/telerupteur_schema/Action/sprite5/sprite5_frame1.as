// Action script...

// [Action in Frame 1]
if (!hasOwnProperty("_load02BD6D78"))
{
    _load02BD6D78 = true;
    tellTarget("..")
    {
        _root.lampe01A._visible = 0;
        _root.lampe01B._visible = 0;
        _root.lampe01C._visible = 0;
        _root.tele01._visible = 0;
    } // End of TellTarget
} // end else if
}
else
{
    tellTarget("")
    {
        stop ();
    } // End of TellTarget
