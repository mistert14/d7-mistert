// Action script...

// [Action in Frame 3]
tellTarget("..")
{
    if (bouton == 1)
    {
        thon._rotation = -28;
        2;
        _root.lampe11._visible = 1;
        _root.chauff._visible = 1;
        _root.phase._visible = 0;
        _root.deplace(rectangle, 340, 120, 0.990000);
    }
    else
    {
        thon._rotation = 0;
        _root.lampe11._visible = 0;
        _root.thon._rotation = 0;
        _root.chauff._visible = 0;
        _root.phase._visible = 1;
        _root.deplace(rectangle, 340, 160, 0.990000);
    } // end else if
    tellTarget("")
    {
        gotoAndPlay(2);
    } // End of TellTarget
} // End of TellTarget
