// Action script...

// [Action in Frame 1]
if (!hasOwnProperty("_load072C8F90"))
{
    _load072C8F90 = true;
    tellTarget("..")
    {
        function deplace(nom, x, y, vitesse)
        {
            tellTarget(getProperty("..", _target))
            {
                nom._x = vitesse * (nom._x - x) + x;
                nom._y = vitesse * (nom._y - y) + y;
            } // End of TellTarget
        } // End of the function
    } // End of TellTarget
} // end else if
}
else
{
    bouton = 0;
    _root.lampe11._visible = 0;
    _root.chauff._visible = 0;
    
    
    tellTarget("")
    {
        nextFrame ();
        play ();
    } // End of TellTarget

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
