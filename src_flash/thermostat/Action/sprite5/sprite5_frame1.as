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
