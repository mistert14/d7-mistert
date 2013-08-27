// Action script...

on (release)
{
    tellTarget("..")
    {
        if (bouton == 1)
        {
            bouton = 0;
        }
        else
        {
            bouton = 1;
        } // end else if
    } // End of TellTarget
}
