// Action script...

on (release)
{
    tellTarget("..")
    {
        getURL("http://www.volta-electricite.info/", "_self");
    } // End of TellTarget
}
