// Action script...

// [Action in Frame 3]
tellTarget("..")
{
    call("/_phy:3");
    call("/_phy:4");
    tellTarget("")
    {
        gotoAndPlay(2);
    } // End of TellTarget
} // End of TellTarget
