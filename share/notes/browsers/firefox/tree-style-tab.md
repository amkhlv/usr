# Tree Style Tab extension

## To hide native tab bar

First go to `about:config` and set `toolkit.legacyUserProfileCustomizations.stylesheets` to `true`

Then:

    mkdir FirefoxProfile/chrome
    cat > FirefoxProfile/chrome/userChrome.css

and feed to cat:

    /* hides the native tabs */

    @namespace url("http://www.mozilla.org/keymaster/gatekeeper/there.is.only.xul");

    #TabsToolbar {
      visibility: collapse;
    }
    /* Hide min, max, close in title bar */

    #titlebar-min, #titlebar-max, #titlebar-close
    { display: none !important; }

    /* Hide minimize, restore, close in full screen */

    #minimize-button, #restore-button, #close-button
    { display: none !important; }



