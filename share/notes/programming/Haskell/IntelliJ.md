Creating project from existing sources
======================================


1. Make sure your Stack project builds without errors. Preferably by using: stack build --test --haddock --no-haddock-hyperlink-source

1. After your project is built successfully, import project in IntelliJ by using `File → New  → Project from Existing Sources`... from the IntelliJ menu

1. Navigate to the project folder and highlight __the folder__ (and __not__ `stack.yaml` or something like that)

1. In next page of wizard configure Project SDK by configuring Haskell Tool Stack with selecting path to stack binary, e.g. ~/.local/bin/stack

1. Finish wizard and project will be opened;

1. Plugin will automatically build Intero and Haskell Tools (HLint, Hoogle, Hindent and Stylish Haskell) to prevent incompatibility issues


During use
==========

1. After changing the Cabal file and/or `stack.yaml` use `Tools → Haskell → Update Settings and Restart REPLs` to download missing library sources and update the project settings

1. The Event Log will display what's going on in the background. Useful when something fails. It's disabled by default. 
   It can be enabled by checking Haskell Log checkbox in the `Event Log → Settings` or `Settings → Appearance` and `Behavior → Notifications`
