{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.GI.Base.GVariant
import Data.Text
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import GI.Gio.Objects.Cancellable

main :: IO ()
main = do
  -- Connect to the session bus
  bus <- Gio.busGetSync Gio.BusTypeSession (Nothing :: Maybe Cancellable)

  let appId = Just "org.inkscape.Inkscape"
      mainPath = "/org/inkscape/Inkscape"
      docPath = "/org/inkscape/Inkscape/document/1"

  -- Get the remote DBusActionGroup
  inkact <- Gio.dBusActionGroupGet bus appId mainPath
  docact <- Gio.dBusActionGroupGet bus appId docPath
  winact <- Gio.dBusActionGroupGet bus appId "/org/inkscape/Inkscape/window/1"
  -- Loop every second
  Gio.actionGroupActivateAction winact "document-save" Nothing
  Gio.dBusConnectionFlush bus (Nothing :: Maybe Cancellable) Nothing
