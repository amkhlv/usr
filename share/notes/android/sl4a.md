API reference
=============

[SL4A Api Reference](https://github.com/damonkohler/sl4a/blob/wiki/ApiReference.md)

Some useful functions
---------------------

### Put text to clipboard

    setClipboard(String text)

### Take a picture and save it to the specified path.

    cameraCapturePicture(
     String targetPath,
     Boolean useAutoFocus[optional, default true])

Returns:
 A map of Booleans autoFocus and takePicture where True indicates success.

Calling from QLua
=================

    require "android"
    h = android.getLastKnownLocation()
    for key,value in pairs(h.result.network) do print(key,value) end



