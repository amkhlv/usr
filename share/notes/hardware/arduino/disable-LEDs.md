Disable TX and RX LEDs
======================

As described in [this answer](https://arduino.stackexchange.com/questions/63446/arduino-pro-micro-switch-off-leds)

In `setup()`:

    pinMode(LED_BUILTIN_TX,INPUT);
    pinMode(LED_BUILTIN_RX,INPUT);

(This sets them into the "button" mode, effectively disabling...)
