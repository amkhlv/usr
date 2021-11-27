# Microphone volume

## Edit file /system/vendor/etc/mixer_paths_0.xml

    adb root
    adb remount
    adb shell

Then change:

        <!-- Input stage volume -->
        <!-- media input volume -->
        <path name="gain-media-mic">
                <ctl name="MIC1 Volume" value="11" />
                <ctl name="MIC1 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="150" />
                <ctl name="ADC Right Gain" value="150" />
        </path>

        <path name="gain-media-headset-mic">
                <ctl name="MIC2 Volume" value="4" />
                <ctl name="MIC2 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="127" />
                <ctl name="ADC Right Gain" value="127" />
        </path>

        <path name="gain-media-headphone-mic">
                <ctl name="MIC1 Volume" value="11" />
                <ctl name="MIC1 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="127" />
                <ctl name="ADC Right Gain" value="127" />
        </path>

        <path name="gain-media-bt-sco-headset-in">
                <ctl name="S2801 RMIX2_LVL" value="0" />
        </path>

to:

        <!-- Input stage volume -->
        <!-- media input volume -->
        <path name="gain-media-mic">
                <ctl name="MIC1 Volume" value="89" />
                <ctl name="MIC1 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="150" />
                <ctl name="ADC Right Gain" value="150" />
        </path>

        <path name="gain-media-headset-mic">
                <ctl name="MIC2 Volume" value="89" />
                <ctl name="MIC2 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="127" />
                <ctl name="ADC Right Gain" value="127" />
        </path>

        <path name="gain-media-headphone-mic">
                <ctl name="MIC1 Volume" value="89" />
                <ctl name="MIC1 Boost Volume" value="2" />
                <ctl name="ADC Left Gain" value="127" />
                <ctl name="ADC Right Gain" value="127" />
        </path>

        <path name="gain-media-bt-sco-headset-in">
                <ctl name="S2801 RMIX2_LVL" value="0" />
        </path>

Then: 

    adb unroot

And __reboot__ cell phone

