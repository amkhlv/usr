
Interacting via USB
===================

Preliminaries
-------------

To interact with `/dev/ttyACM0` the user should be in `dialout` group.

Reset problem
-------------

Arduino usually resets on starting `serial` connection. 

As explained [here](https://stackoverflow.com/questions/3918032/bash-serial-i-o-and-arduino) , have to open the connection 
(by `exec` opening a file descriptor) and keep it open:

    exec 3<> /dev/ttyACM0

    sleep 5

    stty -F /proc/$$/fd/3 cs8 9600 -ignbrk -brkint -icrnl -imaxbel -opost -onlcr -isig -icanon -iexten -echo -echoe -echok -echoctl -echoke noflsh -ixon

    echo A >&3

    # blah-blah-blah
    #
    # and at the end close the file descriptor:

    exec 3>&-

The `sleep` step is crucial, because opening the serial will reset the board. Need time to wait it to reset and settle down !

Using GNU screen for testring
-----------------------------

    screen /dev/ttyACM0 9600

The transmission is keystroke-by-keystroke, no need to press `Enter`.

Sample schema
=============

Green LED and red LED controlled over USB
-----------------------------------------

    int incomingByte = 0;

    int greenLED = 9;
    int greenDim = 3;
    int greenBright = 200;

    int redLED = 11;
    int redDim = 5;
    int redBright = 200;

    void setup() {
      // initialize serial communication at 9600 bits per second:
      Serial.begin(9600);
      analogWrite(redLED, redDim);
      analogWrite(greenLED, greenDim);
    }

    void loop() {

            // send data only when you receive data:
            if (Serial.available() > 0) {

                    // read the incoming byte:
                    incomingByte = Serial.read();

                if (incomingByte == 65) {
                  analogWrite(redLED, redDim);
                  analogWrite(greenLED, greenDim);
                }
                if (incomingByte == 66) {
                  analogWrite(redLED, redBright);
                  analogWrite(greenLED, greenDim);
                }
                if (incomingByte == 67) {
                  analogWrite(redLED, redDim);
                  analogWrite(greenLED, greenBright);
                }
                if (incomingByte == 68) {
                  analogWrite(greenLED, greenBright);
                  analogWrite(redLED, redBright);
                }
            }
    }
