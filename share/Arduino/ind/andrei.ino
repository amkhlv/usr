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

