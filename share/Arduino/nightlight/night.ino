bool lit[6] = {false, false, false, false, false, false};
int pwmPin[6] = {3, 5, 6, 9, 10, 11};

void setup() {
  pinMode(2,OUTPUT);
  pinMode(7,OUTPUT);
  analogWrite(3,5);
  analogWrite(5,5);
  analogWrite(6,5);
  analogWrite(9,5);
  analogWrite(10,5);
  analogWrite(11,5);
  digitalWrite(2,HIGH);
  digitalWrite(7,HIGH);
}

void loop() {
  delay(16);
  int c = random(6);
  int v = random(2);
  if (lit[c]) {
    if (v == 0) {
        for (int i = 3 ; i >= 0 ; i = i - 1) {
            delay(4);
            analogWrite(pwmPin[c], 64 * i);
        }
        lit[c] = false;
    }
  } else {
    if (v == 1) {
        for (int i = 1; i <= 4 ; i = i + 1) {
            delay(4);
            analogWrite(pwmPin[c], 64 * i - 1);
        }
        lit[c] = true;
    }
  }
}

