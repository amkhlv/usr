#include "DHT.h"

//#include "MemoryFree.h"

DHT dht;

const int humLED = 6; 
const int humDATA = 2;
const int timeInterval = 2 * dht.getMinimumSamplingPeriod();

int brightness = 200;

void setup()
{ 
  pinMode(humLED, OUTPUT);
  analogWrite(humLED, brightness);
  dht.setup(humDATA); // data pin 
  //Serial.begin(9600);
}

void adjust(int newbrightness, int t) 
{
    int step = 1;
    while (abs(newbrightness - brightness) > step) {
        if (newbrightness > brightness) { brightness = brightness + step ; } else { brightness = brightness - step ; }
        analogWrite(humLED, brightness);
        delay(t);
    }
}

void loop()
{ 
  delay(timeInterval);
  //Serial.print("humidity=");
  int hum = dht.getHumidity();
  //Serial.println(hum);
  if (hum < 50) { 
    brightness = min(255, 3.5 * (100 - hum)); 
    analogWrite(humLED, brightness); 
    if (hum > 40) { 
      delay( 15 * (50 - hum) );
      brightness = 64;
      analogWrite(humLED, brightness); 
    }
  } else { adjust(24, 30); }
  if (hum >= 55) { adjust(12, 30); delay(180); }
  if (hum >= 60) { adjust(6, 60); delay(360); }
  if (hum >= 65) { adjust(3, 80); delay(720); }
  if (hum >= 70) { digitalWrite(humLED, LOW); brightness = 0; }
  //Serial.print("freeMemory()=");
  //Serial.println(freeMemory());
}
