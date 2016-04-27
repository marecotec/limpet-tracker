//Adapted from the Love Electronics Tilt compensated compass tutorial - Posted 18/10/11
//http://www.loveelectronics.co.uk/Tutorials/13/tilt-compensated-compass-arduino-tutorial
//Adapted for the Limpet Tracking Device MSc Project
//Will Alderson and Andrew Davies, Bangor University 2013
 
// Reference the I2C Library
#include <Wire.h>
#include <SPI.h>
//Reference Adafruit Sensor Framework
#include <Adafruit_Sensor.h>
// Reference the HMC5883L Compass Library
#include <Adafruit_HMC5883_U.h>
// Reference the ADXL345 Accelerometer Library
#include <Adafruit_ADXL345_U.h>
// Reference the SD Card Library
#include <SD.h>
 
// Store our compass as a variable.
Adafruit_HMC5883_Unified mag = Adafruit_HMC5883_Unified(12345);

// Store our accelerometer as a variable.
Adafruit_ADXL345_Unified accel = Adafruit_ADXL345_Unified(12345);


// Set up a pin we are going to use to indicate our status using an LED.
int runningPin = 8; // I'm using digital pin 8.
int failurePin = 6;

//Create Error variables
int areConnected = 1; // Store our connection status here.
int recordCount = 1;
int writeError = 0;

//Set up the SD card CS on digital pin 10 and open the data file
const int chipSelect = 10;
File dataFile;

//Create a buffer to hold data before exporting it to the SD card
String dataString = "";

//TC or not TC that is the question
String tc = "";

//Calibration constants for Accel. Run calibration code included in github repo
float offaccXmin = 11.61;
float offaccXmax = -10.59;
float offaccYmin = -9.51;
float offaccYmax = 12.59;
float offaccZmin = -10.71;
float offaccZmax = 9.02;

float xOffset=0.0;
float yOffset=0.0;
float zOffset=0.0;


//Calibration constants for Mag. Run calibration code included in github repo
int xMagnetMax=-2;
int yMagnetMax=-37;
int zMagnetMax=-54;
int xMagnetMin=43;
int yMagnetMin=0;
int zMagnetMin=-0;

float xMagnetMap=0;
float yMagnetMap=0;
float zMagnetMap=0;

int xMagnet=0;
int yMagnet=0;
int zMagnet=0;

void setup()
{
  Serial.begin(9600); // Initialize the serial port.
  Wire.begin(); // Start the I2C interface.
 
  pinMode(runningPin, OUTPUT); // Ready an LED to indicate our status.
  pinMode(failurePin, OUTPUT);
 
  // Construct a new HMC5883 compass.
  sensor_t sensor;
  mag.getSensor(&sensor);
  
 // Construct a new ADXL345 accelerometer.
  sensor_t sensor1;
  accel.getSensor(&sensor1);
  
  if(accel.begin() && mag.begin())
  {
    areConnected = true;
    Serial.println(F("Connected to HMC5883L and ADXL345."));
    digitalWrite(runningPin, HIGH);
    digitalWrite(failurePin, LOW);
  }
  else
  {
    areConnected = false;
    digitalWrite(runningPin, LOW);
    digitalWrite(failurePin, HIGH);    
 
    if(!mag.begin())
      Serial.println(F("Could not connect to HMC5883L."));
    if(!accel.begin())
      Serial.println(F("Could not connect to ADXL345."));
  }
 
 
 if(areConnected = true)
  { 
  //  compass.SetScale(1.3); // Set the scale of the compass.
  //  compass.SetMeasurementMode(Measurement_Continuous); // Set the measurement mode to Continuous
 
  /* Set the range to whatever is appropriate for your project */
  accel.setRange(ADXL345_RANGE_2_G);
  // displaySetRange(ADXL345_RANGE_8_G);
  // displaySetRange(ADXL345_RANGE_4_G);
  //displaySetRange(ADXL345_RANGE_2_G);

  }
  Serial.print(F("Initializing SD card..."));
  // **SD** make sure that the default chip select pin is set to output, even if you don't use it:
  pinMode(SS, OUTPUT);
 
  // **SD** see if the card is present and can be initialized:
  if (!SD.begin(10)) {
    Serial.println(F("Card failed, or not present"));
    // don't do anything more:
    digitalWrite(runningPin, LOW);
    digitalWrite(failurePin, HIGH);
    while (1);
  }
  Serial.println(F("card initialized."));
 
  // **SD** Open up the file we're going to log to!
  dataFile = SD.open("datalog.txt", FILE_WRITE);
  if (! dataFile) {
    Serial.println(F("error opening datalog.txt"));
    digitalWrite(runningPin, LOW);
    digitalWrite(failurePin, HIGH);
    // Wait forever since we cant write data
    while (1) ;
  }
  digitalWrite(runningPin, HIGH);

  getAccelOffset();           //keep it flat and non moving on the table
}
 
void loop()
{
  if(areConnected)
  {
    sensors_event_t event; 
    accel.getEvent(&event);
    
    sensors_event_t event1; 
    mag.getEvent(&event1);
 
  // DO THE ACCEL TILT FIX ETC
  // We are swapping the accelerometers axis as they are opposite to the compass the way we have them mounted.
  // We are swapping the signs axis as they are opposite.
  // Configure this for your setup.
  float accX = event.acceleration.x;
  float accY = event.acceleration.y;
  float accZ = event.acceleration.z;

  accX = accX-xOffset;
  accY = accY-yOffset;
  accZ = accZ-zOffset;
  
  accX = map(accX, offaccXmin, offaccXmax, -1, 1);
  accY = map(accY, offaccYmin, offaccYmax, -1, 1);
  accZ = map(accZ, offaccZmin, offaccZmax, -1, 1);
  
  float rollRadians = atan2(accY, sqrt(sq(accX)+sq(accZ)));  
  float pitchRadians  = atan2(accX, sqrt(sq(accY)+sq(accZ)));

  //Here apply compass calibration to compass data
  if (event1.magnetic.x>xMagnetMax) {xMagnetMax = event1.magnetic.x;}
  if (event1.magnetic.y>yMagnetMax) {yMagnetMax = event1.magnetic.y;}
  if (event1.magnetic.z>zMagnetMax) {zMagnetMax = event1.magnetic.z;}
 
  if (event1.magnetic.x<xMagnetMin) {xMagnetMin = event1.magnetic.x;}
  if (event1.magnetic.y<yMagnetMin) {yMagnetMin = event1.magnetic.y;}
  if (event1.magnetic.z<zMagnetMin) {zMagnetMin = event1.magnetic.z;}

  //Map the incoming Data from -1 to 1
  xMagnetMap = float(map(xMagnet, xMagnetMin, xMagnetMax, -100, 100))/100;
  yMagnetMap = float(map(yMagnet, yMagnetMin, yMagnetMax, -100, 100))/100;
  zMagnetMap = float(map(zMagnet, zMagnetMin, zMagnetMax, -100, 100))/100;

  //normalize the magnetic vector
  float norm = sqrt(sq(xMagnetMap) + sq(yMagnetMap) + sq(zMagnetMap));
  xMagnetMap /=norm;
  yMagnetMap /=norm;
  zMagnetMap /=norm;

  //calculate heading  
  float heading = atan2(event1.magnetic.y, event1.magnetic.x);
{
  
  // Correct for when signs are reversed.
  if(heading < 0)
    heading += 2*PI;
 
  // Check for wrap due to addition of declination.
  if(heading > 2*PI)
    heading -= 2*PI;
}

  // Convert radians to degrees for readability (subtract 90deg in excel (IF cell >90, cell + 90, (cell - 90) + 360).
  float headingDegrees = heading * 180/M_PI;
  
  //Include declination angle for your location (menai bridge)
  float declinationAngle = -2.503;
  float finalAngle = headingDegrees += declinationAngle;
 
 if (recordCount <= 50) {
    recordCount ++;
    digitalWrite(runningPin, HIGH);
  }  else {
    recordCount ++;
    digitalWrite(runningPin, LOW);
  }

  if (SD.exists("datalog.txt")) 
  {
   dataString += String(event.acceleration.x);
   dataString += ", ";
   dataString += String(event.acceleration.y);
   dataString += ", ";   
   dataString += String(event.acceleration.z);
   dataString += ", ";
   dataString += String(event1.magnetic.x);
   dataString += ", ";
   dataString += String(event1.magnetic.y);
   dataString += ", ";
   dataString += String(event1.magnetic.z);
   dataString += ", ";
   dataString += String(finalAngle);    
 
  } 
  else {
    dataFile.flush();
    digitalWrite(failurePin, HIGH); 
    Serial.print(F("Error recording at "));
    Serial.println();
    digitalWrite(runningPin, LOW);
    digitalWrite(failurePin, HIGH);
  }
 
    dataFile.println(dataString);
    Serial.println(dataString);
    dataFile.flush();
    dataString = ""; 
    delay(250);  
  }
}
 
void getAccelOffset()
{ //you can make approx 60 iterations because we use an unsigned int 
 //otherwise you get an overflow. But 60 iterations should be fine
  for (int i=1; i <= 60; i++){
    sensors_event_t event; 
    accel.getEvent(&event);        
    xOffset += event.acceleration.x;     
    yOffset += event.acceleration.y;
    zOffset += event.acceleration.z;
    } 
  xOffset /=60;   
  yOffset /=60;
  zOffset /=60;
  
//Serial.print("xOffset: ");
  //Serial.print(xOffset);
  //Serial.print("   yOffset: ");
  //Serial.print(yOffset);
  //Serial.print("   zOffset: ");
  //Serial.println(zOffset);
  
}
