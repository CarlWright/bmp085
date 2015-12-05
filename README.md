# bmp085 (A Erlang app to measure air temperature and pressure)
# Also works for the Bosch BMP180.

This erlang application works on the Raspberry PI using the "erlang_ale" library to interface using I2C to the BMP085 device.

## Initialization

Before you can get air temperatures or pressures, you need to start a process to communicate with the BMP085 device. Call `bmp085:start_link()` and get `{ok, <sensor pid>}`. Use the <sensor pid> value in all your calls to get temperatures and pressures.

## Air temperature

When you call `bmp085:read_temp(Sensor_PID)`, you get `{ok,<temperature in Celsius>, <temperature in Fahrenheit>}`.

For example, I called it now and got `{ok,21.51,70.72}`.

## Air pressure

When you call `bmp085::read_pressure(Sensor_PID)`, you get `{ok, <air pressure in Pa>}`. 1 Pa equals 0.01hPa or 0.01mbar.

For example, I called it now and got `{ok,98958}`.

## Altitude

When you call `bmp085:read_altitude(Sensor_PID)`, you get `{ok, <altitude in meters>}`. This implements the algorithm found in the data sheet for the Bosch BMP180, but I'm not impressed with the results. It doesn't get close to the results I expect for today's test location in the U.S. Midwest. It is strongly affected by local air pressure variation. I guess that makes sense. Some stuff is "real".

All the development and testing of this software was done on a Raspberry PI model B using Erlang version 18. The sensor was an Adafruit Bosch BMP180 pressure sensor. 