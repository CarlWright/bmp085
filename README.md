# bmp085 (A Erlang app to measure air temperature and pressure)

This erlang application works on the Raspberry PI using the "erlang_ale" library to interface using I2C to the BMP085 device.

## Initialization

Before you can get air temperatures or pressures, you need to start a process to communicate with the BMP085 device. Call `bmp085:start_link()` and get `{ok, <sensor pid>}`. Use the <sensor pid> value in all your calls to get temperaturs and pressures.

## Air temperature

When you call `bmp085:read_temp(Sensor_PID)`, you get `{ok,<temperature in Celsius>, <temperature in Fahrenheit>}`.

For example, I called it now and got `{ok,21.51,70.72}`.

## Air pressure

When you call `bmp085::read_pressure(Sensor_PID)`, you get `{ok, <air pressure in Pa>}`. 1 Pa equals 0.01hPa or 0.01mbar.

For example, I called it now and got `{ok,98958}`.