![Image](image/dataviewer.png)

This package is for doing statistics and graphs for 2-column csv files.  
The image above displays an analysis of time series data.
It is computed by the app in the `examples` folder using this `elm-stat` library.  See
the [Demo App](https://jxxcarlson.github.io/app/dataviewer.html) to try the app out on line.

The data used in the example in the file `temperature-anomalies.csv` in this repo. It is a list of global temperature anomalies for the period 1880-2017 from [www.climate.gov](http://www.climate.gov). The annual temperature anomaly is the difference between the global mean temperature and the long-term mean global temperature. For this data set, the long-term mean is computed for the period 1901-2000. Here is what the data looks like:

```
Global Land and Ocean Temperature Anomalies, January-December
Units: Degrees Celsius
Base Period: 1901-2000
Year,Value
1880,-0.12
1881,-0.07
...
2015,0.91
2016,0.95
```
