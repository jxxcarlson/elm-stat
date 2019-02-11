![Image](dataviewer.png)

Statistics and graphs for 2-column csv files — a start.  The example above is time series data.  I plan to add other data/plot types later, e.g. scatter plots.  Also, the ability to grab data directly from a URL.

The data used in the example above is in the file `temperature-anomalies.csv` in this repo. It is a list of global temperature anomalies for the period 1880-2017 from [www.climate.gov](http://www.climate.gov). The annual temperature anomaly is the difference between the global mean temperature and the long-term average global temperature. For this data set, the long-term average is computed for the period 1901-2000. Here is what the data looks like:

```
Global Land and Ocean Temperature Anomalies, January-December
Units: Degrees Celsius
Base Period: 1901-2000
Missing: -999
Year,Value
1880,-0.12
1881,-0.07
...
2015,0.91
2016,0.95
```
