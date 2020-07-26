# Elm-stat

The `elm-stat` package provides tools for statistics and visualization of n-column data files. 
Most of the features of the package are exposed in the [Demo App](https://jxxcarlson.github.io/app/dataviewer.html) (work in progress), the code for which is in `./examples`. 
The package is intended to be a long-term research and development effort which will gradually add features over time. I'd be very interested in finding collaborators with some background in mathematics and/or statistics.

## The API

The `elm-stat` package consists of three modules,

- `StatRandom`, provides commonly used probability distributions
- `Stat`, provides a number of common functions useful in statistics
- `RawData`, for extracting raw data from a text string or file: metadata such as comments about the data, column headings, and an `m*n` table of strings representing the actual data.

## Example usage

There is also a module `SampleData` with some test data. Let's import the above modules to see how some of this works. For more details, see the documentation for the individual modules.

First, some test data:

```
> SampleData.temperature
 """
 Global Land and Ocean Temperature Anomalies
 January-December 1880-2016
 Units: Degrees Celsius
 Base Period: 1901-2000
 Missing: -999
  Year  Value
  1880  -0.12
  1881  -0.07
  1882  -0.08
  1883  -0.15
  ...
```

Transform this text to a RawData value using `RawData.get`, a function which automatically detects the type of data represented by the text: space-delimited, tab-delimited, or comma-delimited (csv).

```
> data = RawData.get SampleData.temperature
 Just {
  columnHeaders = ["Year","Value"]
 , data = [["1880","-0.12"],["1881","-0.07"],["1882","-0.08"], ...
 , metadata = [
   "Global Land and Ocean Temperature Anomalies"
   , "January-December 1880-2016"
   , "Units: Degrees Celsius"
  ]
```

Extract the first column of the data which represents the temperature anomalies:

```
> tempAnom = RawData.getColumn data 1
  [-0.12,-0.07,-0.08,-0.15,-0.22,-0.22,-0.21, ...
```

Statistics can be computed for the `tempAnom` list:

```
> Stat.mean tempAnom
Just 0.048759 : Maybe Float

> Stat.median tempAnom
Just -0.05 : Maybe Float

> Stat.standardDeviation tempAnom
Just 0.31324402291377174 : Maybe Float
```

Extract two columns as a tuple list: 

```
> yearTemp = RawData.toData 0 1 (Maybe.withDefault (RawData.RawData [] [] [[]]) data)
[(1880,-0.12),(1881,-0.07),(1882,-0.08),(1883,-0.15),(1884,-0.22),(1885,-0.22),(1886,-0.21)...

```

Calculate correlation between the years and temperature anomalies:

```
> Stat.correlation yearTemp
Just 0.8635859634657297 : Maybe Float

```
## The Demo App

Code for the demo app is in `./examples`. There is an online version at [Data explorer](https://jxxcarlson.github.io/app/dataviewer.html).

The data used in the example in the file `data/temperature-anomalies.csv` in this repo. It is a list of global temperature anomalies for the period 1880-2017 from [www.climate.gov](http://www.climate.gov). The annual temperature anomaly is the difference between the global mean temperature and the long-term mean global temperature. For this data set, the long-term mean is computed for the period 1901-2000.
