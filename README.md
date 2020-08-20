# Elm-stat

The `elm-stat` package provides tools for statistics and visualization of n-column data files.
Most of the features of the package are exposed in the the demo apps, found in the
`./examples` folder of the source code. The package is intended to be a long-term research and development effort which will gradually add features over time. I'd be very interested in finding collaborators with some background in mathematics and/or statistics.

## The API

The `elm-stat` package consists of seven modules divided into three groups

### Statistical Functions

- `StatRandom` provides commonly used probability distributions
- `Stat` provides many common functions used in statistics

### Data

- `RawData` is used for extracting raw data from a text string or file: metadata such as comments about the data, column headings, and an `m*n` table of strings representing the actual data.

- `SampleData` provides several data sets: Global temperature anomalies, sea level data, Edwin Hubble's galactic recession data, and historic speed of light data


### Charts  

- `Chart` provides tools for building graphs and charts. A chart consists of one more graphs.
Graphs come in various types, e.g, line and scatter.

- `Data`  provides types and functions for
extracting lists of 2D points both from strings and
`RawData` values.  This module is used in `Chart`

- `ErrorBars` provides utilities for drawing error bars.  Used in `Chart`.


## Example usage

Let's import the above modules to see how some of this works. For more details, see the documentation for the individual modules. First, some test data:

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
## Demo Apps

There are three examples in `./src`.  

- `Main.elm` provides usage examples for the functions in the modules `Stat` and `StatRandom`

- `Hubble.elm` provides a scatter plot and regression line for Edwin Hubble's 1929 galactic recession data.

- `Temperature.elm` provides a line graph and regression line for global temperature anomalies for the period
1880-2016. The temperature anomaly for a given year is the difference between the mean temperature for the year and the mean global temperature for a reference period.


Code for the demo apps is in `./examples`. Compile the first example by saying

```
elm make src/Main.elm
```

then opening `index.html` in a browser.

## Note  

This is a major update to version 4.0.3.  The API for the statistical functions in module `Stat`
has been completely rewritten, and a new module, `StatRandom`, featuring generators for many common statistical
packages has been added.  Both are contributions of Raul Fleischman (FleischmanRaul on Github)
