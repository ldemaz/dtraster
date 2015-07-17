# Overview

This package provides some tools that allow rasters to be converted to and from data.tables, in order to take advantage of the speed and memory management of the data.table package.  The initial idea (and one of the functions, with a few tweaks) come from a [gist](https://gist.github.com/etiennebr/9515738) by etiennebr. 

This package is very initial and rough, but will be built on. Some advantages for using data.tables to perform certain raster functions might be the following: 

  + substituting values from non-spatial data into very large rasters
  + more complex aggregations

On the latter point, `dt_aggregate` tries to accomplish this. It is still significantly slower then `raster::aggregate` on a larger raster in terms of performing a straight mean, but I wrote it because I couldn't figure out how to do a weighted mean aggregation with the raster package, so this was my solution. It should get faster with some attention. 

# Installation

```
devtools::install_github("ldemaz/dtraster")
library(dtraster)
```
