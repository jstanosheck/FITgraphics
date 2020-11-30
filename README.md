# FITgraphics


## Required packages to be downloaded from GitHub

This package requires the [FITfileR](https://github.com/grimbough/FITfileR) package to properly work. 

### Downloading FITfileR package in Rstudio

In the Rstudio Console this is the method for downlading the FITfileR package:

```R

#Install Devtools package if not yet installed on machine
install.packages("devtools")

#Install package from GitHub using devtools
devtools::install_github("grimbough/FITfileR")

#Load Library in your console
library(FITfileR)

```

When using a wearable activity device (Garmin or Apple Watch) or a cycling computer, the files
that are generated from activities are .FIT files. These files contain data that is recorded during
the activity such as heart rate, elevation, speed, GPS data, etc. In general, all of the platforms that
manufacture these devices have a native platform to see the data from these files. There are also
several R packages that can extract the data from .FIT files to be used in R, but there is no
package that I have found that is able to manipulate these data to show them in a nice form like
the native Garmin or Apple apps.


## Intended Functionality:

The intent behind this package will be to read in the .FIT files and give the user the opportunity
to explore their data in a meaningful way, similar to what is experienced in native apps. This
package heavily relies on ggplot2, dplyr, and plotly.
