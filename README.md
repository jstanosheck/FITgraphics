#FITgraphics

When using a wearable activity device (garmin or apple watch) or a cycling computer, the files
that are generated from activities are .FIT files. These files contain data that is recorded during
the activity such as heart rate, elevation, speed, GPS data, etc. In general, all of the platforms that
manufacture these devices have a native platform to see the data from these files. There are also
several R packages that can extract the data from .FIT files to be used in R, but there is no
package that I have found that is able to manipulate these data to show them in a nice form like
the native Garmin or Apple apps.
Intended Functionality:
The intent behind this package will be to read in the .FIT files and give the user the opportunity
to explore their data in a meaningful way, similar to what is experienced in native apps. This
package will rely heavily on ggplots2 package and potentially other graphical packages. 
