# psrcelmer
R utilities for interacting with Elmer and ElmerGeo

This package provides a few single-line tools to retrieve data sets from PSRC's data warehouse databases *Elmer* and *ElmerGeo*.  

## What is *Elmer*?

Elmer is a database containing the agency's general-use tabular data.  We are constantly expanding its contents, which currently ranges from demographic, employment, aviation, parking, and other survey data.  

## What is *ElmerGeo*?

ElmerGeo is PSRC's home for geospatial data for use by staff.  In the past we used shapefiles for our spatial data, but today we store these as feature classes within a geodatabase.  At PSRC, the database we use for this purpose is named *ElmerGeo*.  


## Installation Instructions

You can install the package with the following command: 
```
  devtools::install_github("psrc/psrcelmer")
```

Then load it with:
```
  library(psrcelmer)
```

## Database Authentication

To use this package you will first need a login to the SQL Server *Sockeye* (at AWS-PROD-SQL\\Sockeye).  See the agency's IT staff if you need a login created, or if have questions about this.  

If you are on a Windows machine, you should now have all you need in order to connect and retrieve data from both Elmer and ElmerGeo. This is because your Windows account can authenticate you seamlessly, without requiring you to supply another username and password.

If you are on a Mac, Linux box or TRS-80 you will need provide a username and password.  To do this, you will need to set up two environment variables on your computer.  These variables are `SOCKEYE_UID` and `SOCKEYE_PWD`.  So, if your SQL username were "some_username" and your password were "some_passwd", you could set these as so, in R:
```
  Sys.setenv('SOCKEYE_UID' = 'some_username', 
             'SOCKEYE_PWD' = 'some_passwd')
```

Once these are set, you should be able to connect and run the functions in this package.  
