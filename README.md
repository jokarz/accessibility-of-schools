# Burp

An interactive geospatial application that maps out the accessibility of schools (excluding tertiary schools and institutions) from public HDB in Singapore (Best viewed on desktop/laptop)

![alt text](https://github.com/jokarz/Burp/raw/master/ss.jpg)
**_Live at [here](https://burp.shinyapps.io/proj)_**

# Content
* Getting Started
* Addition data
* Running backend server
* Addition resources
* Credits

## Getting Started

All the data are generated beforehand to make it feasible to be uploaded to shinyapps. Thus if one wants to run the backend to generate their own data, consider looking at running backend server section

1. Clone this repository
2. Install [RStudio](https://rstudio.com/)
3. Open this repository in RStudio and press the Run App button

## Addition data

Not all data that are present on the live application is available inside this repository, if you really needed it (for school projects and such), feel free to contact me on [Telegram](https://t.me/pengpengg)

## Running the backend server

1. Make sure Java JDK (JDK 8 preferrably) is installed
2. Make sure the local port 8080 is not used
3. Run the following code in terminal/powershell/console from the root dir of this repo
```
cd server
java -Xmx2G -jar otp.jar --router current --graphs graphs --server
```
4. Have a look at the ```generator.Rmd``` to understand how the data are called and stored

## Addition resources

The backend server runs a local OpenTripPlanner server. Take a look at their api documentation [here](https://dev.opentripplanner.org/apidoc/1.3.0/) 

The isochrone generation was heavily inspired by https://xang1234.github.io/isochrone/. Take a look at the detailed explanation on the data that are generated/required to created an isochrone overlay

## Credits

GTFS - https://transit.land/feed-registry/

Isochrone - https://xang1234.github.io/isochrone/

Server - https://www.opentripplanner.org/

Measures of accessibility (good read) - https://upcommons.upc.edu/bitstream/handle/2099.1/6327/03.pdf

Data - https://docs.onemap.sg/, https://data.gov.sg/