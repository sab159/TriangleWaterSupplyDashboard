# TriangleWaterSupplyDashboard
Workflows for getting data into Triangle Water Supply Dashboard

## Components

### 1. Data Upload

Participants will maintain system of spreadsheet templates that can be maintained on any web-accessible location via an HTTP or FTP request. Examples include:

* Google Sheets (with View link enabled)
* Dropbox (with View link enabled)
* Box (with View link enabled)


### 2. SensorThings Server

The Water Supply Dashboard administrator will operate an instance of a SensorThings API and database. For the pilot implementation, the SensorThings API is accessible at https://twsd.internetofwater.dev/api/v1.1

### 3. R container

The Water Supply Dashboard will operate an R docker image that serves two functions:

A. It periodically checks the template worksheets for updates and issues HTTP POST requests to the SensorThings Server
B. It downloads data from the SensorThings Server and any external APIs (e.g. USGS, USACE) as necessary, and processes them for visualization by the dashboard.

[Docker setup desribed here](https://www.bioconductor.org/packages/release/bioc/vignettes/sevenbridges/inst/doc/docker.html#1_Introduction)

### 4. Dashboard 

The user-facing Dashboard itself is a standalone javascript and html web page