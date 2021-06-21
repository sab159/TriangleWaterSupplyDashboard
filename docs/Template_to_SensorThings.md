WaterSupplyDashboard\_TemplateToSensorThings
================
Kyle Onda
6/20/2021

-   [Introduction](#introduction)
-   [Mapping the Template to
    SensorThings.](#mapping-the-template-to-sensorthings.)
-   [The Workflow (in Progress)](#the-workflow-in-progress)

# Introduction

This document elaborates the steps necessary to ingest data from Water
Supply Dashboard XLSX templates (see [blank
template](https://raw.githubusercontent.com/internetofwater/TriangleWaterSupplyDashboard/master/TEMPLATE-TWSD.xlsx),
and [example filled
template](https://carync.box.com/shared/static/l6zlh41b3kbqxn39s9yc048s3e1u9adj.xlsx)),
into an instance of the OGC SensorThings API with [Basic
Authentication](https://en.wikipedia.org/wiki/Basic_access_authentication).

The following libraries are required:

``` r
knitr::opts_chunk$set(echo = TRUE)
library(jsonlite)
library(httr)
library(readxl)
library(dplyr)
library(lubridate)
```

# Mapping the Template to SensorThings.

SensorThings API has a particular data model that we need to
conceptually map the template to before ingesting data. Our first draft
of this dashboard is essentially mapping 4 basic pieces of data from
utilities:

1.  Utility Metadata, such as its name, PWSID, location, and contact
    information, updated only as needed or annually. (template sheet
    `system_metadata`)
2.  Finished water deliveries at a daily timestep, updated weekly
    (template sheet `delivery`)
3.  Supply conditions at a daily timestep, updated weekly (generally
    available water storage capacity from relevant reservoirs).
    (template sheet `supply_conditions`)
4.  Active conservation status and associated policies, updated as
    needed. (template sheet `conservation_status`)

SensorThings has an complex data model:

![image](https://ogc-iot.github.io/ogc-iot-api/img/SensorThingsUML_Core.svg)
We model the data like so:

-   Each Utility shall be a `Thing`, identified viw the U.S. EPA SDWIS
    PWSID.
-   Each `Thing` shall be associated with 1 `Location`, which is its
    Service Area Boundary
-   Each `Thing` shall be associated with 2-3 `Datastream`s
-   The `Datastreams` are:
    -   Finished water deliveries (“\[PWSID\]-WaterDistributed”)
        -   `Sensor`: Report of delivered water (“DemandReport”)
        -   `ObservedProperty`: Water distributed for use by consumers
            of utility water (“WaterDistributed”)
        -   `unitOfMeasurement`: “Million Gallons per Day (MGD)”
    -   Water conservation status (“\[PWSID\]-ConservationStatus”)
        -   `Sensor`: Water Shortage Status form (“StageReport”)
        -   `ObservedProperty`: Phase of water shortage severity
            associated with appropriate responses for each phase
            (“ConservationStatus”)
        -   `unitOfMeasurement`: “Status”
    -   Storage Capacity (“\[PWSID\]-StorageCapacity”)
        -   `Sensor`: Water Shortage Status form (“StorageReport”)
        -   `ObservedProperty`: Percent of storage capacity available
            for distribution (“StorageCapacity”)
        -   `unitOfMeasurement`: “Percent”

# The Workflow (in Progress)

Below we describe and implement the full workflow, which is repeated for
all participating utilities in a loop on a regular schedule:

1.  Read in the `utility_registry.csv`
    [here](https://github.com/internetofwater/TriangleWaterSupplyDashboard/blob/master/utility_registry.csv)
    which matches a utility name and PWSID with the URL for its XLSX
    template.

2.  another thing

    1.  a diferent thing
    2.  ds sjksfs

3.  woohoo
