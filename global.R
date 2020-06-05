#
# This is the global declarative section of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load packages
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
library(plotly)
library(lubridate)
suppressPackageStartupMessages(library(googleVis))

# Load data to data frames
data.masterdata <- read.csv("epl-master-data.csv", header = TRUE, stringsAsFactors = FALSE)
data.leaguetable <- read.csv("epl-table-data.csv", header = TRUE, stringsAsFactors = FALSE)

# Rename columns in data.epl table and generate Year column
data.masterdata <- rename(data.masterdata, 'Team' = 'HomeTeam', 'Opponent' = 'AwayTeam') 










