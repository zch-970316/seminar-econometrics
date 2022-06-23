# pkg needed
library('tidyverse')
library("rmarkdown")
library('bookdown')
library('knitr')
library('tidyr')
library('dplyr')
library('stringr')
library("scales")
library("magrittr")
require("lubridate")

# project tools
require(renv)       # pkg management
require("openxlsx")
require("glue")
require("here")

# plot tool
library('ggplot2')
require("png")
require("jpeg")
require("rvg")
library("gridExtra")
require("ggthemes")
require("ggrepel")
require("latex2exp")

# data clean tool
require("janitor")

# data analysis
require("foreign")
require("haven")
require("wooldridge")

# tool for publish
require("officer")
require("officedown")
require("kableExtra")
require("DT")
library("flextable")

# html tools
library(webshot)
library(htmlwidgets)
#webshot::install_phantomjs(force = TRUE)
#webshot::is_phantomjs_installed()

# my custom pkg
require("xmerit")

