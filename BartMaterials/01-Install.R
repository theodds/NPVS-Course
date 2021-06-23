#' Packages required by us which are non-BART related are:

library(tidyverse)
library(caret)

#' Most reliably-installable package which implements our variable selection
#' methods

install.packages("BART")
library(BART)

#' dartMachine is faster, but requires rJava and can be trickier to install
#' in that you might have to install other stuff on top of it. The package
#' can be downloaded at www.github.com/theodds/dartMachine. After downloading
#' and installing rJava, you can install by navigating to the folder dartMachine
#' and installing as you would any local package from source.

options(java.parameters = "-Xmx4g") ## This is to ensure the package has enough
                                    ## memory to work with.
library(dartMachine)

#' An alternative to dartMachine which is on CRAN (but no fully-Bayes variable
#' selection) is bartMachine, which can be installed in the usual way, but
#' requires rJava.
#' 
#' IMPORTANT: You should not have bartMachine and dartMachine loaded at the
#' same time

options(java.parameters = "-Xmx4g") ## This is to ensure the package has enough
                                    ## memory to work with.
library(bartMachine)

#' SoftBart is another option, essentially a fancier version of BART which 
#' performs better on some problems. 

library(devtools)
install_github("theodds/SoftBART")
library(SoftBart)

#' A completely different approach to variable selection, which has also been
#' used with BART, is a wrapper method which looks at how well we can
#' "fit-the-fit" using a surrogate model which eliminates some of the varibales.
#' It's not on CRAN, but can be downloaded by running the following commands.

install.packages('foreach', dep = T)
url <- 'http://www.rob-mcculloch.org/chm/nonlinvarsel_0.0.1.9001.tar.gz'
download.file(url, destfile = 'temp')
install.packages('temp', repos = NULL, type='source')