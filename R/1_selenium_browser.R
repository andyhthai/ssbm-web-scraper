library(tidyverse)
library(rvest)
library(janitor)
library(RSelenium)
library(netstat)
library(data.table)


# Browser settings for Selenium -------------------------------------------


extraCapabilities <- list("moz:firefoxOptions" = list(args = list("--headless")))

create_firefox_driver <- function(headless = FALSE) {
  if (headless) {
    rD <- rsDriver(
      browser = "firefox",
      verbose = FALSE,
      check = FALSE,
      chromever = NULL,
      port = free_port(),
      extraCapabilities = list("moz:firefoxOptions" = list(args = list("--headless")))
    )
  } else {
    rD <- rsDriver(
      browser = "firefox",
      verbose = FALSE,
      check = FALSE,
      chromever = NULL,
      port = free_port()
    )
  }
}

# Headless browser; run rD <- create_firefox_driver() if you don't want headless
rD <- create_firefox_driver(headless = TRUE)

firefox <- rD[["client"]]

firefox$open()

