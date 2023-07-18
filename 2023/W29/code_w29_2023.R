library(tidyverse)
library(gt)

tuesdata <- tidytuesdayR::tt_load('2023-07-18')

detectors <- tuesdata$detectors

detectors

tibble(img = c("me_png.png",
               "chatgpt_png.png"),
       sentences = c("Hola",
                     "aaaa"))
