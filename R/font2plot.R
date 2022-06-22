## see url 
## https://gist.github.com/richarddmorey/27e74bcbf28190d150d266ae141f5117

library(showtext)
## Needed for the location of Candara on my system
font_paths("/System/Library/Fonts")
## Add two fonts, Avenir and Candara
font_add("avenir", regular = "Avenir.ttc", bold = "Avenir.ttc")
font_add("candara",
         regular = "Candara.ttf", 
         bold = "Candara Bold.ttf",
         italic = "Candara Italic.ttf",
         bolditalic = "Candara Bold Italic.ttf"
)

## Add hooks so that I can choose fonts in the chunk options         
knitr::knit_hooks$set(
  avenir = function(before, options, envir)
    if (before) par(family = "avenir"),
  candara = function(before, options, envir)
    if (before) par(family = "candara")
)
## set chunk options to use showtext
knitr::opts_chunk$set(fig.showtext=TRUE)
## Could also do
# knitr::opts_chunk$set(fig.showtext=TRUE, candara = TRUE)
## to always use candara
