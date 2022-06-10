# see ref [url](https://github.com/yihui/xaringan/issues/26)
options(htmltools.dir.version = FALSE)
library(RefManageR)
library(bibtex)
BibOptions(check.entries = FALSE, bib.style = "authoryear", style = "markdown",
           dashed = TRUE)
file.name <- here("bib/causality-analysis.bib")
bib <- ReadBib(file.name)

# for inline reference#

#`r Citet(bib, "loh")`

#`r Citep(bib, "salam")`


#  xariagan slide at the end

#---
  
## References
  
#```{r, results='asis', echo=FALSE}
#PrintBibliography(bib)
#```

# another useage 
## see raw code [url](https://github.com/jvcasillas/xaringan_bib/blob/master/helper.R)
## xaringan demo [url](https://www.jvcasillas.com/xaringan_bib/index.html#3)


print_bib_rmd <- function(bib, .opts = list(), start = 1, stop = NULL, decreasing = FALSE) {
  
  bib <- sort(bib, decreasing = FALSE)
  
  if (!length(bib)) {
    return(bib)
  }
  
  if (identical(class(bib), "bibentry")) {
    bib <- as.BibEntry(bib)
  }
  
  keys <- unlist(bib$key)
  ind <- keys %in% names(.cites$indices)
  
  if (!any(ind)) {
    message("You haven't cited any references in this bibliography yet.")
    return()
  }
  
  if (length(.opts$bib.style)) {
    bibstyle <- .opts$bib.style
  } else {
    bibstyle <- .BibOptions$bib.style
  }
  
  if (length(.opts$cite.style)) {
    citestyle <- .opts$cite.style
  } else {
    citestyle <- .BibOptions$cite.style
  }
  
  if (length(.opts$style)) {
    style <- .opts$style
  } else {
    style <- .BibOptions$style
  }
  
  bib <- bib[[ind]] # gets citations to print
  
  if (bibstyle == citestyle) {
    if (bibstyle == "numeric") {
      if (length(bib) == length(.cites$labs)) {
        bib <- bib[[names(.cites$labs)]]
        .opts$sorting <- "none"
        bib$.index <- structure(.cites$labs, names = NULL)
      }
    } else { 
      bib$.index <- .cites$labs[keys[ind]]
    }
  }
  
  if (length(.opts)) {
    old.opts <- BibOptions(.opts)
    on.exit(BibOptions(old.opts))
  }
  
  if (style == "yaml") {
    cat("\n---\nnocite:", sQuote(paste0(paste0("@", names(.cites$indices)), 
                                        collapse = ", ")))
    cat("\n...  \n\n")
  }
  if (is.null(stop)) {
    stop <- length(bib)
  } 
  bib <- bib[start:stop]
  print(bib)
}

environment(print_bib_rmd) <- asNamespace("RefManageR")