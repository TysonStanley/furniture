## Utilities for the furniture package

## Output type constructor
.type_constructor = function(type){
  if (any(grepl("simp", type)) && any(grepl("cond", type))){
    simple <- TRUE
    condense <- TRUE
  } else if (any(grepl("cond", type))){
    simple <- FALSE
    condense <- TRUE
  } else if (any(grepl("simp", type))){
    simple <- TRUE
    condense <- FALSE
  } else {
    simple <- FALSE
    condense <- FALSE
  }
  list(condense, simple)
}

## Header Labels checker
.header_labels = function(header_labels, format_output){
  if(!is.null(header_labels)){
    if (grepl("f|F", format_output)) { 
      length_labels <- 3
    } else if (grepl("p|P", format_output)) { 
      length_labels <- 2
    } else if (grepl("s|S", format_output)) { 
      length_labels <- 1
    } else {
      stop("Type must be one of 'full', 'pvalues', or 'stars'.")
    }
    
    if (length_labels != length(header_labels)){
      stop("header_labels must match the length of adjustable header values.")
    }
  }
}

## More than one value per variable warning
.more_than_one_value <- function(data){
  subset(data, select=-split) %>%
    lapply(function(x) length(unique(x)) > 1) %>%
    unlist() %>%
    all()
}

## Observations and Header Labels
.obs_header = function(d, f1, format_output, test, output, header_labels, total){
  
  if (isTRUE(total)){
    tot <- NROW(d[[1]])
    nams <- c(" ", "Total", levels(d$split))
    
    if (!is.null(header_labels)){
      header_labels <- c(header_labels[1], "Total", header_labels[2:length(header_labels)])
    }
    
  } else {
    tot <- NULL
    nams <- c(" ", levels(d$split))
  }
  
  N   <- c("Total" = tot, tapply(d[[1]], d$split, length))
  N[] <- sapply(N, function(x) as.character(paste("n =", x)))
  N   <- suppressWarnings(formatC(N, big.mark = f1, digits = 0, format = "f")) %>%
    sapply(trimws, which = "left") %>%
    t(.)
  
  ## Formatting the N line
  if (grepl("f|F", format_output) & test){
    if (is.null(header_labels)){
      header_labels <- c(nams, "Test", "P-Value")
      N <- data.frame("", N, "", "", stringsAsFactors = TRUE)
      names(N) <- header_labels
    } else {
      N <- data.frame("", N, "", "", stringsAsFactors = TRUE)
      names(N) <- c(header_labels[1], levels(d$split), header_labels[2:length(header_labels)])
    }
  } else if ((grepl("p|P", format_output) || grepl("s|S", format_output)) && test){
    N <- data.frame(" ", N, " ", stringsAsFactors = TRUE) 
    if (grepl("p|P", format_output)){
      if (is.null(header_labels)){
        header_labels <- c(nams, "P-Value")
        names(N) <- header_labels
      } else {
        names(N) <- c(header_labels[1], levels(d$split), header_labels[2:length(header_labels)])
      }
    } else {
      if (is.null(header_labels)){
        header_labels <- c(nams, " ")
        names(N) <- header_labels
      } else {
        names(N) <- c(header_labels[1], levels(d$split), header_labels[2:length(header_labels)])
      }
      
    }
  } else {
    if (is.null(header_labels)){
      header_labels <- nams
      N <- data.frame("", N, stringsAsFactors = TRUE)
      names(N) <- header_labels
    } else {
      N <- data.frame("", N, stringsAsFactors = TRUE)
      names(N) <- c(header_labels[1], levels(d$split))
    }

  }
  N[] <- sapply(N, as.character)
  N
}



## Formatting for default summaries
.summary_functions1 = function(FUN, format_number, digits){
  if (format_number){
    f1 <- ","
  } else {
    f1 <- ""
  }
  ## Primary Function
  if(is.null(FUN)){
    num_fun <- function(x){
      gettextf("%s (%s)",
               formatC(mean(x, na.rm=TRUE), big.mark = f1, digits = digits, format = "f"),
               formatC(sd(x, na.rm=TRUE),   big.mark = f1, digits = digits, format = "f"))
    }
  } else {
    num_fun <- FUN
  }
  return(num_fun)
}
.summary_functions2 = function(FUN2, format_number, digits){
  if (format_number){
    f1 <- ","
  } else {
    f1 <- ""
  }
  ## Secondary Function
  if(is.null(FUN2)){
    num_fun2 <- function(x){
      gettextf("%s [%s]",
               formatC(median(x, na.rm=TRUE), big.mark = f1, digits = digits, format = "f"),
               formatC(IQR(x, na.rm=TRUE),    big.mark = f1, digits = digits, format = "f"))
    }
  } else {
    num_fun2 <- FUN2
  }
  return(num_fun2)
}


## Pipe
`%>%` <- magrittr::`%>%`

## Group by
group_by <- dplyr::group_by

