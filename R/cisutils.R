#' Read data and JSON
#' @export
#' @import haven jsonlite
input <- function(json, data) {
    rules <- fromJSON(readLines(json))
    
    ## Data
    data <- read_spss(data)
    
    valid_instructions(rules, names(data))
    class(data) <- append(class(data), "cis")
    attributes(data)$json <- rules
    attributes(data)$renamed <- FALSE
    return(data)
}


#' Test JSON
#' @export
valid_instructions <- function(x, ndata) {
    
    if (length(setdiff(names(x$variables), ndata)) != 0) {
        stop("Variables in the JSON file do not exist in the data")
    }
    
    if(all(names(x$variables) != names(x$missing))) {
        stop("Mismatch between defined and missing variables")
    }
    
    return(NULL)    
}


#' Recode according to instructions
#' @export
recode_missings <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }
    
    x <- attr(data, "json")
    
    for (i in 1:length(x$variables)) {
        misvals <- unlist(x$missing[i])
        varname <- names(x$missing)[i]
        
        if (!is.null(misvals)) {
            data[[varname]][data[[varname]] %in% misvals] <- NA
        }
    }
    
    return(data)
}


#' Rename variables
#' @export
rename_variables <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }
    
    x <- attr(data, "json")

    for (i in 1:length(x$variables)) {
        oldname <- names(x$variables)[i]
        newname <- unlist(x$variables[i])
        data[[newname]] <- data[[oldname]]
    }
    
    attributes(data)$renamed <- TRUE
    return(data)
}


#' Apply cleaning recodes
#' @export
#' @import dplyr
recode_values <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }

    if (!attr(data, "renamed")) {
        stop("Must rename variables first")
    }
    
    x <- attr(data, "json")

    for (i in 1:length(x$recodes)) {
        varname <- names(x$recodes)[i]
        data[[varname]] <- as.integer(data[[varname]])
        data[[varname]] <- recode(data[[varname]], !!!x$recodes[[varname]], .default="other")
    }
    return(data)
}


#' Apply common renames
#' @export
#' @import dplyr
known_recodes <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }
    
    if (!attr(data, "renamed")) {
        stop("Must rename variables first")
    }
    
    x <- attr(data, "json")

    if (!all(c("gender", "age", "ccaa") %in% names(data))) {
        stop("Variables gender, age, and ccaa must exist in renamed data")
    }
    
    ## Gender
    data$gender <- recode(as.numeric(data$gender), "1"="man", "2"="woman")

    ## Age
    data$age[data$age %in% c(0:24)] <- "age24"
    data$age[data$age %in% c(24:34)] <- "age25t34"
    data$age[data$age %in% c(35:44)] <- "age35t44"
    data$age[data$age %in% c(45:54)] <- "age45t54"
    data$age[data$age %in% c(55:64)] <- "age55t64"
    data$age[data$age %in% c(65:1000)] <- "age65"
    
    ## CCAA
    data$ccaa <- as_factor(data$ccaa)
    levels(data$ccaa) <- c("andalucia",
                          "aragon",
                          "asturias",
                          "balears",
                          "canarias",
                          "cantabria",
                          "castilla-lamancha",
                          "castilla-leon",
                          "cataluna",
                          "valencia",
                          "extremadura",
                          "galicia",
                          "madrid",
                          "murcia",
                          "navarra",
                          "paisvasco",
                          "rioja",
                          "ceuta",
                          "melilla")
    return(data)
}


#' Ensure appropriate types
#' @export
retype <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }
    
    if (!attr(data, "renamed")) {
        stop("Must rename variables first")
    }
    
    x <- attr(data, "json")

    for (i in 1:length(x$types)) {
        varname <- names(x$types)[i]
        newtype <- unlist(x$types[i])
        if (newtype == "numeric") {
            data[[varname]] <- as.numeric(data[[varname]])
        }
        if (newtype == "ordered") {
            data[[varname]] <- factor(data[[varname]], ordered=TRUE)
        }
    }

    for (i in 1:ncol(data)) {
        if ("character" %in% class(data[, i])) {
            data[, i] <- factor(data[, i])
        }
    }

    return(data)
}


#' Keep only relevant variables
#' @export
subselect <- function(data) {
    if (!"cis" %in% class(data)) {
        stop("Only allows CIS data")
    }
    
    if (!attr(data, "renamed")) {
        stop("Must rename variables first")
    }
    
    x <- attr(data, "json")

    data <- data[, unlist(x$variables)]
    return(data)
}
