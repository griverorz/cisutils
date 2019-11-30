#' Read data and JSON
#'
#' @export
#' @import haven jsonlite
input <- function(json, data) {
    rules <- fromJSON(readLines(json))
    
    ## Data
    data <- as.data.frame(read_spss(data))
    
    class(data) <- append(class(data), "cis")
    attributes(data)$json <- rules
    attributes(data)$renamed <- FALSE
    return(data)
}


#' Recode according to instructions
#' @export
recode_missing <- function(data) {
    json <- attributes(data)$json
    for (i in seq_along(json$variables)) {
        var <- names(json$variables)[i]
        misvals <- unlist(json$variables[[i]]$missing)
        if (!is.null(misvals)) {
            data[[var]][data[[var]] %in% misvals] <- NA
        }
    }
    return(data)
}

#' Rename variables
#' @export
rename_variables <- function(data) {
    json <- attributes(data)$json
    for (i in seq_along(json$variables)) {
        oldname <- names(json$variables)[i]
        newname <- json$variables[[i]]$label
        data[[newname]] <- data[[oldname]]
    }
    
    return(data)
}


#' Apply cleaning recodes
#' @export
#' @import dplyr
recode_values <- function(data) {
    json <- attributes(data)$json
    for (i in seq_along(json$variables)) {
        var <- names(json$variables)[i]
        data[[var]] <- as.integer(data[[var]])
        if (!is.null(json$variables[[i]]$recodes)) {
            data[[var]] <- recode(data[[var]],
                                  !!!json$variables[[i]]$recodes,
                                  .default="other")
        }
    }
    return(data)
}


#' Apply common renames
#' @export
#' @import dplyr
known_recodes <- function(data) {

    if (!all(c("gender", "age", "ccaa") %in% names(data))) {
        stop("Variables gender, age, and ccaa must exist in renamed data")
    }
    
    ## Gender
    data$gender <- recode(as.character(data$gender), "1"="man", "2"="woman")

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
    json <- attributes(data)$json
    for (i in seq_along(json$variables)) {
        var <- names(json$variables)[i]
        newtype <- json$variables[[i]]$type
        if (is.null(newtype)) {
            if ("character" %in% class(data[var])) {
                data[[var]] <- factor(data[[var]])
            }            
        } else {
            if (newtype == "numeric") {
                data[[var]] <- as.numeric(data[[var]])
            }
            if (newtype == "ordered") {
                data[[var]] <- factor(data[[var]], ordered=TRUE)
            }
        }                
    }

    return(data)
}


#' Keep only relevant variables
#' @export
subselect <- function(data) {
    json <- attributes(data)$json
    nn <- unlist(lapply(json$variables, function(x) x$label))
    data <- data[, nn]
    return(data)
}
