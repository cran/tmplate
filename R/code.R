translate <-
function (vars, ..., template, drop = FALSE, default = "NULL", 
    warn = TRUE, start = "<:", end = ":>", redo = 2, envir = new.env(parent = parent.frame()), 
    retrans = 2, debug = FALSE) 
{
    if (is.null(debug)) 
        debug <- FALSE
    if (debug) 
        browser()
    if (any(file.exists(template))) 
        template <- readLines(template)
    if (length(template) == 1) 
        template <- strsplit(template, split = "\n", fixed = TRUE)[[1]]
    if (missing(vars)) 
        vars <- NULL
    if (is.null(vars)) 
        vars <- list(...)
    if (!identical(vars, list(...))) {
        dots <- list(...)
        ndots <- names(dots)
        if (length(dots) > 0 & !is.null(ndots)) {
            for (d in 1:length(dots)) {
                if (any(grepl(paste("^", ndots[d], "$", sep = "", 
                  collapse = ""), names(vars)))) {
                  vars[[ndots[d]]] <- dots[[d]]
                }
            }
        }
    }
    nvars <- names(vars)
    m <- match.call(expand.dots = TRUE)
    requireNamespace("tRnslate", quietly = TRUE)
    if (length(vars) > 0) {
        do_again = TRUE
        step <- 0
        while (do_again) {
            for (i in 1:length(vars)) {
                if (!is.null(vars[[i]])) {
                  aux <- vars
                  aux$template <- vars[[i]]
                  aux$drop <- FALSE
                  aux$default <- default
                  aux$warn <- FALSE
                  aux$envir <- envir
                  vars[[i]] <- do.call("translate_template", 
                    aux)
                }
            }
            for (j in 1:length(vars)) {
                if (!is.null(vars[[j]])) 
                  vars[[j]] <- tRnslate::translate_r_code(x = vars[[j]], 
                    char_begin = "", char_clean = "<:NULL:>", 
                    char_drop = NULL, envir = envir, comments = TRUE, 
                    debug = debug)
            }
            step <- step + 1
            if (step == retrans) 
                do_again <- FALSE
        }
    }
    do_again = TRUE
    step <- 0
    while (do_again) {
        aux <- vars
        aux$template <- template
        aux$drop <- ifelse(step == retrans - 1, drop, FALSE)
        aux$default <- default
        aux$warn <- ifelse(step == retrans - 1, warn, FALSE)
        template <- do.call("translate_template", aux)
        template <- tRnslate::translate_r_code(x = template, 
            char_begin = "", char_clean = "<:NULL:>", char_drop = NULL, 
            envir = envir, comments = TRUE, debug = debug)
        step <- step + 1
        if (step == retrans) 
            do_again <- FALSE
    }
    if (drop) {
        e <- grepl("<:NULL:>", template)
        if (any(e)) 
            template <- template[!e]
    }
    return(template)
}
translate_template <-
function (vars, ..., template, drop = FALSE, default = "NULL", 
    warn = TRUE, start = "<:", end = ":>", redo = 2, allow_file = FALSE) 
{
    if (missing(vars)) 
        vars <- NULL
    if (is.null(vars)) 
        vars <- list(...)
    if (!identical(vars, list(...))) {
        dots <- list(...)
        ndots <- names(dots)
        if (length(dots) > 0 & !is.null(ndots)) {
            for (d in 1:length(dots)) {
                if (any(grepl(paste("^", ndots[d], "$", sep = "", 
                  collapse = ""), names(vars)))) {
                  vars[[ndots[d]]] <- dots[[d]]
                }
            }
        }
    }
    x <- as.character(template)
    if (allow_file) {
        if (any(file.exists(template))) 
            x <- readLines(template)
        else x <- template
    }
    if (length(x) == 1) 
        x <- strsplit(x, split = "\n", fixed = TRUE)[[1]]
    pe <- grepl("^[[:blank:]]*$", x)
    x[pe] <- "<:@@BLANK@@:>"
    nvars <- names(vars)
    patt_all <- paste("(", paste(nvars, sep = "", collapse = "|"), 
        ")", sep = "", collapse = "")
    if (length(nvars) > 0 & any(grepl(patt_all, x))) {
        x <- lapply(x, c)
        toadd <- FALSE
        once_again <- TRUE
        step <- 0
        while (once_again) {
            for (i in 1:length(nvars)) {
                patt <- paste(start, "[[:blank:]]*", nvars[i], 
                  "[[:blank:]]*", end, sep = "", collapse = "")
                L <- grepl(patt, x)
                p <- c(1:length(x))[L]
                if (any(L)) {
                  use_var <- vars[[i]]
                  if (is.null(use_var) & !is.null(default)) 
                    use_var <- default
                  replace <- paste(use_var, sep = "", collapse = "<%%::split::%%>")
                  xp <- x[p]
                  for (j in 1:length(p)) {
                    aux <- xp[[j]]
                    aux <- gsub(patt, replace, aux)
                    if (length(vars[[i]]) > 1) 
                      toadd <- TRUE
                    xp[[j]] <- aux
                  }
                  x[p] <- xp
                }
            }
            step <- step + 1
            if (step == redo) 
                once_again <- FALSE
        }
        if (toadd) {
            for (k in 1:length(x)) x[[k]] <- strsplit(x[[k]], 
                split = "<%%::split::%%>")[[1]]
        }
    }
    x <- unlist(x)
    if (drop) {
        todrop <- grepl(paste(start, "[[:blank:]]*[[:alnum:]_]+[[:blank:]]*", 
            end, sep = "", collapse = ""), x)
        if (any(todrop)) 
            x <- x[!todrop]
    }
    if (warn) {
        if (any(grepl(paste(start, "[[:blank:]]*[[:alnum:]_]+[[:blank:]]*", 
            end, sep = "", collapse = ""), x))) 
            warning("Not all variables has been replaced in template. It may not work!")
    }
    pe <- grepl("^<:@@BLANK@@:>$", x)
    if (any(pe)) 
        x[pe] <- ""
    return(x)
}
translate_template0 <-
function (..., template, drop = FALSE, default = "NULL", warn = TRUE, 
    start = "<:", end = ":>", allow_file = FALSE) 
{
    vars <- list(...)
    x <- as.character(template)
    if (allow_file) {
        if (any(file.exists(template))) 
            x <- readLines(template)
        else x <- template
    }
    if (length(x) == 1) 
        x <- strsplit(x, split = "\n", fixed = TRUE)[[1]]
    nvars <- names(vars)
    patt_all <- paste("(", paste(nvars, sep = "", collapse = "|"), 
        ")", sep = "", collapse = "")
    if (length(nvars) > 0 & any(grepl(patt_all, x))) {
        x <- lapply(x, c)
        toadd <- FALSE
        for (i in 1:length(nvars)) {
            patt <- paste(start, "[[:blank:]]*", nvars[i], "[[:blank:]]*", 
                end, sep = "", collapse = "")
            L <- grepl(patt, x)
            p <- c(1:length(x))[L]
            if (any(L)) {
                use_var <- vars[[i]]
                if (is.null(use_var) & !is.null(default)) 
                  use_var <- default
                replace <- paste(use_var, sep = "", collapse = "<%%::split::%%>")
                xp <- x[p]
                for (j in 1:length(p)) {
                  aux <- xp[[j]]
                  aux <- gsub(patt, replace, aux)
                  if (length(vars[[i]]) > 1) 
                    toadd <- TRUE
                  xp[[j]] <- aux
                }
                x[p] <- xp
            }
        }
        if (toadd) {
            for (k in 1:length(x)) x[[k]] <- strsplit(x[[k]], 
                split = "<%%::split::%%>")[[1]]
        }
    }
    x <- unlist(x)
    if (drop) {
        todrop <- grepl(paste(start, "[[:blank:]]*[[:alnum:]_]+[[:blank:]]*", 
            end, sep = "", collapse = ""), x)
        if (any(todrop)) 
            x <- x[!todrop]
    }
    if (warn) {
        if (any(grepl(paste(start, "[[:blank:]]*[[:alnum:]_]+[[:blank:]]*", 
            end, sep = "", collapse = ""), x))) 
            warning("Not all variables has been replaced in template. It may not work!")
    }
    return(x)
}
