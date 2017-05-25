
jmvOmegaClass <- R6::R6Class(
    "jmvOmegaClass",
    inherit = jmvOmegaBase,
    private = list(
        .run = function() {
            
            if (length(self$options$vars) < 2) {
                self$results$omega$setContent("Please specify at least two variables")
                return()
            }
            
            data <- self$data
            
            for (name in names(data)) {
                var <- data[[name]]
                if ( ! jmvcore::canBeNumeric(var))
                    stop("Variable '", name, "' is not numeric")
                data[[name]] <- jmvcore::toNumeric(var)
            }
            
            data <- na.omit(data)
            
            nfactors <- self$options$nfactors
            fm       <- self$options$fm
            n.iter   <- self$options$n_iter
            p        <- self$options$p
            poly     <- self$options$corrMethod == 'poly'
            flip     <- self$options$flip

            res <- omega(
                m = data,
                nfactors = nfactors,
                fm = fm,
                n.iter = n.iter,
                p = p,
                poly = poly,
                flip = flip,
                plot = FALSE)
            
            self$results$omega$setContent(res)
            self$results$plot$setState(res)
        },
        .plot=function(image, ...) {
            
            state <- image$state
            if (is.null(state))
                return(FALSE)
            
            omega.diagram(state)
            
            return(TRUE)
        },
        .sourcifyOption = function(option) {
            
            name <- option$name
            value <- option$value
            
            if (name == 'vars') {
                return('')
            }
            else if (name == 'data') {
                return('m = data')
            }
            else if (name == 'n_iter') {
                if (value != option$default)
                    return(paste0('n.iter = ', value))
            }
            else if (name == 'corrMethod') {
                if (value == 'poly')
                    return('poly = TRUE')
            }
            
            super$.sourcifyOption(option)
        }),
    public = list(
        asSource=function() {
            paste0(private$.package, '::', 'omega', '(', private$.asArgs(), ')')
        }
    )
)
