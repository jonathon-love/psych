
jmvAlphaClass <- R6::R6Class(
    "jmvAlphaClass",
    inherit = jmvAlphaBase,
    private = list(
        .run = function() {
            
            if (length(self$options$vars) < 2)
                return()
            
            data <- self$data
            
            for (name in names(data)) {
                var <- data[[name]]
                if ( ! jmvcore::canBeNumeric(var))
                    stop("Variable '", name, "' is not numeric")
                data[[name]] <- jmvcore::toNumeric(var)
            }
            
            data <- na.omit(data)

            res <- alpha(data)
            
            alpha <- res$total$raw_alpha
            se <- res$total$ase
            cil <- alpha - 1.96 * se
            ciu <- alpha + 1.96 * se
            
            table <- self$results$main
            table$setRow(rowNo=1, values=list(
                raw_alpha = alpha,
                std_alpha = res$total$std.alpha,
                g6 = res$total$`G6(smc)`,
                average_r = res$total$average_r,
                sn = res$total$`S/N`,
                se = se,
                cil = cil,
                ciu = ciu))
            
            drop <- res$alpha.drop
            table <- self$results$dropped
            
            for (i in seq_len(nrow(drop))) {
                row <- drop[i,,drop=TRUE]
                table$setRow(rowNo=i, value=list(
                    raw_alpha = row$raw_alpha,
                    std_alpha = row$std.alpha,
                    g6 = row$`G6(smc)`,
                    average_r = row$average_r,
                    sn = row$`S/N`))
            }
            
            items <- res$item.stats
            table <- self$results$items
            
            for (i in seq_len(nrow(items))) {
                row <- items[i,,drop=TRUE]
                table$setRow(rowNo=i, value=list(
                    r = row$raw.r,
                    rcor = row$r.cor,
                    rdrop = row$r.drop))
            }
            
            
        })
)
