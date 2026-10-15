# Exact selected definitions from R/graphmode_q1_supplement.R
graphmode_q1_clock <- function(now = microbenchmark::get_nanotime) {
    origin <- now(); last <- 0
    function() {
        value <- (now() - origin) / 1e9
        if(length(value)!=1L || !is.finite(value) || value < last)
            stop(structure(list(message="Monotonic timer failed; not a scientific-state error.",call=NULL),
                class=c("graphmode_clock_error","error","condition")))
        last <<- value
        x <- base::proc.time(); x[3L] <- value; x
    }
}
