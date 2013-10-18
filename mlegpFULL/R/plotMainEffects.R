`plotMainEffects` <-
function(gp, ylab = "predicted output", graphStyle = 2, verbose = FALSE, no.plot = FALSE, parallel = FALSE, ...) {
	UseMethod("plotMainEffects", gp)
}
