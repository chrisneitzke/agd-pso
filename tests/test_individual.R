source("../core/individual.R")

ind <- createIndividual(10)

print(ind)

stopifnot(length(ind$x) == 10)

stopifnot(length(ind$velocity) == 10)

stopifnot(ind$neighbors == 0)

cat("Individual OK!\n")

clone <- cloneIndividual(ind)

stopifnot(identical(ind, clone))

clone$x[1] <- 999

stopifnot(ind$x[1] != clone$x[1])

cat("Clone OK!\n")