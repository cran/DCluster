library(spdep)
library(boot)

.First.lib <- function(lib, pkg) {
        library.dynam("DCluster", pkg, lib)
}
