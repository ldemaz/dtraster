#' Spatial aggregation of data.table.raster
#' @param dt Input data.table, with x, y coordinates
#' @param nr Number of rows in data.table.raster
#' @param nc Number of columns in data.table.raster
#' @param fact aggregation factor
#' @param funexpr Aggregation function for data.table to evaluate (see details)
#' @param dxy Resolution of input data.table.raster (square grids only)
#' @param outname Character vector for value to be aggregated
#' @param dropid Keep or remove cell number ID in output data.table.raster?
#' @return An aggregated data.table.raster
#' @details This function was created to allow more complicated aggregations,
#' such as weighted mean aggregation. Functions need to be passed in as an
#' expression, complete with the names of the variables to evaluate. E.g. for
#' a data.table.raster with variables x and w, then pass in
#' funexpr = parse(text = "weighted.mean(x, w)"). Aggregation will always be
#' done by variables ac and ar, which indicate the aggregation groupings.
#' Don't name one of your variables x, because as.data.table.raster creates
#' a x coordinate, and the function will fail if it detects a duplicate column
#' name.
#' @export
#' @examples
#' r1 <- raster(xmn = 10, xmx = 20, ymn = 10, ymx = 20)
#' res(r1) <- 0.25
#' set.seed(1)
#' r1[] <- sample(1:10, ncell(r1), replace = TRUE)
#' r2 <- raster(r1)
#' set.seed(3)
#' r2[] <- runif(ncell(r2), min = 0, max = 1)
#' s <- stack(r1, r2)
#' names(s) <- c("v", "w")  # selecting name v, rather than x.
#' rdt <- as.data.table.raster(s, xy = TRUE)
#' p4s <- CRS(projection(r1))
#' plot(dt_to_raster(rdt, p4s))
#' vw <- dt_aggregate(x = rdt, nr = nrow(r1), nc = ncol(r1), fact = 5,
#'                    funexpr = parse(text = "weighted.mean(v, w)"),
#'                    dxy = xres(r1), outname = "muw")
#' v <- dt_aggregate(rdt, nr = nrow(r1), nc = ncol(r1), fact = 5,
#'                   funexpr = parse(text = "mean(v)"),
#'                   dxy = xres(r1), outname = "mu")
#' plot(dt_to_raster(vw, p4s))
#' plot(dt_to_raster(v, p4s))
#' plot(aggregate(r1, fact = 5))
#' plot(aggregate(r1, fact = 5) - dt_to_raster(v, p4s)) # r inferno
#' plot(round(aggregate(r1, fact = 5) - dt_to_raster(v, p4s), 10)) # same
#'
#' # speed test
#' r3 <- raster(xmn = 10, xmx = 20, ymn = 10, ymx = 20)
#' p4s <- CRS(projection(r3))
#' res(r3) <- 0.001
#' set.seed(1)
#' r3[] <- sample(1:10, ncell(r3), replace = TRUE)
#' names(r3) <- "v"  # selecting name v, rather than x.
#' rdt <- as.data.table(r3, xy = TRUE)  # time penalty to convert to data.table
#' system.time(v <- dt_aggregate(rdt, nr = nrow(r3), nc = ncol(r3), fact = 10,
#'                               funexpr = parse(text = "mean(v)"),
#'                               dxy = xres(r3), outname = "mu"))
#' # user  system elapsed
#' # 11.328  16.006  35.346
#' system.time(r4 <- aggregate(r3, fact = 10))
#' # user  system elapsed
#' # 2.240   1.183   4.159 # raster aggregate still much faster
#' dtr <- dt_to_raster(v, p4s)
#' plot(dtr)
#' plot(r4)
#' plot(round(dtr - r4), 10)

dt_aggregate <- function(xdt, nr, nc, fact, funexpr, dxy, outname,
                           dropid = TRUE) {

  if(length(unique(names(xdt))) != length(names(xdt))) {
    stop("Duplicate column names may cause incorrect aggregation results!",
         call. = FALSE)
  }  # put this check in here because I had two x columns when testing

  # aggregation row and column counters, adapted from raster::aggregate
  # this is slow for now
  rsteps <- as.integer(ceiling(nr / fact))
  csteps <- as.integer(ceiling(nc / fact))
  DT <- copy(xdt)  # bottleneck for large rasters
  DT[, ac := rep(rep(1:csteps, each = fact)[1:nc], times = nr)]
  DT[, ar := rep(1:rsteps, each = nc * fact)[1:nrow(DT)]]
  setkeyv(DT, c("ar", "ac"))  # slow

  # aggregate raster dimensions (pretty slow)
  adj <- 0.5 * dxy
  xmn <- DT[, min(x) - adj]
  xmx <- xmn + csteps * fact * dxy
  ymx <- DT[, max(y) + adj]
  ymn <- ymx - rsteps * fact * dxy
  r <- raster(extent(xmn, xmx, ymn, ymx))
  res(r) <- fact * dxy
  r[] <- 1:ncell(r)

  # coerce to data.table to get new XY values
  aggr <- as.data.table.raster(r, xy = TRUE)
  setnames(aggr, "layer", "ID")
  setkey(aggr, "ID")

  # perform aggregation function (pretty fast down here)
  aggr[, (outname) := DT[, eval(funexpr), by = .(ac, ar)][, V1]]  # assign var
  if(dropid == TRUE) aggr[, ID := NULL]
  aggr <- na.omit(aggr)
  return(aggr)
}
