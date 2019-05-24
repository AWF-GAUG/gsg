#' Load country boundaries into a \code{\link[sp]{SpatialPolygonsDataFrame}}
#' object
#'
#' @param x If missing, \code{link[raster]{getData}} function is used to load
#'   boundaries from the GADM project (http://gadm.org). Otherwise the name of
#'   the data source (see \code{\link[gdal]{readOGR}}).
#' @param country_code Country specified by three letter ISO code. Use function
#'   \code{\link[raster]{ccodes}} to get appropriate codes. If multiple
#'   countries are desired, provide a character vector of appropriate ISO codes.
#' @param adm_level Level of administrative subdivision 0 = country, 1 = first
#'   level subdivision
#' @param ... Other arguments to \code{\link[rgdal]{readOGR}}.
#'
#' @return A \code{\link[sp]{SpatialPolygonsDataFrame}} object storing country
#'   boundaries.
#' @author Nils Noelke, Sebastian Schnell
#' @export
#'
#' @examples
#' # Load boundary of Germany
#' ger_bnd <- load_boundary(country_code = "DEU", adm_level = 0);
#' plot(ger_bnd);
#'
#' # Load boundary of several countries
#' bnd <- load_boundary(country_code = c("DEU", "ITA", "FRA"));
load_boundary <- function (x = NULL, country_code = 'world', adm_level = 0, ...) {
  if (is.null(x)) {
    if (toupper(country_code[1]) == 'WORLD') {
      data("wrld_simpl");
      return(wrld_simpl);
    } else {
      # Load boundary from GADM
      bnd <- raster::getData(name = 'GADM',
                             country = country_code[1],
                             level = adm_level);
      row.names(bnd) <- country_code[1];
      if (length(country_code) > 1) {
        for (i in 2:length(country_code)) {
          temp_bnd <- raster::getData(name = 'GADM',
                                      country = country_code[i],
                                      level = adm_level);
          row.names(temp_bnd) <- country_code[i];
          bnd <- maptools::spRbind(bnd, temp_bnd);
        }
      }
      bnd <- bnd[, c("GID_0", "NAME_0")];
    }
  } else {
    # Load boundary from other vector format using readOGR
    layer = sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(x))
    bnd <- rgdal::readOGR(dsn = x, layer=layer, ...);
  }
  return(bnd);
}


#' Add point clusters at the points of the Global Sampling Grid
#'
#' @param gsg A matrix with longitudes (first column) and latitudes (second column).
#' @param layout A matrix specifiying the layout of the point cluster. See Details and Examples.
#' @param ref Where is the reference of the point cluster. With default settings
#' it is centered on the grid coordinates. Defaults can be overwritten by
#' providing row and column indexes. See examples.
#' @param dist Distance in m between points in the cluster.
#' @param rot Should the clusters be rotated? Default is 0 for no rotation. If
#' 'random', each point cluster is rotated in a random direction. If a single
#' value is provided, all clusters are rotated in the same direction.
#'
#' @return SpatialPointsDataFrame. The data slot contains point and sub-point IDs.
#' @export
#'
#' @examples
#' library(gsg);
#' # Generate a global sampling grid with 500km distance
#' gsg <- gen_gsg(500);
#'
#' # Define the layout of the point cluster
#' # 25 points arranged in a square
#' layout <- matrix(rep(1, 25), nrow = 1);
#' # 500m between points
#' dist = 500;
#' # Centered on the grid points
#' ref <- c(3, 3);
#'
#' # New grid with clusters
#' gsg_clus <- plot_design(gsg = gsg@coords,
#'                         layout = layout,
#'                         ref = ref,
#'                         dist = dist,
#'                         rot = 0);
#' plot(gsg_clus)
#'
#' # L-shaped clusters
#' layout <- matrix(c(1, 0, 0,
#'                    1, 0, 0,
#'                    1, 0, 0,
#'                    1, 1, 1), nrow = 4, byrow = TRUE);
#'
#' # Cross
#' layout <- matrix(c(0, 1, 0,
#'                    1, 1, 1,
#'                    0, 1, 1), nrow = 3);
#'
#' # Export to Shapefile
#' rgdal::writeOGR(gsg_clus, "gsg_clus.shp", driver = "ESRI Shapefile", layer = "gsg_clus");
plot_design <- function(gsg, layout, ref = dim(layout)/2 + 0.5, dist, rot = 0) {
  if (tolower(rot) == "random") {
    rot <- runif(nrow(coords), 0, pi);
  }

  n <- nrow(gsg);
  n_sub <- sum(layout);
  gsg_clus <- matrix(rep(0, n*n_sub*4), nrow = n*n_sub);
  colnames(gsg_clus) <- c("id", "id_sub", "lon", "lat");

  r_gap <- ref[1] - row(layout); # gap from reference point in rows
  c_gap <- col(layout) - ref[2]; # gap from reference point in columns
  b <- as.vector((2*pi + atan2(c_gap, r_gap)));
  b <- b[which(as.vector(layout) == 1)] + rot;
  d <- as.vector(sqrt(r_gap^2 + c_gap^2));
  d <- d[which(as.vector(layout) == 1)]*dist;

  for (r in seq.int(nrow(gsg))) {
    gsg_clus_sub <- geosphere::destPoint(gsg[r, ], b*180/pi, d);
    r_end <- r*n_sub;
    r_start <- r_end - n_sub + 1L;
    gsg_clus[r_start:r_end, ] <- cbind(r, 1:n_sub, gsg_clus_sub);
  }
  return(SpatialPointsDataFrame(coords = gsg_clus[, c(3, 4)],
                         data = as.data.frame(gsg_clus[, c(1, 2)]),
                         proj4string = CRS("+init=epsg:4326")));
}
