#' Generating global sampling grids with constant distance between sample
#' locations on the surface of a sphere
#'
#' @param dis Distance in kilometers between sample locations
#' @param bnd Polygon outline of an area of interest for which the sampling grid
#'   is generated (a \code{\link[sp]{SpatialPolygonsDataFrame}} object). If
#'   \code{NULL}, a global grid is generated.
#'
#' @details The grid consists of equidistant points along circles of latitude on
#'   a spheroid (WGS84/Pseudo-Mercator, epsg:43328).
#'
#' @return An object of \code{\link[sp]{SpatialPointsDataFrame}} holding the
#'   sampling locations of the grid.
#' @author Lutz Fehrmann
#' @export
#'
#' @examples
#' # Boundary of Germany
#' ger_bnd <- load_boundary(x = NA, country_code = "DEU", adm_level = 0);
#'
#' gsg_ger <- gen_gsg(50, ger_bnd);
#' plot(gsg_ger)
gen_gsg <- function(dis, bnd = NULL) {
  wgs84 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0";
  # Area of interest
  if (is.null(bnd)) {
    data("wrld_simpl"); # Using data set from maptools, "countries" in getData() not working
    bnd <- wrld_simpl;
  } else {
    if (identical(proj4string(bnd),wgs84) == FALSE) {
      if (!is.na((proj4string(bnd))) == TRUE) {
        warning("bnd has wrong projection! Transformed to EPSG:4326");
        bnd <- sp::spTransform(x = bnd, CRSobj = CRS(wgs84));
      }
      else if (!is.na((proj4string(bnd))) == FALSE) {
        warning("bnd has no projection! Projected to EPSG:4326");
        proj4string(bnd) <- CRS(wgs84)
      }
    }
  }

  aoi <- c(sp::bbox(bnd)[2], sp::bbox(bnd)[4],
           sp::bbox(bnd)[1], sp::bbox(bnd)[3]);

  wgs84_semi_major_axis <- 6378.137;
  deg <- (dis/(pi * wgs84_semi_major_axis)) * 180;

  # Create a vector of longitue along equator
  lon <- sort(c(seq(0, -180, -deg), seq(deg, 180, deg)));

  # Create a vector of latitudes in aoi
  lat <- lon[lon >= aoi[1] & lon <= aoi[2]];

  # Calculate a matrix of longitudes for each circle of latitude
  lon_mat <- outer(lon, lat, function(x, y) x/cos(pi/180 * y));

  # Compile coordinates
  coord <- cbind(as.vector(t(lon_mat)), lat);
  colnames(coord) <- c("X", "Y");

  # Subset coordinates in aoi
  coord <- coord[coord[, 1] >= aoi[3] & coord[, 1] <= aoi[4], , drop = FALSE];
  if (nrow(coord) == 0) {
    warning("No sample locations in the area of interest! Adjust value of dis");
    return(SpatialPoints(coord = matrix(c(0, 0), ncol = 2),
                         proj4string = CRS(wgs84)));
  }

  spdf_gsg <- sp::SpatialPointsDataFrame(coords = coord,
                                         data = data.frame(1:nrow(coord)));
  sp::proj4string(spdf_gsg) <- sp::CRS(wgs84);

  # Subset gridpoints falling on land (or in a specific country)
  df_over <- sp::over(spdf_gsg, bnd);
  idx <- is.na(df_over[, 1]) == FALSE;
  if (sum(is.na(df_over[, 1])) == 0) {
    warning("No sample locations in the area of interest! Adjust value of dis");
    return(SpatialPoints(coord = matrix(c(0, 0), ncol = 2),
                         proj4string = CRS(wgs84)));
  }

  return(SpatialPointsDataFrame(coords = coordinates(spdf_gsg[idx, ]),
                                data = cbind(1:sum(idx), df_over[idx, ]),
                                proj4string = raster::crs(spdf_gsg)));
}
