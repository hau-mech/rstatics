#' Get centroid t-beam
#' @description Calculate the centroid of a T-beam (from the bottom)
#' @param .flange Dimension of the flange [mm]
#' @param .flange_thickness Dimension of the flange thickness [mm]
#' @param .web Dimension of the web [mm]
#' @param .web_thickness Dimension of the web thickness [mm]
#' @return A data frame with the X-Y coordinates of the centroid
#' @export
#' @examples
#' calculate_t_beam_centroid(.flange = 38.1,
#'                           .flange_thickness = 6.4,
#'                           .web = 38.1,
#'                           .web_thickness = 6.4)

calculate_t_beam_centroid <- function(.flange,
                                      .flange_thickness,
                                      .web,
                                      .web_thickness) {
  # Calculate the area and centroid of the flange part
  flange_area <- .flange * .flange_thickness
  flange_centroid_x <- 0  # The flange is centered on the y-axis
  flange_centroid_y <- .flange_thickness / 2

  # Calculate the area and centroid of the web part
  web_area <- (.web - .flange_thickness) * .web_thickness
  web_centroid_x <- 0  # The web is centered on the y-axis
  web_centroid_y <- .flange_thickness + ((.web - .flange_thickness) / 2)

  # Calculate the total area and centroid of the T-section
  total_area <- flange_area + web_area
  total_centroid_x <- (flange_area * flange_centroid_x + web_area * web_centroid_x) / total_area
  total_centroid_y <- (flange_area * flange_centroid_y + web_area * web_centroid_y) / total_area

  # Return the centroid coordinates
  return(data.frame(x = total_centroid_x, y = total_centroid_y))

}
