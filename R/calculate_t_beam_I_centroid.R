#' Areal moment of inertia
#' @description Calculate the areal moment of inertia of a T-beam along its
#' centroid. The flange is centered on the y-axis.
#'   Area moment of inertia (I in mm4)
#' @param .flange Dimension of the flange [mm]
#' @param .flange_thickness Dimension of the flange thickness [mm]
#' @param .web Dimension of the web [mm]
#' @param .web_thickness Dimension of the web thickness [mm]
#' @return data frame with the values of the areal oment of inertio along the x and y axes
#' @export
#' @examples
#' calculate_t_beam_I_centroid(.flange = 38.1,
#'                             .flange_thickness = 6.4,
#'                             .web = 38.1,
#'                             .web_thickness = 6.4)

calculate_t_beam_I_centroid <- function(.flange,
                                        .flange_thickness,
                                        .web,
                                        .web_thickness) {

  # Calculate the area, centroid, and moment of inertia of the flange part
  flange_area <- .flange * .flange_thickness
  flange_centroid_x <- 0  # The flange is centered on the y-axis
  flange_centroid_y <- .flange_thickness / 2

  # Calculate the area, centroid, and moment of inertia of the web part
  web_area <- (.web - .flange_thickness) * .web_thickness
  web_centroid_x <- 0  # The web is centered on the y-axis
  web_centroid_y <- .flange_thickness + ((.web - .flange_thickness) / 2)
  web_moment_of_inertia <- (1/3) * .web_thickness * .web^3

  # Calculate the total area and centroid of the T-section
  total_area <- flange_area + web_area
  total_centroid_x <- (flange_area * flange_centroid_x + web_area * web_centroid_x) / total_area
  total_centroid_y <- (flange_area * flange_centroid_y + web_area * web_centroid_y) / total_area

  # Calculate moment of inertia of the flange about T-section centroid
  flange_Ix = (1/12) * .flange * .flange_thickness^3
  flange_Ix_section_centroid <- flange_Ix + flange_area * (total_centroid_y - flange_centroid_y)^2
  flange_Iy_section_centroid <- (1/12) * .flange_thickness * .flange^3

  # Calculate moment of inertia of the web about T-section centroid
  web_Ix = (1/12) * .web_thickness * (.web - .flange_thickness)^3
  web_Ix_section_centroid <- web_Ix + web_area * (total_centroid_y - web_centroid_y)^2
  web_Iy_section_centroid <- (1/12) * (.web - .flange_thickness) * (.web_thickness)^3

  # Total I [mm4] of the cross-section
  total_Ix <- flange_Ix_section_centroid + web_Ix_section_centroid
  total_Iy <- flange_Iy_section_centroid + web_Iy_section_centroid

  # Output
  return(data.frame(Ix = total_Ix, Iy = total_Iy))

}
