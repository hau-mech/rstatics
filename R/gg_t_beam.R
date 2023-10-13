#' Plot cross-section area
#' @description Plot in ggplot2 the cross-sectional area of a T-beam
#' @param .flange Dimension of the flange [mm]
#' @param .flange_thickness Dimension of the flange thickness [mm]
#' @param .web Dimension of the web [mm]
#' @param .web_thickness Dimension of the web thickness [mm]
#' @return A ggplot2 object
#' @export
#' @examples
#' library(ggplot2)
#'
#' centroid <- calculate_t_beam_centroid(.flange = 38.1,
#'                                       .flange_thickness = 6.4,
#'                                       .web = 38.1,
#'                                       .web_thickness = 6.4)
#'
#' I <- calculate_t_beam_I_centroid(.flange = 38.1,
#'                                  .flange_thickness = 6.4,
#'                                  .web = 38.1,
#'                                  .web_thickness = 6.4)
#'
#' gg_t_beam(.flange = 38.1,
#'           .flange_thickness = 6.4,
#'           .web = 38.1,
#'           .web_thickness = 6.4,
#'           fill = "darkgrey") +
#'   geom_point(data = centroid, aes(x, y), col = "red", size = 4) +
#'   geom_text(data = centroid, aes(x = x,
#'                                  y = y,
#'                                  label = paste0("Centroid (", round(x, 2), ", ", round(y, 2), ")"),
#'                                  hjust = -0.1,
#'                                  vjust = 0),
#'             size = 6,
#'             color = "red") +
#'   annotate(geom = "text", x = 5.6, y = 40, label = paste("Ix = " , round(I[1]), "mm4"), hjust = "left") +
#'   annotate(geom = "text", x = 5.6, y = 38, label = paste("Iy = " , round(I[2]), "mm4"), hjust = "left") +
#'   theme_bw()

gg_t_beam <- function(.flange,
                      .flange_thickness,
                      .web,
                      .web_thickness,
                      ...) {
  # Create a data frame for the T-section beam using tribble
  beam_data <- tibble::tribble( ~x, ~y,
                                -.flange/2, 0,
                                .flange/2, 0,
                                .flange/2, .flange_thickness,
                                .web_thickness/2, .flange_thickness,
                                .web_thickness/2, .web,
                                -.web_thickness/2, .web,
                                -.web_thickness/2, .flange_thickness,
                                -.flange/2, .flange_thickness,
                                -.flange/2, 0
  )

  # Create a basic plot
  beam_plot <- ggplot2::ggplot(beam_data, aes(x = x, y = y)) +
    ggplot2::geom_polygon(...)  +
    ggplot2::labs(x = "Flange [mm]",
                  y = "Web [mm]",
                  title = "T-beam") +
    ggplot2::coord_fixed(ratio = 1)

  # Display the plot
  return(beam_plot)

}
