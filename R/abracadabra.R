#' Basic ELISA plotting
#'
#' This function generates a plot of your ELISA data with basic customization features. The plot separates data by 2-3 variables. For example, you could plot different colored bars based on +/-dox and facet the plot by cell type.
#'
#' This function is intended to be a quick and basic plotting tool. If you need more advanced data visualization, it might not serve all the purposes you need.
#'
#' Important: the x axis always displays +/- stim. y, title, and palette must be entered in quotation marks and must exactly match the options available. color and facet_by must be in dataframe$columnname format and must not be in quotation marks.
#' @param y Data you'd like to put on the y-axis. You can enter "aldo", "viability", or "aldo_norm". Must be in quotation marks.
#' @param title Title for your plot. Must be in quotation marks.
#' @param color Specifies the variable by which you'd like to subset boxes (ie. OD$dox, OD$cell, etc.). Do not use quotation marks.
#' @param facet_by Specifies the variable by which you'd like to facet (ie. OD$cell, etc.). Do not use quotation marks.
#' @param palette Specifies the RColorBrewer palette you'd like to use. Recommend "Dark2" or "Set1". Defaults to "Set1". Use quotation marks.
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyverse
#'
#' @examples
#' abracadabraAlacazam_plotmyELISA_withthisProgram(y = "aldo", title = "example title", color = OD$dox, facet_by = OD$cell, palette = "Dark2")


abracadabraAlacazam_plotmyELISA_withthisProgram <- function(y, title, color, facet_by, palette) {
  # define y axis label and data
  ylab <- case_when(
    y == "aldo" ~ "Aldosterone concentration",
    y == "viability" ~ "PrestoBlue viability",
    y == "aldo_norm" ~ "PB-normalized aldosterone concentration",
    TRUE ~ "errorcondition"
  )

  if(ylab == "errorcondition") {
    stop("Error: invalid value for y. Please enter aldo, viability, or aldo_norm.")
  }

  yAxis <- if(y == "aldo") {
    OD$conc
  } else
    if(y == "viability") {
      OD$PB
    } else
      if(y == "aldo_norm") {
        OD$corrected
      } else {
        stop("Error: invalid value for y. Please enter aldo, viability, or aldo_norm.")
      }

  #set defaults
  if(missing(color)) {
    stop("Error: no value entered for color. Please enter a variable by which to color (suggested: dox, siRNA, etc)")
  }

  if(missing(title)) {
    title <- ""
  }

  if(missing(facet_by)){
    OD$facets <- NULL
  } else {
    OD$facets <- facet_by
  }

  if(missing(palette)) {
    palette <- "Set1"
  }

  if(is.null(OD$facets)) {
    ggplot(OD,
           aes(y=yAxis, x=stim, fill = color)) +
      geom_boxplot() +
      theme_classic() +
      ggtitle(title) +
      xlab("") +
      ylab(ylab) +
      scale_fill_brewer(palette = palette)
  } else {
    ggplot(OD,
           aes(y=yAxis, x=stim, fill = color)) +
      geom_boxplot() +
      theme_classic() +
      ggtitle(title) +
      xlab("") +
      ylab(ylab) +
      facet_grid( ~ OD$facets) +
      scale_color_brewer(palette) +
      scale_fill_brewer(palette = palette)
  }
}
