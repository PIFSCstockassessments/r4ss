#' Open png device and return info on the file being created
#'
#' this was previously contained within each of the SSplotXXX() functions.
#' it (1) translates the not-quite-matching specifications for the image to the
#' values needed by png(), then return (2) returns the plotinfo data.frame
#' (which exists within each function which calls this) after adding a row
#' with the filename and caption for each new plot
#' Note: this just opens the png device which needs to be closed via dev.off()
#' outside this function
#' 
#' @param file filename to write to (including .png extension)
#' @param caption caption for the image
#' @param alt_text alternative text for screen readers (may match caption in some cases)
#' @author Ian G. Taylor

pngfun <- function(plotinfo,
                   file,
                   plotdir,
                   pwidth,
                   pheight,
                   punits,
                   res,
                   ptsize,
                   caption = NA,
                   alt_text = NA,
                   filenameprefix = "",
                   par = NULL) {

  # replace any slashes (as in 'Eggs/kg_inter_Fem')
  file <- gsub(pattern = "/", replacement = "_per_", x = file, fixed = TRUE)

  # open png device
  png(
    filename = file.path(plotdir, file),
    width = pwidth,
    height = pheight,
    units = punits,
    res = res,
    pointsize = ptsize
  )

  # change graphics parameters to input value
  if (!is.null(par)) {
    par(par)
  }
  
  # assemble and return info
  invisible(rbind(plotinfo, data.frame(file = file,
                                       caption = caption,
                                       alt_text = alt_text)))
}

