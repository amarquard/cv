## ---- plot_bridge

#' Plot a bridge using cosine function and ggplot lines and segments
#'
#' @param p the x-axis value at which to plot the pillars (at p and -p) in the graph coordinates
#'
#' @return a ggplot object
#' @export
#'
#' @examples
#' plot_bridge(2)
#' 
plot_bridge <- function(p) {
  
  # x values around two pillars at position p and minus p
  x <- seq(-2*p, 2*p, length.out = 10000)
  
  # Hyperbolic cosine to get the centre curve
  y <- cosh(x)
  
  # Extend by repeating parts of the centre curve at the ends
  y[x < -p] <- y[x >= 0 & x <  p]
  y[x >  p] <- y[x <= 0 & x > -p]
  
  df <- data.frame(x = x, y = y)
  
  ggplot(df, aes(x = x, y = y)) +
    geom_line(size = 1) +
    ylim(0.5, cosh(p)) +
    
    # add road
    geom_hline(yintercept = 1, size = 1) +
    
    # add pillars
    geom_vline(xintercept = c(-p, p), size = 1.3) +
    
    # add ropes
    geom_segment(aes(x = x, y = 1, xend = x, yend = y),
                 data = df[round(seq(1, 10000, length.out = 61)),]) +
    
    # remove all axes etc.
    theme_void()
}


## ---- make_scholar_link
make_scholar_link <- function(pubid, authorid) {
  glue(
    paste0(
      "https://scholar.google.com/citations?",
      "view_op=view_citation&",
      "hl=da&",
      "user={authorid}&",
      "citation_for_view={authorid}:{pubid}"
    )
  )
}
