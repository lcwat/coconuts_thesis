# plot theme details to maintain consistent look across plots
# set colors and font
library(showtext)
library(ggplot2)
library(NatParksPalettes)

clrs <- natparks.pals('Everglades')
showtext_opts(dpi = 300)
showtext_auto()

# presentation font
font_add_google('Roboto')

project_theme <- function() {
  theme_bw() +
    theme(
      panel.border = element_blank(), 
      panel.background = element_blank(), 
      plot.background = element_blank(), 
      legend.background = element_blank(),
      panel.grid = element_blank(), 
      axis.line = element_line(color = 'grey20', linewidth = .75),
      axis.ticks = element_line(color = 'grey20', linewidth = .5),
      text = element_text(family = 'Roboto'),
      axis.text = element_text(size = 10), 
      axis.title = element_text(size = 14, face = 'bold'), 
      legend.text = element_text(size = 10), 
      legend.title = element_text(size = 14, face = 'bold'),
      legend.position = 'top', 
      legend.justification = 'left', 
      legend.direction = 'horizontal', 
      strip.background = element_rect(fill = NA, color = NA), 
      strip.text = element_text(face = 'italic')
    )
}
