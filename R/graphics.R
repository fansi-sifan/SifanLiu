
# https://towardsdatascience.com/build-a-corporate-r-package-for-pleasure-and-profit-78b73ce4ff4b

# 1. scale_metro_fill(): metro palette from Luisa
# 2. scale_metro_color(): metro palette from Luisa
# 3. bbplot(): ggplot + customized theme

# FUNCTIONS ======================
# Palette main colors
metro.styles <- c(

  `M.sea` = "#003249",
  `M.grey` = "#767171",
  `M.sky` = "#0070c0",
  `M.sun` = "#ffc000",
  `M.beach` = "#ff6966",
  `M.bg` = "#D9D9D9")

# Fn to extract them by hex codes
styles <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (metro.styles)

  metro.styles[cols]
}

# Create separate palettes
metro.palettes <- list(
  `main`  = styles('M.sky','M.sun', 'M.grey', 'M.beach'),

  `cool`  = styles('M.sea', 'M.sky', 'M.sun')
)

# Fn to access them
metro_pal <- function(palette = "main", reverse = FALSE, ...) {
  pal <- metro.palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

# Fn for customer scale
scale_color_metro <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- metro_pal(palette = palette, reverse = reverse)

  #' Scale color using AgC color palette.
  #' @param palette: main, cool
  #' @param discrete: T or F
  #' @param reverse: reverse the direction of the color scheme

  if (discrete) {
    discrete_scale("colour", paste0("metro_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}


scale_fill_metro <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {

  #' Scale fill using AgC color palette.
  #' @param palette: main, cool
  #' @param discrete: T or F
  #' @param reverse: reverse the direction of the color scheme

  pal <- metro_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("metro_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}


# ggplot customization
# presentation background
require(ggplot2)
bbplot <- function(...){
  ggplot(...)+
    theme(rect = element_rect(fill = "#D9D9D9", colour = NA),
          panel.background = element_rect(fill = "#D9D9D9",colour = NA),
          plot.background = element_rect(fill = "#D9D9D9", colour = NA),
          panel.grid = element_blank(),
          legend.background = element_rect(fill = "transparent"),
          legend.key = element_rect(fill = "transparent", color = NA),
          legend.box.background = element_rect(fill = "transparent", colour = NA),
          text = element_text(size = 15,family ="sans" ),
          axis.text = element_text(size = 12, family = "sans"),
          plot.title = element_text(hjust = 0.5),
          axis.ticks = element_blank()
    )
}

# TEST RUN ==========

bbplot(mtcars, aes(x = mpg, y = hp, color = as.factor(cyl)))+
  geom_point()+
  scale_color_metro(discrete = T, palette = "main")

bbplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_bar(stat = "identity")+
  scale_fill_metro(discrete = T, palette = "cool")

