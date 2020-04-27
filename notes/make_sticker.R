
library(motifr)
library(ggplot2)
library(hexSticker)
library(magick)
library(patchwork)
library(svglite)

library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Roboto")
## Automatically use showtext to render text for future devices
showtext_auto()

p1 <- motifr::show_motif(motif = '2,2[III.C]') +
  theme_void() + theme_transparent() + guides(color = FALSE)

p2 <- motifr::show_motif(motif = '1,2[I.C]') +
  theme_void() + theme_transparent() + guides(color = FALSE)
p2

p3 <- motifr::show_motif(motif = '2,2,1[III.D.3]') +
  theme_void() + theme_transparent() + guides(color = FALSE)
p3

p4 <- motifr::show_motif(motif = '2,2,1[III.D.1]') +
  theme_void() + theme_transparent() + guides(color = FALSE)
p4

sticker(p4,
        package="motifr", p_size=25, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/motifr.png", h_fill = "#ede9e8", dpi = 300,
        p_family = "Roboto", p_color = "black", h_color = "black")

sticker(p4,
        package="motifr", p_size=25, s_x=1, s_y=.75, s_width=1.3, s_height=1,
        filename="inst/figures/motifr.svg", h_fill = "#ede9e8", dpi = 300,
        p_family = "Roboto", p_color = "black", h_color = "black")

usethis::use_logo(img = "inst/figures/motifr.png")
