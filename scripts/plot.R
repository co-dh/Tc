#!/usr/bin/env Rscript
# Plot /tmp/tc-plot.dat (tab-separated: x, y [, category])
# Usage: Rscript scripts/plot.R
#   or:  ./scripts/plot.R

library(ggplot2)

dat <- "/tmp/tc-plot.dat"
png <- "/tmp/tc-plot.png"

# detect columns
first <- readLines(dat, n = 1)
ncols <- length(strsplit(first, "\t")[[1]])
cnames <- if (ncols >= 3) c("x", "y", "cat") else c("x", "y")
d <- read.delim(dat, header = FALSE, col.names = cnames, colClasses = "character")
d$y <- as.numeric(d$y)

# detect time x-axis (HH:MM:SS)
is_time <- grepl("^\\d{1,2}:\\d{2}:\\d{2}$", d$x[1])
if (is_time) d$x <- as.POSIXct(d$x, format = "%H:%M:%S")

p <- ggplot(d, aes(x = x, y = y))

if (ncols >= 3) {
  p <- p + geom_line(aes(color = cat), linewidth = 0.5) +
    labs(color = NULL)
} else {
  p <- p + geom_line(color = "#4682B4", linewidth = 0.5)
}

if (is_time) p <- p + scale_x_datetime(date_labels = "%H:%M")

p <- p + labs(x = NULL, y = NULL) + theme_minimal()

ggsave(png, p, width = 12, height = 7, dpi = 100)

system2("viu", png)
