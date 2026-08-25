suppressMessages({library(dplyr); library(tibble); library(readr)})
set.seed(3700)
# Balanced 2x2, n = 8 per cell. WT loses a little biomass under
# drought; the mutant collapses -- a real interaction, which is the
# whole point of the example.
cell_means <- c("WT.control" = 24.0, "WT.drought" = 21.5,
                "mutant.control" = 23.2, "mutant.drought" = 12.8)
grid <- expand.grid(genotype = c("WT", "mutant"),
                    treatment = c("control", "drought"),
                    rep = 1:8, stringsAsFactors = FALSE)
key <- paste(grid$genotype, grid$treatment, sep = ".")
fac <- tibble(
  genotype = grid$genotype,
  treatment = grid$treatment,
  biomass_g = round(cell_means[key] + rnorm(nrow(grid), 0, 2.2), 1)
) |> arrange(genotype, treatment)
write_csv(fac, "app/data/factorial_growth.csv")

fit <- aov(biomass_g ~ genotype * treatment, data = fac)
print(summary(fit))
cat("\ncell counts:\n"); print(table(fac$genotype, fac$treatment))
cat("\ncell means:\n")
print(fac |> group_by(genotype, treatment) |>
        summarize(mean = round(mean(biomass_g), 2), n = n(), .groups = "drop"))
