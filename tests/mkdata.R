suppressMessages({library(dplyr); library(tibble); library(readr)})
dest <- "app/data"
set.seed(2024)

# 3 treatments: Low is close to Control, High clearly separates. Gives
# a significant ANOVA where Tukey finds some pairs and not others.
plant <- tibble(
  treatment = rep(c("Control", "Low nitrogen", "High nitrogen"), each = 15),
  height_cm = round(c(rnorm(15, 21.5, 2.6), rnorm(15, 23.2, 2.8),
                      rnorm(15, 28.4, 3.0)), 1)
)
write_csv(plant, file.path(dest, "plant_growth.csv"))

fish <- tibble(
  lake = rep(c("Lake Ashe", "Lake Bynum"), c(22, 19)),
  length_mm = round(c(rnorm(22, 141, 16), rnorm(19, 158, 19)), 0)
)
write_csv(fish, file.path(dest, "fish_length.csv"))

# Wide layout with unequal group sizes, so the NA-dropping path gets
# exercised by a dataset students actually open.
enzyme <- tibble(
  pH_6.0 = round(c(rnorm(12, 4.1, 0.7), rep(NA, 2)), 2),
  pH_7.0 = round(rnorm(14, 6.8, 0.8), 2),
  pH_8.0 = round(c(rnorm(11, 5.2, 0.9), rep(NA, 3)), 2)
)
write_csv(enzyme, file.path(dest, "enzyme_wide.csv"))

before <- rnorm(18, 138, 11)
bp <- tibble(
  subject = sprintf("S%02d", 1:18),
  before = round(before, 0),
  after = round(before - rnorm(18, 7.5, 5), 0)
)
write_csv(bp, file.path(dest, "bp_paired.csv"))

cat("--- checks ---\n")
cat("ANOVA p:", signif(summary(aov(height_cm ~ treatment, plant))[[1]][1, "Pr(>F)"], 3), "\n")
print(round(TukeyHSD(aov(height_cm ~ treatment, plant))$treatment[, "p adj"], 4))
cat("fish t p:", signif(t.test(length_mm ~ lake, fish)$p.value, 3), "\n")
cat("paired t p:", signif(t.test(bp$before, bp$after, paired = TRUE)$p.value, 4), "\n")
cat("enzyme ANOVA p:", signif(summary(aov(value ~ name,
  tidyr::pivot_longer(enzyme, everything()) |> filter(!is.na(value))))[[1]][1, "Pr(>F)"], 3), "\n")
