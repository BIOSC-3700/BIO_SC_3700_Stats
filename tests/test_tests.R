suppressMessages({
  library(shiny); library(bslib); library(ggplot2); library(dplyr)
  library(tibble); library(readr); library(readxl)
  library(DT); library(glue); library(fs)
})
for (f in list.files("R", full.names = TRUE)) source(f)
ok <- function(label, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
near <- function(a, b) isTRUE(abs(a - b) < 1e-9)

fish <- read_csv("data/fish_length.csv", show_col_types = FALSE)
fish_tidy <- tibble(value = fish$length_mm,
                    group = factor(fish$lake))
plant <- read_csv("data/plant_growth.csv", show_col_types = FALSE)
plant_tidy <- tibble(value = plant$height_cm,
                     group = factor(plant$treatment))
bp <- read_csv("data/bp_paired.csv", show_col_types = FALSE)

stub <- function(tidy_df, raw_df, vlab = "value", glab = "group") {
  list(raw = reactive(raw_df), tidy = reactive(tidy_df),
       info = reactive(NULL),
       labels = reactive(list(value = vlab, group = glab)))
}

# ---- two-sample: Welch ---------------------------------------------
lv <- levels(fish_tidy$group)
testServer(mod_ttest2_server,
           args = list(data = stub(fish_tidy, fish, "length_mm", "lake")), {
  session$setInputs(g1 = lv[1], g2 = lv[2], paired = FALSE,
                    var_equal = FALSE, alt = "two.sided", conf = 0.95,
                    plot_style = "box", plot_title = "", plot_xlab = "",
                    plot_ylab = "")
  ref <- t.test(fish$length_mm[fish$lake == lv[1]],
                fish$length_mm[fish$lake == lv[2]])
  ok("welch p matches t.test", near(result()$p.value, ref$p.value))
  ok("welch df matches", near(result()$parameter, ref$parameter))
  ok("test named Welch", test_name() == "Welch's two-sample t-test")
  ok("no problems", length(problems()) == 0)
  ok("verdict renders", grepl("Welch", output$verdict$html))
  ok("plot builds", inherits(the_plot(), "ggplot"))

  # pooled variant
  session$setInputs(var_equal = TRUE)
  ref2 <- t.test(fish$length_mm[fish$lake == lv[1]],
                 fish$length_mm[fish$lake == lv[2]], var.equal = TRUE)
  ok("pooled p matches", near(result()$p.value, ref2$p.value))
  ok("pooled df is n-2", near(result()$parameter, nrow(fish) - 2))

  # one-sided
  session$setInputs(var_equal = FALSE, alt = "less")
  ref3 <- t.test(fish$length_mm[fish$lake == lv[1]],
                 fish$length_mm[fish$lake == lv[2]], alternative = "less")
  ok("one-sided p matches", near(result()$p.value, ref3$p.value))

  # same group twice must be caught
  session$setInputs(alt = "two.sided", g2 = lv[1])
  ok("identical groups blocked", any(grepl("different groups", problems())))

  # unequal n + paired must be caught
  session$setInputs(g2 = lv[2], paired = TRUE)
  ok("unequal-n paired blocked", any(grepl("same number", problems())))
})

# ---- two-sample: ANOVA data has 3 groups, picks 2 -------------------
testServer(mod_ttest2_server,
           args = list(data = stub(plant_tidy, plant)), {
  session$setInputs(g1 = "Control", g2 = "High nitrogen", paired = FALSE,
                    var_equal = FALSE, alt = "two.sided", conf = 0.95,
                    plot_style = "box", plot_title = "", plot_xlab = "",
                    plot_ylab = "")
  ok("subsets 3 groups to 2", nlevels(pair_data()$group) == 2)
  ref <- t.test(plant$height_cm[plant$treatment == "Control"],
                plant$height_cm[plant$treatment == "High nitrogen"])
  ok("subset p matches", near(result()$p.value, ref$p.value))
})

# ---- one-sample ------------------------------------------------------
testServer(mod_ttest1_server,
           args = list(data = stub(fish_tidy, fish, "length_mm", "lake")), {
  session$setInputs(mode = "single", group = "__all__", mu = 150,
                    alt = "two.sided", conf = 0.95,
                    plot_title = "", plot_xlab = "", plot_ylab = "")
  ref <- t.test(fish$length_mm, mu = 150)
  ok("one-sample p matches", near(result()$p.value, ref$p.value))
  ok("one-sample n correct",
     length(sample_values()$values) == nrow(fish))

  session$setInputs(group = lv[1])
  ref2 <- t.test(fish$length_mm[fish$lake == lv[1]], mu = 150)
  ok("group subset p matches", near(result()$p.value, ref2$p.value))
  ok("plot builds", inherits(the_plot(), "ggplot"))
})

# ---- one-sample: difference-between-columns (paired) ----------------
testServer(mod_ttest1_server,
           args = list(data = stub(NULL, bp)), {
  session$setInputs(mode = "diff", col1 = "before", col2 = "after",
                    mu = 0, alt = "two.sided", conf = 0.95,
                    plot_title = "", plot_xlab = "", plot_ylab = "")
  ref <- t.test(bp$after, bp$before, paired = TRUE)
  ok("diff mode equals paired t-test", near(result()$p.value, ref$p.value))
  ok("diff mode estimate matches",
     near(unname(result()$estimate), unname(ref$estimate)))
  session$setInputs(col2 = "before")
  ok("same column twice blocked", any(grepl("different columns", problems())))
})

# ---- ANOVA ------------------------------------------------------------
testServer(mod_anova_server,
           args = list(data = stub(plant_tidy, plant, "height_cm", "treatment")), {
  session$setInputs(tukey = TRUE, conf = 0.95, plot_style = "box",
                    plot_title = "", plot_xlab = "", plot_ylab = "")
  ref <- summary(aov(height_cm ~ treatment, data = plant))[[1]]
  ok("anova F matches", near(anova_row()$f, ref[1, "F value"]))
  ok("anova p matches", near(anova_row()$p, ref[1, "Pr(>F)"]))
  tk <- tukey_table()
  reft <- as.data.frame(TukeyHSD(aov(height_cm ~ treatment, plant))$treatment)
  ok("tukey has 3 rows", nrow(tk) == 3)
  ok("tukey p values match", all(abs(tk$p_adj - reft$`p adj`) < 1e-9))
  # Levels sort alphabetically, so combn order is Control/High,
  # Control/Low, High/Low -- and must line up with TukeyHSD's rownames.
  ok("tukey labels match TukeyHSD row order",
     identical(gsub(" − ", "-", tk$comparison, fixed = TRUE),
               rownames(reft)))
  ok("tukey plot builds", inherits(the_tukey_plot(), "ggplot"))
  ok("anova plot builds", inherits(the_plot(), "ggplot"))
})

# ---- ANOVA refuses 2 groups -----------------------------------------
testServer(mod_anova_server,
           args = list(data = stub(fish_tidy, fish)), {
  session$setInputs(tukey = TRUE, conf = 0.95)
  ok("anova blocks 2 groups", any(grepl("three or more", problems())))
})
