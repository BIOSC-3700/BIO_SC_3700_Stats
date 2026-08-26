suppressMessages({
  library(shiny); library(bslib); library(ggplot2); library(dplyr)
  library(tibble); library(readr); library(readxl)
  library(DT); library(glue); library(fs)
})
for (f in list.files("R", full.names = TRUE)) source(f)
ok <- function(label, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
near <- function(a, b) isTRUE(abs(a - b) < 1e-9)

lizard <- read_csv("data/horned_lizards.csv",
                   show_col_types = FALSE)
lizard_tidy <- tibble(value = lizard$horn.length,
                      group = factor(lizard$group))
jetlag <- read_csv("data/jetlag_knees.csv",
                   show_col_types = FALSE)
jetlag_tidy <- tibble(value = jetlag$shift,
                      group = factor(jetlag$treatment))
bb <- read_csv("data/blackbird_antibodies.csv",
               show_col_types = FALSE)

stub <- function(tidy_df, raw_df,
                 vlab = "value", glab = "group") {
  list(raw = reactive(raw_df), tidy = reactive(tidy_df),
       info = reactive(NULL),
       labels = reactive(list(value = vlab, group = glab)))
}

# ---- two-sample: Welch ---------------------------------------------
lv <- levels(lizard_tidy$group)
testServer(mod_ttest2_server,
           args = list(data = stub(lizard_tidy, lizard,
                                   "horn.length", "group")), {
  session$setInputs(g1 = lv[1], g2 = lv[2], paired = FALSE,
                    var_equal = FALSE, alt = "two.sided",
                    conf = 0.95, plot_style = "box",
                    plot_title = "", plot_xlab = "",
                    plot_ylab = "")
  ref <- t.test(
    lizard$horn.length[lizard$group == lv[1]],
    lizard$horn.length[lizard$group == lv[2]])
  ok("welch p matches t.test",
     near(result()$p.value, ref$p.value))
  ok("welch df matches",
     near(result()$parameter, ref$parameter))
  ok("test named Welch",
     test_name() == "Welch's two-sample t-test")
  ok("no problems", length(problems()) == 0)
  ok("verdict renders", grepl("Welch", output$verdict$html))
  ok("plot builds", inherits(the_plot(), "ggplot"))

  # pooled variant
  session$setInputs(var_equal = TRUE)
  ref2 <- t.test(
    lizard$horn.length[lizard$group == lv[1]],
    lizard$horn.length[lizard$group == lv[2]],
    var.equal = TRUE)
  ok("pooled p matches",
     near(result()$p.value, ref2$p.value))
  ok("pooled df is n-2",
     near(result()$parameter, nrow(lizard) - 2))

  # one-sided
  session$setInputs(var_equal = FALSE, alt = "less")
  ref3 <- t.test(
    lizard$horn.length[lizard$group == lv[1]],
    lizard$horn.length[lizard$group == lv[2]],
    alternative = "less")
  ok("one-sided p matches",
     near(result()$p.value, ref3$p.value))

  # same group twice must be caught
  session$setInputs(alt = "two.sided", g2 = lv[1])
  ok("identical groups blocked",
     any(grepl("different groups", problems())))

  # unequal n + paired must be caught
  session$setInputs(g2 = lv[2], paired = TRUE)
  ok("unequal-n paired blocked",
     any(grepl("same number", problems())))
})

# ---- two-sample: ANOVA data has 3 groups, picks 2 -------------------
testServer(mod_ttest2_server,
           args = list(data = stub(jetlag_tidy, jetlag)), {
  session$setInputs(g1 = "control", g2 = "eyes",
                    paired = FALSE, var_equal = FALSE,
                    alt = "two.sided", conf = 0.95,
                    plot_style = "box", plot_title = "",
                    plot_xlab = "", plot_ylab = "")
  ok("subsets 3 groups to 2",
     nlevels(pair_data()$group) == 2)
  ref <- t.test(
    jetlag$shift[jetlag$treatment == "control"],
    jetlag$shift[jetlag$treatment == "eyes"])
  ok("subset p matches",
     near(result()$p.value, ref$p.value))
})

# ---- one-sample ------------------------------------------------------
testServer(mod_ttest1_server,
           args = list(data = stub(lizard_tidy, lizard,
                                   "horn.length", "group")), {
  session$setInputs(mode = "single", group = "__all__",
                    mu = 20, alt = "two.sided", conf = 0.95,
                    plot_title = "", plot_xlab = "",
                    plot_ylab = "")
  ref <- t.test(lizard$horn.length, mu = 20)
  ok("one-sample p matches",
     near(result()$p.value, ref$p.value))
  ok("one-sample n correct",
     length(sample_values()$values) == nrow(lizard))

  session$setInputs(group = lv[1])
  ref2 <- t.test(
    lizard$horn.length[lizard$group == lv[1]], mu = 20)
  ok("group subset p matches",
     near(result()$p.value, ref2$p.value))
  ok("plot builds", inherits(the_plot(), "ggplot"))
})

# ---- one-sample: difference-between-columns (paired) ----------------
testServer(mod_ttest1_server,
           args = list(data = stub(NULL, bb)), {
  session$setInputs(mode = "diff", col1 = "before",
                    col2 = "after", mu = 0,
                    alt = "two.sided", conf = 0.95,
                    plot_title = "", plot_xlab = "",
                    plot_ylab = "")
  ref <- t.test(bb$after, bb$before, paired = TRUE)
  ok("diff mode equals paired t-test",
     near(result()$p.value, ref$p.value))
  ok("diff mode estimate matches",
     near(unname(result()$estimate),
          unname(ref$estimate)))
  session$setInputs(col2 = "before")
  ok("same column twice blocked",
     any(grepl("different columns", problems())))
})

# ---- ANOVA ------------------------------------------------------------
testServer(mod_anova_server,
           args = list(data = stub(jetlag_tidy, jetlag,
                                   "shift", "treatment")), {
  session$setInputs(tukey = TRUE, conf = 0.95,
                    plot_style = "box", plot_title = "",
                    plot_xlab = "", plot_ylab = "")
  ref <- summary(aov(shift ~ treatment,
                     data = jetlag))[[1]]
  ok("anova F matches",
     near(anova_row()$f, ref[1, "F value"]))
  ok("anova p matches",
     near(anova_row()$p, ref[1, "Pr(>F)"]))
  tk <- tukey_table()
  reft <- as.data.frame(
    TukeyHSD(aov(shift ~ treatment,
                 jetlag))$treatment)
  ok("tukey has 3 rows", nrow(tk) == 3)
  ok("tukey p values match",
     all(abs(tk$p_adj - reft$`p adj`) < 1e-9))
  ok("tukey labels match TukeyHSD row order",
     identical(gsub(" \u2212 ", "-", tk$comparison,
                    fixed = TRUE),
               rownames(reft)))
  ok("tukey plot builds",
     inherits(the_tukey_plot(), "ggplot"))
  ok("anova plot builds",
     inherits(the_plot(), "ggplot"))
})

# ---- ANOVA refuses 2 groups -----------------------------------------
testServer(mod_anova_server,
           args = list(data = stub(lizard_tidy, lizard)), {
  session$setInputs(tukey = TRUE, conf = 0.95)
  ok("anova blocks 2 groups",
     any(grepl("three or more", problems())))
})
