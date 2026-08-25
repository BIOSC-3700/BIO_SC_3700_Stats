suppressMessages({
  library(shiny); library(bslib); library(ggplot2); library(dplyr)
  library(tibble); library(readr); library(readxl); library(DT)
  library(glue); library(fs)
})
for (f in list.files("R", full.names = TRUE)) source(f)
ok <- function(label, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))
near <- function(a, b) isTRUE(abs(a - b) < 1e-9)

fac <- read_csv("data/factorial_growth.csv", show_col_types = FALSE)
stub <- function(raw_df, vlab = "biomass_g", glab = "genotype") {
  list(raw = reactive(raw_df), tidy = reactive(NULL), info = reactive(NULL),
       labels = reactive(list(value = vlab, group = glab)))
}
base_inputs <- function(session, ...) {
  defaults <- list(response = "biomass_g", fac_a = "genotype",
    fac_b = "treatment", tukey = TRUE, simple_dir = "a_within_b",
    conf = 0.95, swap_axes = FALSE, plot_style = "box",
    plot_title = "", plot_xlab = "", plot_ylab = "")
  overrides <- list(...)
  defaults[names(overrides)] <- overrides
  do.call(session$setInputs, defaults)
}

# ---- core model ------------------------------------------------------
testServer(mod_anova2_server, args = list(data = stub(fac)), {
  base_inputs(session)
  ref <- summary(aov(biomass_g ~ genotype * treatment, data = fac))[[1]]
  at <- anova_terms()
  ok("no problems on clean factorial", length(problems()) == 0)
  ok("F values match aov (all 3 terms)",
     all(mapply(near, at$f, ref[1:3, "F value"])))
  ok("p values match aov (all 3 terms)",
     all(mapply(near, at$p, ref[1:3, "Pr(>F)"])))
  ok("df match aov", all(at$df == ref[1:3, "Df"]))
  ok("residual df matches", at$df_resid == ref[4, "Df"])
  ok("design detected as balanced", isTRUE(is_balanced()))
  ok("interaction flagged significant", isTRUE(interaction_sig()))
  ok("cells built with 4 cells", nlevels(cells()$cell) == 4)

  # cell-level Tukey vs. reference one-way cell model
  cd <- cells()
  refc <- as.data.frame(TukeyHSD(aov(value ~ cell, data = cd))$cell)
  tk <- tukey_cells()
  ok("cell Tukey p matches TukeyHSD",
     all(abs(tk$p_adj - refc$`p adj`) < 1e-9))
  ok("cell Tukey labels match row order",
     identical(gsub(" − ", "-", tk$comparison, fixed = TRUE),
               rownames(refc)))
  ok("cell model resid df equals two-way",
     df.residual(cell_fit()) == df.residual(fit()))

  # simple effects vs. manual per-level aov
  se <- simple_effects()
  manual <- sapply(levels(cd$b), function(l) {
    s <- cd[cd$b == l, ]; s$a <- droplevels(s$a)
    summary(aov(value ~ a, data = s))[[1]][1, "Pr(>F)"] })
  ok("simple effects match manual aov",
     all(abs(se$p - unname(manual)) < 1e-9))
  ok("simple effects one row per level of B",
     nrow(se) == nlevels(cd$b))

  ok("residual normality check runs",
     check_normality_residuals(fit())$status %in%
       c("ok", "warn", "fail", "info"))
  ok("interaction plot builds", inherits(the_plot(), "ggplot"))
  ok("grouped box plot builds", inherits(the_box_plot(), "ggplot"))
  ok("colors not exhausted", !too_many_colors())

  # swapping axes must not change the model, only the plot
  session$setInputs(swap_axes = TRUE)
  ok("swap axes keeps F unchanged", near(anova_terms()$f[3], ref[3, "F value"]))
  ok("swap axes flips plot roles", plot_roles()$xlab == "treatment")
})

# ---- main-effect Tukey when interaction is absent --------------------
set.seed(9)
add <- expand.grid(g = c("g1", "g2", "g3"), t = c("t1", "t2"),
                   r = 1:6, stringsAsFactors = FALSE)
add$y <- 10 + 3 * (add$g == "g2") + 6 * (add$g == "g3") +
  2 * (add$t == "t2") + rnorm(nrow(add), 0, 1.5)
testServer(mod_anova2_server, args = list(data = stub(add, "y", "g")), {
  base_inputs(session, response = "y", fac_a = "g", fac_b = "t")
  ok("additive data: interaction not significant", !interaction_sig())
  reft <- as.data.frame(
    TukeyHSD(aov(y ~ g * t, data = add), which = "g")$g)
  tk <- tukey_main("a")
  ok("main-effect Tukey matches TukeyHSD",
     all(abs(tk$p_adj - reft$`p adj`) < 1e-9))
  ok("main-effect Tukey labels match rows",
     identical(gsub(" − ", "-", tk$comparison, fixed = TRUE),
               rownames(reft)))
})

# ---- Type I order dependence on unbalanced data ----------------------
unb <- fac[-(1:5), ]
testServer(mod_anova2_server, args = list(data = stub(unb)), {
  base_inputs(session)
  ok("unbalanced design detected", !is_balanced())
  ss_ab <- anova_terms()$table$`Sum Sq`[1]
  session$setInputs(fac_a = "treatment", fac_b = "genotype")
  ss_ba <- anova_terms()$table$`Sum Sq`[2]
  ok("Type I SS is order-dependent when unbalanced",
     abs(ss_ab - ss_ba) > 1e-6)
})
testServer(mod_anova2_server, args = list(data = stub(fac)), {
  base_inputs(session)
  ss_ab <- anova_terms()$table$`Sum Sq`[1]
  session$setInputs(fac_a = "treatment", fac_b = "genotype")
  ss_ba <- anova_terms()$table$`Sum Sq`[2]
  ok("Type I SS is order-INdependent when balanced", near(ss_ab, ss_ba))
})

# ---- blocking validation ---------------------------------------------
empty_cell <- fac[!(fac$genotype == "mutant" & fac$treatment == "drought"), ]
testServer(mod_anova2_server, args = list(data = stub(empty_cell)), {
  base_inputs(session)
  ok("empty cell blocked", any(grepl("empty", problems())))
})
one_per <- fac |> group_by(genotype, treatment) |> slice(1) |> ungroup()
testServer(mod_anova2_server, args = list(data = stub(one_per)), {
  base_inputs(session)
  ok("n=1 per cell blocked", any(grepl("at least 2", problems())))
})
testServer(mod_anova2_server, args = list(data = stub(fac)), {
  base_inputs(session, fac_b = "genotype")
  ok("duplicate column choice blocked",
     any(grepl("three different columns", problems())))
})
one_lvl <- fac; one_lvl$treatment <- "control"
testServer(mod_anova2_server, args = list(data = stub(one_lvl)), {
  base_inputs(session)
  ok("single-level factor blocked", any(grepl("only one level", problems())))
})

# ---- the non-significant-interaction branch actually renders ---------
testServer(mod_anova2_server, args = list(data = stub(add, "y", "g")), {
  base_inputs(session, response = "y", fac_a = "g", fac_b = "t")
  body <- output$body
  ok("body renders", !is.null(body$html) && nchar(body$html) > 200)
  ok("uses main-effect post-hoc branch",
     grepl("Post-hoc: main effects", body$html))
  ok("omits cell-comparison branch",
     !grepl("which combinations differ", body$html))
  ok("main-effect Tukey section renders",
     grepl("Tukey HSD for g", output$main_effect_tukey$html))
  ok("2-level factor omitted from post-hoc",
     !grepl("Tukey HSD for t", output$main_effect_tukey$html))
  ok("verdict states no interaction",
     grepl("No interaction", output$verdict$html))
  ok("balance notice renders",
     grepl("Balanced design", output$balance$html))
})

# ---- the significant-interaction branch renders ----------------------
testServer(mod_anova2_server, args = list(data = stub(fac)), {
  base_inputs(session)
  body <- output$body
  ok("interaction branch shows cell comparisons",
     grepl("which combinations differ", body$html))
  ok("interaction branch omits main-effect card",
     !grepl("Post-hoc: main effects", body$html))
  ok("verdict states dependence", grepl("depends on", output$verdict$html))
  ok("simple effects table renders", nchar(output$simple_table) > 100)
  ok("cell counts table renders", nchar(output$counts_table) > 50)
})
