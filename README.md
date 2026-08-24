# BIO_SC 3700 — Basic Statistics

A point-and-click Shiny app that lets students run basic statistical
tests on their own data without writing R code. It runs entirely in the
browser through [shinylive](https://posit-dev.github.io/r-shinylive/)
(WebAssembly), so there is no server to maintain, no connection limit
during a lab section, and nothing for students to install.

## Tabs

| Tab | What it does |
| --- | --- |
| **Data** | Upload a CSV/Excel file, paste from a spreadsheet, type data in, or load an example. Declares long vs. wide layout and validates the result. |
| **Two-sample t-test** | Welch by default, with pooled-variance and paired options. |
| **One-sample t-test** | One group against a fixed μ₀, or the difference between two paired columns. |
| **One-way ANOVA** | One-way ANOVA with optional Tukey HSD, plus Welch's ANOVA when variances are unequal. |
| **Two-way ANOVA** | Two crossed factors with an interaction term, Type I SS, and interaction-aware post-hoc. |

Every analysis tab shows the same three things: a plain-language
headline, the numbers, and an assumptions panel (normality,
equal variance, outliers, independence) that is open by default.

The two-way tab leads with the interaction, because a significant one
changes what the main effects mean. It reports **Type I sums of
squares** to match `summary(aov())` and textbook worked examples, and
because Type I is computed sequentially it checks whether the design is
balanced — on a balanced design every SS convention agrees, and on an
unbalanced one the tab says so and explains that factor order matters.
The post-hoc adapts: a significant interaction gets Tukey across cell
means plus simple effects, and an absent one gets Tukey on whichever
main effects are significant.

## Running it locally

```r
shiny::runApp("app")                 # normal Shiny, fast to iterate on
httpuv::runStaticServer("site")      # preview the built site

# Build the static WebAssembly site. template_params sets the browser
# tab title; without it the page is titled "Shiny App".
shinylive::export(
  "app", "site",
  template_params = list(title = "BIO_SC 3700 Statistics")
)
```

Test in plain R first, then confirm in the wasm build before pushing —
the two differ in package availability and file-system behavior.

## Deploying

The app is live at
**<https://kmiddleton.github.io/BIO_SC_3700_Stats/>**

Pushing to `main` triggers `.github/workflows/deploy.yml`, which runs
the shinylive export and publishes `site/` to GitHub Pages. No manual
step is needed. `site/` is generated and is not committed.

Setting this up on a fork takes two things: the repository must be
public (Pages publishes from private repos only on a paid plan), and
Pages must be switched on once with **Settings → Pages → Source:
GitHub Actions**, which is also
`gh api -X POST repos/OWNER/REPO/pages -f build_type=workflow`. Enable
it before the first push, or the build will succeed and the deploy step
will fail with "Pages site not found".

## Layout

```
app/
  app.R              navbar, wires the modules together
  R/
    mod_data.R       input, layout selection, validation
    mod_ttest2.R     two-sample t-test
    mod_ttest1.R     one-sample t-test
    mod_anova.R      one-way ANOVA + Tukey
    mod_anova2.R     two-way factorial ANOVA + interaction
    assumptions.R    Levene, Shapiro-Wilk, outlier helpers
    plots.R          shared theme and plot builders
    format.R         number/p-value formatting, palette
    ui_helpers.R     badges, verdict boxes, empty states
  www/styles.css
  data/              example datasets
```

## Notes on dependencies

Every `library()` call in `app/app.R` is a download on a student's
first visit, so the list is deliberately short: shiny, bslib, ggplot2,
dplyr, tibble, readr, readxl, DT, glue, fs. That comes to about 25 MB
of WebAssembly package binaries.

Two dependencies were removed on purpose:

- **car** — Levene's test is implemented directly in
  `R/assumptions.R` as a one-way ANOVA on absolute deviations from
  group medians. It matches `car::leveneTest(center = median)` exactly.
- **tidyr** — it was used for a single `pivot_longer()` call, but it
  imports stringr, which pulls in stringi. That one reshape was costing
  14 MB, so the wide-to-long stack is done in base R.

All statistics come from base `stats`: `t.test()`, `aov()`,
`TukeyHSD()`, `oneway.test()`, `shapiro.test()`.

See [PLAN.md](PLAN.md) for the design rationale and what is
deliberately out of scope.
