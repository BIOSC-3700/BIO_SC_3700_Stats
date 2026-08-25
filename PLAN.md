# BIO_SC 3700 Statistics App — Plan

A Shiny app that lets students in BIO_SC 3700 run basic statistical
tests on their own data without writing R code. It runs entirely in the
browser via shinylive, so there is no server to maintain and no
connection limit during a lab section.

## Audience and goals

Undergraduate biology students, most with no prior R experience. The app
should:

- Get from raw data to a result in a few clicks.
- Report output students can read, not raw `print.htest` dumps.
- Make assumptions visible, so students learn to check them rather than
  reflexively running a t-test.
- Produce figures students can put in a lab report.

Explicitly **not** a goal: teaching R syntax. There is no code panel.
Students point and click.

## Deployment

Static shinylive export served from GitHub Pages.

- `shinylive::export("app", "site")` builds the static site.
- GitHub Actions rebuilds and deploys on every push to `main`.
- Students get a URL; nothing to install.

Consequences that shape the design:

- Every R package used must have a webR wasm build available at
  <https://repo.r-wasm.org>. Verify each one before committing to it.
- First load downloads the R runtime plus all packages, so the
  dependency list is a user-facing performance decision. Keep it short.
- No server-side state, no database, no persistence between sessions.
  Everything lives in the browser tab.
- No `rmarkdown`/`pandoc`, so report generation is out.
- File uploads work: the file lands in webR's virtual filesystem and is
  read normally.

## App structure

`bslib::page_navbar()` with four tabs:

1. **Data** — load or type in data, preview it, pick the layout.
2. **Two-sample t-test**
3. **One-sample t-test**
4. **One-way ANOVA** (with optional Tukey HSD)
5. **Two-way ANOVA** (two crossed factors with an interaction)

The Data tab owns a single shared reactive dataset. The three analysis
tabs read from it, so students load data once and can run several tests
on it. Each analysis tab shows a clear "no data loaded yet" state that
links back to the Data tab.

## Data tab

### Input methods

Three options, presented as a radio choice:

- **Upload a file** — `.csv`, `.txt`, `.xlsx`, `.xls`. For Excel files
  with more than one sheet, show a sheet picker after upload.
- **Paste from a spreadsheet** — a `textAreaInput` that accepts a
  tab-delimited block copied out of Excel or Google Sheets. This is the
  path most students will actually use for small datasets.
- **Use an example dataset** — one click to a working dataset, both
  for demonstration and so a student who has broken their own file can
  still follow along in class.

A fourth option, an editable table for typing data in by hand, was
built and then removed: for data that only exists on paper, retyping it
into a spreadsheet and pasting is fewer steps and leaves the student
with a file they still have afterwards.

### Layout selection

The single biggest source of student confusion is data shape, so make it
explicit rather than guessing. After data loads, ask:

- **Long format** — one column of measurements, one column of group
  labels. Student picks which is which.
- **Wide format** — one column per group. The app stacks these into
  long form internally.

The app infers a default from column types — two or more numeric
columns reads as wide, otherwise long — states the guess in the
sidebar, and lets the student override it.

### Validation and feedback

Fail loudly and in plain language, not with an R error:

- Non-numeric values in a measurement column: name the offending column
  and show the first few bad values.
- Fewer than 2 observations in a group.
- Fewer than 2 groups for a two-sample test, fewer than 3 for ANOVA.
- Missing values: report how many were dropped and from where.

Always show a preview table of the parsed data plus a per-group summary
(n, mean, SD, SE, min, max) so students can confirm the app read what
they think it read.

## Analysis tabs

All three follow the same three-panel shape: **Setup** → **Assumptions**
→ **Results**, with a plot alongside the results.

### Two-sample t-test

Setup:

- Measurement column, grouping column.
- If more than 2 groups exist, pick which two to compare.
- Paired vs independent.
- Equal variances assumed? Default to **no** (Welch), which is also
  `t.test()`'s default. Explain in one line why Welch is the safer
  default.
- Alternative hypothesis: two-sided (default), less, greater.
- Confidence level, default 0.95.

Results: group means and SDs, mean difference, t, df, p-value, and the
CI on the difference. Lead with a plain-language sentence
("The mean of Treated (12.4) differs from Control (9.8); p = 0.003"),
then the numeric table underneath.

### One-sample t-test

Setup: measurement column, hypothesized mean μ₀, alternative,
confidence level. Also handle the paired case here by offering a
"difference between two columns" mode, since that is the same test and
students meet it that way.

Results: sample mean, SD, n, t, df, p-value, CI on the mean.

### ANOVA

Setup: measurement column, grouping column, confidence level, and a
checkbox for Tukey HSD.

Results:

- ANOVA table (df, SS, MS, F, p) from `stats::aov()`.
- If variances are unequal, additionally report Welch's ANOVA via
  `stats::oneway.test(var.equal = FALSE)` and say which one to trust.
- Tukey HSD table when requested: each pair, difference, CI, adjusted
  p-value, with significant rows highlighted.
- Compact letter display is a stretch goal — it needs a package or a
  hand-rolled algorithm; skip it in v1.

### Two-way factorial ANOVA

Added after the first four tabs. Two crossed factors plus their
interaction, `aov(value ~ a * b)`.

The column pickers live on this tab rather than the Data tab: the
shared `tidy()` carries a single grouping factor and a factorial design
needs two, so the tab reads `raw()` and selects its own response and
two factors, defaulting to whatever was already chosen on the Data tab.
The other four tabs are untouched.

**Type I sums of squares**, matching `summary(aov())` and textbook
worked examples. Type I is sequential, so on an unbalanced design the
table depends on which factor is entered first — measured at a factor's
SS moving 33.2 → 55.8 on a test case. The tab therefore checks balance
and prints either a green "all SS conventions agree" note or a warning
explaining the order dependence and telling students to swap the two
factor pickers to see it.

Blocking validation: three distinct columns, ≥ 2 levels per factor, no
empty cells (`aov()` returns silent `NA` rows otherwise), and ≥ 2
observations per cell so the interaction has something to be measured
against.

Post-hoc adapts to the interaction:

- **Significant** — Tukey across all cell means, plus simple effects
  (a one-way ANOVA of one factor within each level of the other,
  switchable in either direction). Cell Tukey is computed from the
  equivalent one-way model `aov(value ~ cell)`, which spans the same
  column space as the saturated two-way model and so has identical
  residual SS and df; that equivalence lets it reuse the existing
  hyphen-safe `combn()` labelling and `plot_tukey()` unchanged.
- **Not significant** — Tukey on whichever main effects are
  significant and have more than two levels, taken from the two-way fit
  so the pooled MSE is right.

Normality is checked on the **model residuals**, not per cell —
factorial cells are usually far too small for a per-cell Shapiro-Wilk
to say anything. Equal variance is Levene across the cells, reusing the
existing check.

## Assumption checks

Shown on every analysis tab, in an "Assumptions" panel that is open by
default. Each check gets a green/yellow/red indicator and one sentence
of guidance.

- **Normality** — QQ plot per group plus `stats::shapiro.test()` when
  3 ≤ n ≤ 5000. Note that the t-test is robust at moderate n, so a
  significant Shapiro-Wilk on a large sample is not by itself a reason
  to abandon the test.
- **Equal variance** — Levene's test, implemented directly as a one-way
  ANOVA on absolute deviations from group medians. This is about ten
  lines of code and avoids depending on `car`. Report `bartlett.test()`
  alongside it only if it turns out to be useful.
- **Independence** — cannot be tested; show a short reminder about study
  design.
- **Outliers** — flag points beyond 1.5 × IQR in the plot and say
  outright that flagging is not permission to delete.

Since nonparametric alternatives are out of scope for v1, a failed
assumption check tells students what it means and suggests talking to
the instructor. Leave a note in the code where Wilcoxon and
Kruskal-Wallis would slot in later.

## Plots

One `ggplot2` figure per tab, built from the same helper so they share a
theme.

- **Two-sample** — boxplot with jittered points overlaid, one box per
  group. Paired data instead gets a before/after slope plot.
- **One-sample** — histogram or dot plot with the sample mean, its CI,
  and a reference line at μ₀.
- **One-way ANOVA** — boxplot with jittered points across groups, plus
  a second plot of Tukey confidence intervals with a reference line at
  zero.
- **Two-way ANOVA** — an interaction plot (cell means, ±1 SE, one line
  per level of the color factor) and a dodged boxplot. This is the only
  place in the app where color carries identity rather than emphasis,
  so it draws on the validated categorical slots. Only the first three
  clear the all-pairs separation gates — slot 4 puts yellow beside
  orange at normal-vision ΔE 13.7 — so every level also gets its own
  point shape and a label at the end of its line, and identity never
  rests on hue. Past eight levels no palette works, so the plot is
  replaced by a note suggesting the axes be swapped.

Controls: plot title, axis labels, and a point/box/violin toggle.
Download as PNG at a fixed 300 dpi so figures are usable in a report;
`ggsave()` works under webR, so the download is a real button.

Color is spent only where it carries meaning. The x-axis already
encodes group identity on the group plots, so all groups share one
accent color instead of each taking a hue — which also means that
subsetting three groups down to two never repaints the survivors. The
two places color does mean something, a flagged outlier and a Tukey
interval that clears zero, were checked with a palette validator and
are backed by a shape or a printed p-value so the cue is never color
alone.

## Repository layout

```
app/
  app.R              # page_navbar, wires modules together
  R/
    mod_data.R       # data input, layout selection, validation
    mod_ttest2.R     # two-sample t-test
    mod_ttest1.R     # one-sample t-test
    mod_anova.R      # ANOVA + Tukey
    assumptions.R    # Levene, Shapiro, outlier helpers
    plots.R          # shared theme and plot builders
    format.R         # number formatting, p-value formatting, palette
    ui_helpers.R     # badges, verdict boxes, empty states
  www/
    styles.css
  data/              # example datasets, inside app/ so export copies them
.github/workflows/
  deploy.yml         # shinylive export -> GitHub Pages
PLAN.md
README.md
```

Shiny's automatic sourcing of `app/R/` survives the shinylive export,
so no explicit `source()` calls are needed.

## Dependencies

Keep this list short; every entry costs load time.

| Package | Used for | Notes |
| --- | --- | --- |
| `shiny` | app framework | required |
| `bslib` | layout, theming, cards | required |
| `ggplot2` | all plots | |
| `dplyr` | group summaries | |
| `readr` | CSV/TSV import | kept |
| `readxl` | Excel import | confirmed available in the webR repo |
| `DT` | preview table | kept; 1.7 MB |

Everything statistical comes from base `stats`: `t.test()`, `aov()`,
`TukeyHSD()`, `oneway.test()`, `shapiro.test()`. No modeling packages.

## Build and deploy

Local development:

```r
shiny::runApp("app")                       # fast iteration, plain R
shinylive::export("app", "site")           # build static site
httpuv::runStaticServer("site")            # preview the wasm build
```

Test in plain R first, then confirm in the wasm build before pushing —
they differ in package availability and in file-system behavior.

CI (`.github/workflows/deploy.yml`): on push to `main`, set up R,
install `shinylive`, run the export, and publish `site/` with
`upload-pages-artifact` and `deploy-pages`. Cache the shinylive web
assets; they are large and stable.

## Style

Follows the repo's R conventions: hard wrap at 78 characters, native
`|>` pipe, tidyverse over base where it reads better, `fs` for file
paths, `glue()` for strings, explicit `return()` in functions, US
spellings (`summarize`, `color`). Do not name any data frame `df`.

## Non-goals for v1

- Regression, correlation, chi-square, and ANOVA beyond two factors.
- Type II or Type III sums of squares, and random or mixed effects.
- Nonparametric tests.
- Effect sizes beyond the confidence intervals that come with the
  standard output.
- Saving or sharing sessions.
- Generated R code panels.

## Open questions — resolved during the build

1. **Does `readxl` have a webR build?** Yes. So do `DT`, `readr`,
   `ggplot2`, `dplyr`, `fs`, and `glue`; all were checked against the
   `PACKAGES` index at <https://repo.r-wasm.org> before any code was
   written.
2. **Does `ggsave()` produce a downloadable PNG under webR?** Yes.
   A probe app built with shinylive reported `capabilities("png")` and
   `capabilities("cairo")` both `TRUE` and wrote a 40 KB PNG. The
   right-click fallback is not needed.
3. **Does `app/R/` auto-sourcing survive `shinylive::export()`?** Yes.
   The exported build loads all eight files with no explicit `source()`
   calls.
4. **How large is the first-load payload?** About 25 MB of package
   binaries after dropping `tidyr` (see below), down from 40 MB. webR
   fetches packages lazily, and the browser caches them, so this is a
   once-per-student cost.
5. **Which example datasets ship?** Four generic ones in `app/data/`,
   chosen to exercise distinct code paths: a 3-group long-format set
   (significant ANOVA where exactly one Tukey pair is non-significant),
   a 2-group long-format set, a wide-format set with unequal group
   sizes to exercise NA handling, and a paired before/after set. Swap
   in real course data whenever it is available.

## Where the build diverged from this plan

- **Example data lives in `app/data/`, not a top-level `data/`.**
  `shinylive::export()` only copies the app directory.
- **`tidyr` was dropped.** It was there for one `pivot_longer()` call,
  but it imports stringr, which pulls in stringi — 14 MB, more than a
  third of the whole download, for a single reshape. The wide-to-long
  stack is now base R, verified to produce output identical to
  `pivot_longer()` including within-group row order (which the paired
  t-test depends on).
- **Layout inference is simpler than described.** Two or more numeric
  columns reads as wide, otherwise long. The earlier column-counting
  rules mis-classified a paired `subject`/`before`/`after` table. The
  guess is stated in the sidebar either way.
- **Tukey comparison labels are built, not parsed.** `TukeyHSD()` names
  its rows `"B-A"`, which is ambiguous when a group name itself
  contains a hyphen, so labels are regenerated from `combn()` of the
  factor levels and verified to line up with the row order.
- **No dark mode.** Figures are meant to end up in a lab report, so
  they are drawn on white, and the UI stays light to match rather than
  offering a toggle that the downloaded PNG would not honor.

## Implementation phases — all complete

1. ~~**Skeleton**~~ — navbar, four tabs, file upload, preview table.
2. ~~**Two-sample t-test**~~ — full path from data to result to plot,
   with validation and the assumptions panel.
3. ~~**One-sample t-test**~~ — including the paired-difference mode.
4. ~~**ANOVA and Tukey**~~ — with the Tukey CI plot and Welch fallback.
5. ~~**Input methods**~~ — paste-from-spreadsheet (the editable table
   was built here and later removed).
6. ~~**Polish**~~ — plain-language summaries, plot download, example
   datasets, Help tab.

## Verification

- 44 `shiny::testServer()` checks cover the data module (layout
  inference, both pivots, paste parsing, non-numeric coercion
  reporting, NA accounting) and all three analysis modules, comparing
  every statistic against a direct `t.test()` / `aov()` / `TukeyHSD()`
  call.
- The hand-rolled Levene's test matches `car::leveneTest(center =
  median)` to six decimal places.
- Both the plain-Shiny and the WebAssembly builds were run in a
  browser and inspected: all three tabs render, plots draw, and the
  numbers agree between the two builds.

Still worth doing before the semester: run the app past a couple of
students with their own messy spreadsheets. Every validation message
in here is a guess about how data will actually arrive.
