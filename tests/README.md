# Test harness

`shiny::testServer()` checks for every module, plus the generators that
produced the example datasets in `app/data/`.

**Run them from the `app/` directory** — the scripts `source()` the
module files with `list.files("R")` and read the example CSVs with
paths relative to `app/`:

```bash
cd app
Rscript ../tests/test_data.R      # 16 checks: data module
Rscript ../tests/test_tests.R     # 28 checks: t-tests + one-way ANOVA
Rscript ../tests/test_anova2.R    # 41 checks: two-way factorial ANOVA
```

All 85 should print `PASS`. Every statistic is compared against a
direct `t.test()` / `aov()` / `TukeyHSD()` call rather than a stored
expected value, so the tests stay honest if the app is refactored.

`mkdata.R` and `mkfactorial.R` regenerate `app/data/*.csv`. They are
seeded, so rerunning reproduces the shipped files exactly. Only rerun
them if you intend to change the example data — the factorial set is
tuned so that genotype has no effect under control (p = 0.28) but a
large one under drought (p < 0.0001), which is what makes the
interaction worth teaching.
