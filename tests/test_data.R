suppressMessages({
  library(shiny); library(bslib); library(ggplot2); library(dplyr)
  library(tibble); library(readr); library(readxl)
  library(DT); library(glue); library(fs)
})
for (f in list.files("R", full.names = TRUE)) source(f)
ok <- function(label, cond) cat(sprintf("[%s] %s\n", if (isTRUE(cond)) "PASS" else "FAIL", label))

# ---- long-format example ------------------------------------------
testServer(mod_data_server, {
  session$setInputs(source = "example",
                    example = "horned_lizards.csv")
  ok("long example loads", !is.null(raw()) && nrow(raw()) == 184)
  ok("infers long layout", inferred_layout() == "long")
  session$setInputs(layout = "long", value_col = "horn.length",
                    group_col = "group")
  td <- tidy()
  ok("tidy has 184 rows", nrow(td) == 184)
  ok("tidy has 2 groups", nlevels(td$group) == 2)
  ok("value is numeric", is.numeric(td$value))
  ok("no problems flagged", length(tidy_result()$problems) == 0)
  ok("labels carry column names",
     labels()$value == "horn.length")
})

# ---- wide-format example -------------------------------------------
testServer(mod_data_server, {
  session$setInputs(source = "example",
                    example = "blackbird_antibodies.csv")
  ok("infers wide layout", inferred_layout() == "wide")
  session$setInputs(layout = "wide",
                    wide_cols = c("before", "after"))
  td <- tidy()
  ok("wide pivots to 26 usable rows", nrow(td) == 26)
  ok("no rows dropped", tidy_result()$dropped == 0)
  ok("group order follows columns",
     identical(levels(td$group), c("before", "after")))
})

# ---- paste path ----------------------------------------------------
testServer(mod_data_server, {
  session$setInputs(
    source = "paste",
    paste_text = "site\tmass\nA\t1.5\nA\t2.5\nB\t3.5\nB\t4.5",
    paste_go = 1
  )
  ok("tab-delimited paste parses", nrow(raw()) == 4)
  session$setInputs(layout = "long", value_col = "mass",
                    group_col = "site")
  ok("paste tidies to 4 rows", nrow(tidy()) == 4)
})

# ---- non-numeric values are reported, not silently dropped ---------
testServer(mod_data_server, {
  session$setInputs(
    source = "paste",
    paste_text = "g,v\nA,1\nA,2\nB,3 cm\nB,4",
    paste_go = 1
  )
  session$setInputs(layout = "long", value_col = "v",
                    group_col = "g")
  probs <- tidy_result()$problems
  ok("bad number is reported",
     any(grepl("could not be read", probs)))
  ok("bad row dropped", nrow(tidy()) == 3)
})

# ---- same column chosen twice --------------------------------------
testServer(mod_data_server, {
  session$setInputs(source = "example",
                    example = "lion_noses.csv",
                    layout = "long",
                    value_col = "age", group_col = "age")
  ok("duplicate column choice caught",
     any(grepl("cannot be", tidy_result()$problems)))
})
