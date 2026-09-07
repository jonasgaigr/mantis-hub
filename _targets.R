# ---------------------------------------------------------------------------
# _targets.R -- the build graph.
#
# The shape that matters: specs branch dynamically over figures/*.yml, and the
# SVGs branch over the specs. Changing one YAML re-renders exactly that
# figure's three variants and nothing else, because each branch's hash depends
# only on its own spec.
#
#   Rscript -e 'targets::tar_make()'
#   Rscript -e 'targets::tar_visnetwork()'
# ---------------------------------------------------------------------------

library(targets)

SOURCE_DOCX <- "source/MANTIS_Additional_Activity_Proposal_T2.5.3.docx"

tar_option_set(
  packages = c("dplyr", "stringr", "purrr", "tibble", "readr", "yaml", "glue",
               "xml2", "officer"),
  format = "rds"
)

tar_source("R")

list(
  # -- the proposal, watched as a file so a new version re-extracts ---------
  tar_target(proposal, SOURCE_DOCX, format = "file"),

  # -- extraction ----------------------------------------------------------
  tar_target(facts, extract_facts(proposal, out = "data/facts.csv")),
  tar_target(acronyms, extract_acronyms(proposal, out = "data/acronyms.csv")),
  tar_target(tiers, extract_tiers(proposal, out = "data/tiers.csv")),

  # The proposal contradicts itself about which activities are SCALABLE.
  # This target reports the disagreement on every build; it does not resolve
  # it. See MISSING-INPUTS.md.
  tar_target(tier_check, check_tiers_against_budget(tiers, proposal)),

  # -- one branch per spec file --------------------------------------------
  tar_target(spec_files, list.files("figures", pattern = "\\.ya?ml$",
                                    full.names = TRUE)),
  tar_target(spec_file, spec_files, pattern = map(spec_files), format = "file"),
  tar_target(spec, read_spec(spec_file), pattern = map(spec_file),
             iteration = "list"),

  # -- three SVGs per spec -------------------------------------------------
  tar_target(
    figure_svg,
    render_all(spec, outdir = "out"),
    pattern = map(spec),
    format = "file"
  )
)
