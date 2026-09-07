# MANTIS proposal figures

A reproducible pipeline that renders the figures for the MANTIS proposal
(Biodiversa+ Task 2.5.3) into three SVG variants each — slide, A4 and poster —
from a single declarative spec per figure.

The proposal `.docx` is the only permitted source of facts. Everything a label
is allowed to quote is extracted from it first, and the test suite refuses to
build a figure that quotes anything else.

```
Rscript -e 'targets::tar_make()'                    # extract, then render
Rscript -e 'testthat::test_dir("tests/testthat")'   # check the nine rules
quarto render                                       # document and deck
```

In VS Code: <kbd>Ctrl/Cmd</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd> builds,
**Run Test Task** runs the rules.

---

## Layout

```
source/           the proposal .docx, read-only
data/             generated: facts.csv, acronyms.csv, tiers.csv
figures/          one .yml spec per figure -- the only file you edit
out/              generated: <id>-{slide,a4,poster}.svg
R/
  extract.R       .docx -> data/*.csv
  spec.R          read + validate a spec
  profiles.R      the three output profiles
  render.R        spec + profile -> SVG string
  checks.R        parsing helpers the tests are written against
  document.R      glue between the built SVGs and the Quarto documents
tests/testthat/   one file per rule
_targets.R        the build graph
figures.qmd       A4 variants, captions, alt text (html + pdf)
slides.qmd        revealjs, L1 only -- every figure, one slide each
pitch.qmd         revealjs, the 7-10 minute pitch for decision makers
```

## Prerequisites

R ≥ 4.3 and Quarto. `renv::restore()` installs the R packages from
`renv.lock`.

PDF output additionally needs an SVG-to-PDF converter that keeps text live:

- **`rsvg-convert`** (preferred) — `brew install librsvg`,
  `apt install librsvg2-bin`.
- **a Chromium/Chrome/Edge binary**, used automatically if `rsvg-convert` is
  absent. It prints the SVG with its text still text, which is the property
  that matters. Set `MANTIS_SVG2PDF=rsvg` or `=chromium` to force one.

HTML and revealjs need neither; they embed the SVG directly.

The figures deliberately do **not** route SVG through a LaTeX package: the
usual ones either rasterise the figure or outline the glyphs, and an outlined
figure stops being searchable, selectable and editable — which is most of the
point of emitting real `<text>`.

---

## How to add a figure

1. Write `figures/fig02-something.yml`. Copy
   `figures/fig01-connective-tissue.yml` and change it; the schema is below.
2. `Rscript -e 'targets::tar_make()'`. The new spec becomes its own branch in
   the graph and produces `out/fig02-something-{slide,a4,poster}.svg`.
3. `Rscript -e 'testthat::test_dir("tests/testthat")'`. Every rule is
   parameterised over every spec in `figures/`, so the new figure is checked
   the moment it exists — you do not write a test for it.
4. It appears in `figures.qmd` and `slides.qmd` automatically; both read
   `figures/` at render time. `pitch.qmd` does not — it is an argument rather
   than a catalogue, and names the one figure it wants by id.

If the figure needs a fact the proposal does not contain, write the literal
`[TBC]` and add an entry to `MISSING-INPUTS.md` naming the figure and label
that wants it. Do not invent the number.

### Spec schema

```yaml
id: fig01
message: "One sentence a reader can say after five seconds."
caption: "1-2 sentences in the proposal's register. Serves all three variants."
alt: "Under 250 characters."
orientation: landscape          # or portrait; only the poster changes shape

nodes:
  - id: B1
    shape: block                # block | branch | terminal
    x: 0.022                    # unit square; multiplied by the canvas
    'y': 0.520                  # quote it -- see the YAML note below
    w: 0.118
    h: 0.130
    label_w: 0.132              # width of the annotation column beneath
    label_y: 0.775              # top of that column (default: under the box)
    label_side: below           # or above
    budget: SCALABLE            # optional; the ONLY licence for a reserved hue
    labels:
      - {tier: L1, role: block,   text: "Sensors in the field"}
      - {tier: L2, role: caption, text: "AMI [TBC] traps · CamAlien"}

edges:
  - {from: B1, to: B2, kind: flow, gate: G1}    # kind: flow | feeds
gates:
  - id: G1
    'on': [B1, B2]              # quote it too
    label_w: 0.132
    label_y: 0.680
    labels: [...]
brackets:
  - {id: P1, spans: [B1, B3], side: top, 'y': 0.200, labels: [...]}
bands:
  - {id: P3, spans: [B1, END], side: bottom, 'y': 0.872, h: 0.054, labels: [...]}
legend:
  - {tier: L1, kind: flow, text: "solid arrow: data flows to"}
  - {tier: L1, key: gate,  text: "numbered bar: quality-control gate"}
```

A bracket label may carry `at: start | middle | end` to sit at one end of the
span rather than over its middle — what a note about a handover *between* two
brackets needs.

> **YAML 1.1 gotcha.** The bare tokens `y`, `n`, `on`, `off`, `yes` and `no`
> are booleans in YAML 1.1, so an unquoted `y:` key parses as `TRUE` and
> collides with `on:`. `read_spec()` turns implicit bool resolution off, so
> unquoted keys work here — but quoting them keeps the file portable to other
> YAML readers, which is why the shipped spec does.

### Tiers, roles and why font size is not in the spec

`tier` is **how prominent** a label is, and decides which media show it:

| tier | slide | A4 | poster |
| --- | :-: | :-: | :-: |
| L1 | ● | ● | ● |
| L2 | | ● | ● |
| L3 | | | ● |

A tier a profile does not show is dropped entirely, not hidden — the SVG does
not contain it. An element whose every label is dropped goes with them, which
is why gate 4 has no bar on the slide: it is an L2-only gate and has nothing to
say there. Gate numbers come from position in the spec, so a gate keeps its
number in every medium.

`role` is **what kind of text** it is — `title`, `block`, `gate`, `caption`,
`note`, `legend` — and it is the *only* thing that decides size. The size
itself lives in `R/profiles.R`, per profile:

| profile | viewBox | tiers | min font |
| --- | --- | --- | --- |
| slide | 1920 × 1080 | L1 | 32 |
| a4 | 1000 × 700 | L1, L2 | 11 |
| poster | 3370 × 2384 (or 2384 × 3370) | L1, L2, L3 | 24 |

A spec cannot set a font size. If it could, a label written for A4 would be
unreadable on a slide and nobody would notice until the room went quiet. The
poster's extra space goes to annotation, not to bigger boxes: `block` grows
less than the canvas does and `caption`/`note` take the gain.

### Colour

Three hues are reserved and mean one thing each:

| hue | meaning |
| --- | --- |
| `#1F4E79` | CORE |
| `#2E7D32` | ENHANCED |
| `#C55A11` | SCALABLE |

They appear only on an element whose spec sets `budget:`. Everything else uses
a neutral ink ramp — `#111111`, `#555555`, `#999999`, `#E6E6E6` — and is
distinguished additionally by position, stroke style and label, so the figures
survive greyscale printing and colour blindness. Note in particular that
**packages use the ink ramp, never the tier hues**: packages and budget tiers
are different things, and the hues belong to the tiers.

---

## How the tier colours are recovered from the .docx

Section 6.1 keys the three budget tiers to cell shading and states the key in
its own header cell:

> ID and short title (blue = CORE, green = enhanced, orange = SCALABLE)

Any markdown or plain-text conversion of the document drops that shading, and
with it the entire tier assignment. `extract_tiers()` therefore reads
`word/document.xml` out of the `.docx` directly:

```r
x     <- xml2::read_xml(unz(docx, "word/document.xml"))
cells <- xml2::xml_find_all(x, "//w:tbl//w:tc", ns)
fill  <- xml2::xml_attr(xml2::xml_find_first(cells, ".//w:shd", ns), "fill")
```

It finds the Section 6.1 table by looking for the header cell that states the
colour key — not by table index, which would break the moment a table is added
above it. Fills are classified by **hue**, not by matching a hard-coded list of
hex values, so a re-shaded document still resolves:

| fill | hue | tier |
| --- | --- | --- |
| `#cfe2f3` | light blue | CORE |
| `#d9ead3` | light green | ENHANCED |
| `#fce5cd` | light orange | SCALABLE |

Every build prints the fills it found and the mapping it chose, and stops if a
fill cannot be classified.

### The one thing this pipeline will not decide for you

The proposal contradicts itself. Section 6.1's shading gives
**SCALABLE = A1.3, A1.4, A1.7, A2.4**. The Section 11.1 SCALABLE budget table
names six activities, adding **A1.5 and A1.6**.

`check_tiers_against_budget()` runs on every build, prints both lists, and
reports the disagreement. It does not pick a winner. `data/tiers.csv` carries
the Section 6.1 shading because that is the source that states its own colour
key, and because the Section 6.2 per-activity blocks independently reproduce
the same tier for all nineteen activities — two shading sources agree and the
budget line is the outlier. A plausible reconciliation exists and has
deliberately **not** been applied. See `MISSING-INPUTS.md`.

`tests/testthat/test-10-extraction.R` pins the disagreement, so that when
somebody fixes the `.docx` the test fails and `MISSING-INPUTS.md` gets updated
rather than going quietly stale.

---

## What each test enforces, and why

Every rule is parameterised over every spec in `figures/`, and reads rendered
SVG **as XML** rather than as a string. A regex over the file cannot tell a
`fill` on a `<rect>` from the word `#1F4E79` sitting inside a `<desc>`, and
that difference is exactly what rule 2 is about.

| # | Rule | Why it exists |
| --- | --- | --- |
| 1 | **No invented facts.** Every numeric token and every M/D/O/R/A ID in any label is in `data/facts.csv`, or is the literal `[TBC]`. | A figure is a claim about the proposal. A number that is not in the proposal is a number somebody will ask about in a review meeting. |
| 2 | **Reserved hues.** The three tier colours appear only where the spec declares a `budget:`. | A colour that means two things means nothing. |
| 3 | **Arrows are labelled.** Every edge states a `kind`; every kind used appears in that figure's legend, and each renders with its own marker shape. | An unlabelled arrow is a relationship the reader has to guess at. Shape, not colour, carries it — so it survives greyscale. |
| 4 | **Acronyms.** Every all-caps token is expanded *within the same figure* at the same-or-lower tier, or carries `[TBC]`, or is allow-listed (`MANTIS`, `EU`, `AI`, `GBIF`, `TBC`). | Somebody reading the slide does not have the proposal open. An expansion that only exists at L3 does not help them. |
| 5 | **Cluster size.** No visual cluster exceeds seven elements. | Seven is about where a group stops being a group and starts being a list. |
| 6 | **Print safety.** No `<image>`, gradient, filter, `<script>` or external `href`; text is real `<text>`. | A figure that depends on anything outside its own file will be opened somewhere that cannot fetch it. Gradients turn to mud on a press. |
| 7 | **Type size.** The smallest rendered font clears the profile's minimum; every role clears it in every profile; no stroke is thinner than 1.5px in the A4 viewBox. | The minimum is a property of how far away the reader is sitting. |
| 8 | **Slide survivability.** Every node has an L1 label; no L1 label exceeds eight words or reads as a multi-clause sentence; L2/L3 text is absent from the slide, not merely hidden; every L1 label fits inside its box in every profile. | A slide label is read in about a second from the back of a room. |
| 9 | **Determinism.** Rendering twice gives byte-identical output, with no timestamps and no generated ids. | Without it `targets` cannot tell a real change from noise, and a one-word edit produces an unreadable diff. |

### What counts as a cluster

A cluster is a set the reader holds in mind *at once*: the nodes a bracket or
band groups under one label, the legend, the stack of labels on one element. A
left-to-right chain is **not** a cluster — it is read in sequence — which is
why fig01's eight-node spine passes while an eight-entry legend would not.

### The rules were mutation-checked

Each was verified to fail on a deliberately broken spec: an invented number, an
invented ID, an edge kind missing from the legend, a bare acronym, `AMI`
without `[TBC]`, a nine-word L1 label, a multi-clause L1 label. A rule that
never fails is decoration.

---

## The revision protocol

**Change one label in one spec; all three media follow.**

```
$ vim figures/fig01-connective-tissue.yml     # edit one L1 label
$ Rscript -e 'targets::tar_make()'
✔ spec_file completed
✔ spec completed
✔ figure_svg completed
✔ ended pipeline [3 completed, 6 skipped]
```

The three extraction targets and the proposal file target are skipped: nothing
re-reads the `.docx`. All three of that figure's SVGs are rewritten, and no
other figure is touched — each spec is its own branch in the graph, so a branch
is invalidated only by its own spec.

Then `quarto render`, and the document and the deck pick up the new SVGs. The
caption and alt text come from the spec at render time, so they cannot drift
away from the figure: there is exactly one copy of each, in the spec.

Never edit `out/*.svg`. It is generated, and the next build will overwrite it.
