# Missing inputs

Facts a figure needs that the proposal does not supply, and contradictions in the
proposal that a human has to resolve. Anything listed here is rendered as the
literal string `[TBC]` in the figures until it is settled.

Every entry names the figure and label that wants the fact, so that closing an
entry is a one-line edit to one spec.

---

## 1. Budget tier of A1.5 and A1.6 — the proposal contradicts itself

**Status:** open — needs a decision from the proposal authors.
**Blocks:** `data/tiers.csv`, and the acceptance criterion that it "agrees with
the budget section".

Section 6.1 keys the tiers to cell shading and states the key in its own header
cell: *"ID and short title (blue = CORE, green = enhanced, orange = SCALABLE)"*.
Reading `word/document.xml` gives three fills, unambiguous as hues:

| fill | hue | tier |
| --- | --- | --- |
| `#cfe2f3` | light blue | CORE |
| `#d9ead3` | light green | ENHANCED |
| `#fce5cd` | light orange | SCALABLE |

That yields **SCALABLE = A1.3, A1.4, A1.7, A2.4**.

The Section 11.1 *Additional SCALABLE budget* table instead names six activities:

> 1.3 + 2.4 (scaling contributions); 1.4 + 1.5 + 1.6 + 1.7 (upgrading,
> operationalizing/piloting new sensors, increasing spatial covering of existing
> sensors schemes)

and its equipment line prices four of them:

> 13,104.8 (1.4) + 1400 (1.5) + 180 (1.6) + 6,401.6 (1.7) = 21,086.4

So the budget implies **SCALABLE = A1.3, A1.4, A1.5, A1.6, A1.7, A2.4** —
two more than the shading.

**The conflict is A1.5 and A1.6.**

| activity | Section 6.1 shading | Section 6.2 block shading | Section 11.1 budget |
| --- | --- | --- | --- |
| A1.5 Operationalizing underwater image-based monitoring | blue → CORE | unshaded (`#f7fafc`) → CORE | priced under SCALABLE (€1,400 equipment) |
| A1.6 Piloting underwater acoustic monitoring | green → ENHANCED | green → ENHANCED | priced under SCALABLE (€180 equipment) |

The two shading sources are independent of each other and agree exactly; the
Section 6.2 per-activity blocks reproduce the same tier for all 19 activities.
The budget line is the outlier.

**Not resolved here.** `data/tiers.csv` reports the Section 6.1 shading, because
that is the source carrying its own colour key, and `check_tiers_against_budget()`
prints the disagreement on every build rather than letting it pass quietly.

A plausible reconciliation — that A1.5 and A1.6 are CORE/ENHANCED at lead-partner
level but carry an *additional* per-partner equipment cost when scaled out, which
is what the SCALABLE table's own note ("Equipment costs are additive for both
lead partners *and* participating partners") describes — is **a guess and has not
been applied.** If the authors confirm it, the fix is one line in `extract_tiers()`
plus a note in the README; if instead 6.1 should be re-shaded, the fix is in the
.docx and this pipeline needs no change at all.

---

## 2. `AMI` is never expanded in the proposal

**Status:** open.
**Wanted by:** `figures/fig01-connective-tissue.yml`, node `B1`, L2 label.

`AMI` appears throughout the proposal (AMI traps, AMI processing pipeline,
`D2.5 - One-click AMI processing pipeline`) but is never spelled out. It is
rendered as `AMI [TBC]` so that the acronym test passes honestly rather than by
inventing an expansion. `FAIR` has the same problem but is not currently used in
any figure label.

---

## 3. Number of devices in objective O1

**Status:** open, upstream.
**Wanted by:** not yet used by any figure.

O1's target reads "≥\[N\] devices" — the proposal itself carries an unfilled
placeholder. Any figure quoting a device count must render `[TBC]`.
