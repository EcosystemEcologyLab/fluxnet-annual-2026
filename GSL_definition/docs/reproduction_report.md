# GSL figure rebuild — reproduction report (Stage 2, step 5)

Match checks against the manuscript's published values, using the main run
(`GPP_NT_VUT_REF`, negative GPP set to 0 -- `GSL_YEAR_SCREEN="d80"`,
`GSL_NEG_GPP_TO_ZERO_MAIN=TRUE`, `GSL_STRICT_GT=TRUE`, per `docs/framework.md`'s resolved
open decisions). Each check is repeated with the as-delivered negative-GPP version to show
how much that assumption matters. `FR-Mej`, `IT-Tor`, `NL-Vkp` (product_id changed between
our 1 and 20 September snapshots, Stage 1) are flagged and every statistic is reported with
and without them. Nothing below was tuned to force a match.

## Check 1: skewness range

| Negative GPP | Scope | Our min | Our max | Published min | Published max |
|---|---|---|---|---|---|
| zero-floored (main) | all 62 | 0.1932 | 3.8757 | 0.19 | 3.88 |
| as-delivered | all 62 | 0.1870 | 3.7078 | 0.19 | 3.88 |
| zero-floored (main) | excl. flagged (59) | 0.1932 | 3.8757 | 0.19 | 3.88 |
| as-delivered | excl. flagged (59) | 0.1870 | 3.7078 | 0.19 | 3.88 |

**Main run: PASS.** Our range (0.19-3.88) matches the published 0.19-3.88 to 2 decimal places.
As-delivered range is 0.19-3.71 -- the low end shifts down (more negative-skew sites) without zero-flooring.

## Check 2: tercile boundaries (0.63, 0.88) and counts (21, 20, 21)

| Negative GPP | Scope | Low | Middle | High | Published |
|---|---|---|---|---|---|
| zero-floored (main) | all 62 | 21 | 20 | 21 | 21 / 20 / 21 |
| zero-floored (main) | excl. flagged (59) | 20 | 19 | 20 | 21 / 20 / 21 |
| as-delivered | all 62 | 24 | 18 | 20 | 21 / 20 / 21 |
| as-delivered | excl. flagged (59) | 23 | 17 | 19 | 21 / 20 / 21 |

**Main run, all 62 sites: PASS**, exactly 21/20/21. Excluding the 3 flagged sites and under
as-delivered GPP, the counts shift (expected -- the boundaries are fixed values from the
manuscript, not re-fit to our data each time; a different skewness distribution will cross
them at different counts).

## Check 3: three Figure 1 correlations (r = 0.63, 0.49, 0.34)

NOT CHECKABLE: neither the Stage 2 prompt nor docs/framework.md names what two quantities each of the three r values (0.63, 0.49, 0.34) correlates. docs/framework.md's Figure 1 section specifies only a per-site annotation (change in mean GSL between 5% and 10% of P99), not a cross-site correlation. Needed to check this: the manuscript's own definition of what is correlated with what for each of the three values (e.g. skewness vs. some per-site quantity, evaluated on three different subsets or at three different thresholds) -- request from Tash/Dave before this check can be attempted.

## Check 4: T95 vs. skewness correlation (published r = -0.68)

| Negative GPP | Scope | Our r | Published r | Difference |
|---|---|---|---|---|
| zero-floored (main) | all 62 | -0.6781 | -0.68 | 0.0019 |
| zero-floored (main) | excl. flagged (59) | -0.6790 | -0.68 | 0.0010 |
| as-delivered | all 62 | -0.5524 | -0.68 | 0.1276 |
| as-delivered | excl. flagged (59) | -0.5547 | -0.68 | 0.1253 |

**Main run, all 62 sites: PASS.** Our r = -0.6781 vs. published -0.68 (difference 0.0019, well within rounding).
As-delivered (no zero-flooring) gives r = -0.5524 -- a materially weaker correlation, not within rounding of -0.68.
This is the strongest evidence in Stage 2 that NEG_GPP_TO_ZERO_MAIN=TRUE is the right choice
(matches the manuscript's own apparent practice), consistent with (not proof of, since Tash's
own code was not available to check directly) the framework's own note that the manuscript's
notes suggest negative GPP was set to zero.

## Check 5: Tash's 22-value grid vs. the fine grid (internal consistency only)

No external Tash GSL/F values are available yet (her screened daily table/code has not been received) -- this is **not** a match to a published value, only a check that the 22-point grid and the 5000-point fine grid agree with each other when computed from the same screened data. Maximum |difference| across all 62 sites x 22 grid points: GSL = 0.000000 day(s), F = 0.00000000.
**Internal consistency: PASS (effectively zero difference).**

## Summary: which checks pass

| Check | Status |
|---|---|
| 1. Skewness range (0.19-3.88) | **PASS** (main run) |
| 2. Tercile counts (21/20/21 at 0.63/0.88) | **PASS** (main run, all 62) |
| 3. Three Figure 1 correlations (r=0.63/0.49/0.34) | **NOT CHECKABLE** -- no definition given |
| 4. T95 vs. skewness (r=-0.68) | **PASS** (main run, r matches to rounding) |
| 5. Tash grid vs. fine grid | **PASS** (internal consistency only, not a published-value check) |

Checks 1, 2 and 4 all pass cleanly on the main run (`GPP_NT_VUT_REF`, negative GPP zero-floored),
and check 4 in particular degrades materially under the as-delivered alternative -- the
strongest signal in this stage that the Stage 2 decisions were the right calls, pending
confirmation from Tash. Check 3 could not be attempted at all; check 5 is not a check against
a published value and should not be read as one.

## Stop

Stopped here for PI sign-off, per the Stage 2 prompt -- no figure has been drawn.
