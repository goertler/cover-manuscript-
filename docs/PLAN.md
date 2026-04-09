# Analysis Plan

## Goal

Combine two independent full-text review datasets, standardize key variables
with controlled vocabularies, and produce summary tables and visualizations for:

- publication metadata, especially year published
- species
- lifestage
- study location

## Raw Data

- Raw files:
  - `data-raw/full_text_review_LA.csv`
  - `data-raw/full_text_review_PG.csv`
- Both files contain 51 records.
- `id` values overlap perfectly across the two files and appear to be the
  natural first-pass merge key.
- The files share the same core review fields, but `LA` includes extra columns
  (`new`, `updated why?`).
- One or both CSVs are not clean UTF-8 and require explicit encoding handling
  during import.

## Known Data Issues

- Column names are inconsistent and include extra whitespace.
- Inclusion/exclusion columns differ slightly in wording between reviewers.
- `PG` is missing many `doi` and `title` values relative to `LA`.
- Publication year is not stored as its own field and will need to be parsed
  from `full_text_file`.
- Species, lifestage, and location are recorded as free text with inconsistent
  granularity, abbreviations, punctuation, and multi-value entries.
- Some studies appear to include multiple species and/or multiple lifestages in
  one record.
- Location values range from river reach names to states/provinces to countries.

## Workflow

1. Import both raw files with explicit encoding and standardized column names.
2. Validate that `id` is the correct merge key and check for any row-level
   mismatches by `doi`, `title`, and `full_text_file`.
3. Build a reviewer-comparison table that preserves both reviewers' original
   responses side by side.
4. Define reconciliation rules for each analysis field:
   - publication year
   - species
   - lifestage
   - study location
5. Use `PG` as the governing reviewer for inclusion (`Y/N`) and exclude
   non-included records from the final analysis dataset.
6. Create cleaned analysis datasets:
   - one paper-level dataset
   - optional long-format lookup tables for species, lifestage, and location
     if records can map to multiple categories
7. Apply controlled vocabularies and document all recoding decisions.
8. Produce descriptive summaries and publication-ready plots.
9. Save derived data and figures in reproducible outputs under `data-derived/`
   and `figures/`.

## Data Products

- `data-derived/reviewer_comparison.csv`
- `data-derived/papers_clean.csv`
- `data-derived/paper_species.csv`
- `data-derived/paper_lifestage.csv`
- `data-derived/paper_locations.csv`
- `figures/year_published.*`
- `figures/species.*`
- `figures/lifestage.*`
- `figures/location.*`

## File Structure

Use a simple phase-based structure that keeps raw inputs untouched, makes
standardization decisions explicit, and separates tabular outputs from plotting
code.

```text
cover-manuscript-/
├── PLAN.md
├── README.md
├── run_analysis.R
├── data-raw/
│   ├── full_text_review_LA.csv
│   ├── full_text_review_PG.csv
│   └── gis/
│       └── ... raw GIS inputs if needed
├── data-derived/
│   ├── reviewer_comparison.csv
│   ├── papers_included.csv
│   ├── paper_species.csv
│   ├── paper_lifestage.csv
│   ├── paper_locations.csv
│   ├── crosswalk_species.csv
│   ├── crosswalk_lifestage.csv
│   ├── crosswalk_location.csv
│   └── gis/
│       └── ... cleaned spatial layers if needed
├── figures/
│   ├── year_published.png
│   ├── species.png
│   ├── lifestage.png
│   └── location_map.png
├── scripts/
│   ├── 01_import_merge.R
│   ├── 02_filter_included.R
│   ├── 03_standardize_species.R
│   ├── 04_standardize_lifestage.R
│   ├── 05_standardize_location.R
│   └── 06_summarize_plot.R
└── R/
    ├── io.R
    ├── species.R
    ├── lifestage.R
    ├── location.R
    ├── plotting.R
    └── utils.R
```

### Structure Principles

- Keep `data-raw/` immutable.
- Treat `data-derived/` as the main home for inspectable analysis outputs.
- Store crosswalk tables as explicit files rather than burying all recoding in
  code.
- Use long-format lookup tables for species, lifestage, and location so a
  single paper can map to multiple standardized values.
- Keep spatial source data separate from cleaned spatial outputs if mapping  
  inputs are added later.

### Script Location

- Reusable functions should live in `R/`.
- Stepwise analysis scripts should live in `scripts/`.
- A thin root-level orchestrating script such as `run_analysis.R` can source
  functions from `R/` and execute the scripts in order.
- Keep the orchestrating script minimal so the real logic stays in `R/` and
  `scripts/`, where it is easier to test and revise.

## Standardization

### Inclusion/exclusion reconciliation

- `PG` inclusion (`Y/N`) governs the final analysis set.
- Reviewer disagreement does not need to be preserved or summarized in final
  outputs.
- Exclusion reasons can remain in the comparison data for auditability, but they
  are not a main analysis target.

### Publication metadata

- Publication year will be parsed from the last four digits of `full_text_file`.
- No external metadata enrichment is required for the current analysis plan.

### Species

- Standardize to exact species using Latin binomials.
- Preserve run/designation where relevant, e.g. spring-run Chinook.
- Multi-species studies should contribute to multiple species records in a
  long-format table.
- Ambiguous group labels should be coded into unresolved higher-level buckets
  when exact species cannot be defensibly inferred from the source text, e.g.
  `unresolved salmonid`.

### Lifestage

- Use one controlled-vocabulary lifestage representation rather than separate
  coarse and detailed fields.
- Store lifestage in a long-format lookup table so each paper can map to
  multiple lifestages.
- There are 55 distinct raw lifestage strings across the two files before
  cleaning.
- Several raw values clearly need normalization before deriving the coarse
  vocabulary:
  - missing or undefined values, e.g. `NA`, `N/A`, blank, `unclear`,
    `undescribed`
  - descriptive but non-stage phrases, e.g. `biomass, no life stage
    information`, `represent food supply for juvenile salmonids`
  - size- or age-based strings, e.g. `11-14 cm`, `age-1`, `large parr (...)`
  - near-duplicate multi-stage strings that differ only in punctuation or
    wording
- Controlled vocabulary:
  - `egg`
  - `fry`
  - `parr`
  - `smolt`
  - `subyearling`
  - `yearling`
  - `juvenile_unspecified`
  - `adult`
- Working rule:
  - use only true life stages in the lookup table
  - map `spawning` to `adult` rather than treating it as a separate lifestage
  - treat `holding`, `migration`, and similar terms as behaviors/context, not
    lifestage categories

### Location

- The main spatial design choice is how to represent geometry for mapping
  because raw entries mix:
  - rivers and streams
  - watersheds/basins/catchments
  - states/provinces and countries
  - multi-site textual groupings
- Multi-location studies can contribute to multiple location records in a
  long-format table.
- Recommended structure that will feed into mapping choice:
  - `location_raw`
  - `location_standardized`
  - `location_type` (`river`, `watershed`, `admin_area`, `country`, `multi_site`, 
    `not_applicable`)
  - `country`
  - `state_province`
  - `country`
  - `geometry_name`
  - `geometry_type`
  - `geometry_source`

#### Location Mapping

##### Option A: Point-based reference map

- Represent each study with one or more reference points.
- Use a centroid or representative point for rivers, watersheds, and admin areas.
- Best when the source text is heterogeneous and often imprecise.
- Advantages:
  - easiest to standardize consistently
  - visually simple
  - avoids mixing incompatible polygon and line geometries in one main figure
- Limitations:
  - discards spatial extent
  - can imply false precision if points are placed from vague text

##### Option B: Geometry-by-feature-type map

- Use the most natural geometry available for each standardized location:
  - river/stream as lines
  - watershed/basin as polygons
  - state/province/country as polygons
- Best when preserving the type of study geography matters.
- Advantages:
  - closest to the way locations are described
  - preserves extent better than points
- Limitations:
  - harder to build and explain
  - mixed geometry types can make one figure visually busy
  - grouped multi-site studies may still need fallback handling

##### Option C: Hierarchical map plus table

- Standardize all studies to a common admin unit for the main map, then keep the
  original river/watershed detail in tables or supplemental figures.
- Example:
  - main figure maps counts by state/province or country
  - supplemental table lists named rivers/watersheds per paper
- Best when you need a clean publication figure and many records are too
  inconsistent for direct feature mapping.
- Advantages:
  - most robust and reproducible
  - easiest for readers to interpret
  - avoids overclaiming spatial precision
- Limitations:
  - loses hydrologic detail in the main figure
  - may feel coarse if river identity is scientifically important
- Selected option for the main analysis: Option C.

##### Option D: Two-figure approach

- Make one coarse overview map using common polygons or points, and a second
  figure or supplement for higher-resolution named river/watershed features
  where possible.
- Best when both broad geographic coverage and hydrologic specificity matter.
- Advantages:
  - balances interpretability and detail
  - lets uncertain records stay coarse while precise records remain precise
- Limitations:
  - more work
  - requires a clear rule for which studies qualify for detailed geometry

## Visualization Plan

- publication-year figure based on parsed year from `full_text_file`
- species frequency figure using standardized Latin names
- lifestage figure based on counts from the single long-format lifestage lookup
  table
- location figure using Option C:
  - main map at a common admin level
  - river/watershed detail retained in a companion table or supplement

## Reproducibility Notes

- Keep the original reviewer text fields intact in at least one derived file.
- Record all controlled vocabularies in code and, if helpful, a separate
  crosswalk table.
- Separate raw import, cleaning/reconciliation, and plotting into distinct
  scripts or functions.
- Preserve a side-by-side reviewer comparison file for auditability, even though
  final analysis will follow `PG` inclusion.
- Explicitly qualify all namespaces (except for base R functions).
- Keep `renv` dependencies management up-to-date.
- Explain the "why" of code decisions in comments, not the what.
