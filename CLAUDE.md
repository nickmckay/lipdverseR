# lipdverseR

R package for managing, validating, standardizing, and publishing paleoclimate datasets in the LiPD (Linked PaleoData) format. It is the central orchestration tool for the LiPDverse project, a curated database of linked paleoclimate records.

## What This Package Does

lipdverseR runs the full curation pipeline for ~16 paleoclimate compilations (Temp12k, iso2k, HoloceneHydroclimate, hydroclimate2k, etc.):

1. Ingests LiPD files from a local Dropbox database
2. Fetches QC metadata from Google Sheets
3. Standardizes vocabularies against the PaST ontology
4. Generates per-dataset data pages and project web pages
5. Writes finalized QC back to Google Sheets
6. Tracks changelogs and version numbers

## Key File Locations

- **R/**: All package functions (41 files, ~11k lines)
- **drakePlan.R**: Defines Drake workflows for all 16 compilations -- run this interactively to process a compilation
- **nightlyUpdate.R**: Cron/scheduled automation entrypoint
- **data/standardTables.rda**: Controlled vocabulary for standardization
- **convo.csv**: Mapping table from QC sheet column names to timeseries field names
- **googleQC.csv / fixed.csv**: Cached QC data exports

Local database and HTML output live outside the repo:
- Database: `/Users/nicholas/Dropbox/lipdverse/database/`
- HTML output: `/Users/nicholas/Dropbox/lipdverse/html/`

Google auth credentials are cached in `.secret/` (gitignored).

## Architecture

Each compilation is a Drake plan with a standard pipeline:

```
buildParams -> checkIfUpdateNeeded -> loadInUpdatedData
  -> getQcInfo -> standardizeQCInfo -> createQcFromFile
  -> mergeQcSheets -> updateTsFromMergedQc
  -> createDataPages -> createProjectWebpages
  -> updateGoogleQc -> finalize -> changeloggingAndUpdating
```

Change detection uses MD5 checksums on the database directory. Drake caches intermediate results so only changed targets re-run.

## Key Dependencies

- **lipdR**: Core LiPD file read/write
- **geoChronR**: Chronological data handling
- **drake**: Reproducible workflow orchestration
- **googlesheets4 / googledrive**: QC data lives in Google Sheets/Drive
- **tidyverse**: Data manipulation throughout
- **flexdashboard / dygraphs / leaflet**: Interactive web output
- **RefManageR**: Bibliography/BibTeX management

## Development Workflow

Load the package with `devtools::load_all(".")` (the `library(lipdverseR)` line in drakePlan.R is commented out in favor of this during development).

Google auth uses `nick.mckay2@gmail.com` with the `.secret` cache directory.

## Batch Ingestion Workflow

New files are added to a compilation via a two-step batch workflow (see `hydro2kupdate.R` for a working example):

**Step 1 — Stage:**
```r
result <- prepareAndAddBatch(inputDir = "~/Downloads/newFiles", compilationName = "hydroclimate2k")
```
Reads LiPD files, assigns datasetIds and changelogs, runs non-interactive standardization (`standardizeLipdBatch()`), writes all files to the holding tank (`~/Dropbox/lipdverse/batchHoldingTank/`), and creates a Google Sheet listing vocabulary issues.

**Step 2 — Review issues sheet, then commit:**
```r
commitBatchToDatabase(issuesSheetId = result$issuesSheetId, qcSheetId = h2k_qc_sheet)
```
For each `unknown_vocabulary` row in the issues sheet, set exactly one of:
- `add_synonym=TRUE` + `suggested_value`: update LiPD field to `suggested_value`; add `current_value` as a synonym in the vocab sheet
- `add_synonym=FALSE` + `suggested_value`: update LiPD field to `suggested_value`; leave vocab sheet unchanged
- `new_term=TRUE` + `suggested_value`: add `suggested_value` as a new lipdName with `current_value` as its synonym; update LiPD field to `suggested_value`

`commitBatchToDatabase()` validates resolutions, updates vocabulary Google Sheets (one write per sheet), applies field changes to LiPD files, writes to the real database, updates `datasetsInCompilation` in the QC sheet, and prints a verification summary of which files landed in the database.

**Key vocab sheet registry:** `16edAnvTQiWSQm49BLYn_TaqzHtKO9awzv5C-CemwyTY` maps field names to their Google Sheet IDs.

**TSid handling:** `ensureTSids()` generates missing TSids prefixed with `"miss"` before validation; uniqueness is enforced across the whole batch.

## Exported Functions (96 total)

Key groups:
- **Params/workflow**: `buildParams`, `checkIfUpdateNeeded`, `loadInUpdatedData`, `finalize`, `changeloggingAndUpdating`
- **QC**: `getQcInfo`, `createQcFromFile`, `mergeQcSheets`, `updateTsFromMergedQc`, `standardizeQCInfo`, `updateGoogleQc`
- **Standardization**: `standardizeLipd`, `standardizeLipdBatch`, `hasStandardizedVocabulary`, `getConverter`, `rosettaStone`
- **Batch ingestion**: `prepareAndAddBatch`, `addLipdBatchToDatabase`, `commitBatchToDatabase`, `ensureTSids`
- **Web output**: `createProjectWebpages`, `createProjectDashboards`, `createDataPages`
- **Database**: `addLipdToDatabase`, `createDatabaseReference`, `inThisCompilation`
- **Utilities**: `read_sheet_retry`, `write_sheet_retry`, `directoryMD5`, `coords2country`, `createBib`
