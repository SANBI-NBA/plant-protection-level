# Technical workflow for assessing plant species Protection Level
Lize von Staden
2025-08-26

- [Scripts in this workflow](#scripts-in-this-workflow)
- [Technical workflow](#technical-workflow)
  - [Data preparation](#data-preparation)
  - [Protection level assessment](#protection-level-assessment)

## Scripts in this workflow

The R scripts documented here encompasses the full workflow process for
a Protection Level assessment for sampled plant species, starting with
data preparation, assessment, and finally, the production of summary
statistics, which feeds into the graphs and tables displayed in the NBA
website. There are also some supporting scripts, containing reusable
functions, such as connecting to the Protection Level Assessment
Database (PLAD).

Note that many scripts require inputs from an .Renviron file. This file
is used to safely store sensitive data such as database login
credentials, API tokens, and file paths to input data. This file is not
stored as part of the repository, but needs to be set up individually
for each user of this workflow. Instructions for setting up necessary
variables are contained within relevant scripts.

<table style="width:90%;">
<caption>Directory of scripts in this workflow</caption>
<colgroup>
<col style="width: 15%" />
<col style="width: 15%" />
<col style="width: 40%" />
<col style="width: 20%" />
</colgroup>
<thead>
<tr>
<th>Script</th>
<th>Category</th>
<th>Purpose</th>
<th>Dependencies</th>
</tr>
</thead>
<tbody>
<tr>
<td><a
href="apply-pa-effectiveness-x-species.R">apply-pa-effectiveness-x-species.R</a></td>
<td>Assessment</td>
<td>This script transfers general protected area effectiveness as
provided by expert contributors from the PLAD’s protected areas table
<code>tblpaeffectiveness</code> to the species x protected area table
<code>tblspeciesinpa</code>. It applies rules for adjusting
effectiveness for species vulnerable to specific pressures such as
poaching or overgrazing.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="assessment-sense-checks.R">assessment-sense-checks.R</a></td>
<td>Assessment</td>
<td>A script containing various logical tests for Protection Level
assessment results, to detect potential assessment errors.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="calculate-protection-level.R">calculate-protection-level.R</a></td>
<td>Assessment</td>
<td>The main assessment script. It summarises species x pa data, and
then calculates Protection Level without and with the consideration of
effectiveness for each time point in the current assessment.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>function-calculate-protection-level.R</p></li>
</ul></td>
</tr>
<tr>
<td><a href="db-connection.R">db-connection.R</a></td>
<td>Supporting</td>
<td>Script for connecting to the PLAD.</td>
<td><ul>
<li>.Renviron</li>
</ul></td>
</tr>
<tr>
<td><a href="fetch-inat-occurrences.R">fetch-inat-occurrences.R</a></td>
<td>Data preparation</td>
<td>This script fetches the latest research grade occurrence records for
species assessed as under-protected in the previous assessment from
iNaturalist via an API call.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>valid iNaturalist API token</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="fetch-plant-survey-app-plotlist.R">fetch-plant-survey-app-plotlist.R</a></td>
<td>Data preparation</td>
<td>The Plant Survey App allows for field-based density observations of
sampled plant species to be reported directly to the plant Protection
Level assessment. This script processes the latest data stored in the
app, and updated the PLAD with the best available density estimates for
each species recorded.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="function-calculate-protection-level.R">function-calculate-protection-level.R</a></td>
<td>Supporting</td>
<td>Basic function for taking summarised species x protected area data,
comparing it against targets, and calculating Protection Level scores
and categories.</td>
<td></td>
</tr>
<tr>
<td><a href="import-data-to-docker.R">import-data-to-docker.R</a></td>
<td>Supporting</td>
<td>A generic script for importing any data in csv format into a
Docker-based PostgreSQL database</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a href="prepare-map-data.R">prepare-map-data.R</a></td>
<td>Data preparation</td>
<td>A script for reviewing and updating input variables for species’
suitable habitat maps. Species that may need map updates are identified,
and checks are implemented to ensure that mapping input variables are
coded. The script prepares .csv input data on species’ habitat
preferences for creating suitable habitat models.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="process-2025-pa-effectiveness-expert-contributions.R">process-2025-pa-effectiveness-expert-contributions.R</a></td>
<td>Data preparation</td>
<td>In 2025, a process was followed for the first time where protected
area management experts from national and provincial conservation
agencies were invited to contribute protected area effectiveness
ratings. This script processes the outputs of this process into a single
file that can be joined to spatial protected areas data.</td>
<td><ul>
<li>output files of 2025 expert engagement</li>
</ul></td>
</tr>
<tr>
<td><a href="process-landcover.R">process-landcover.R</a></td>
<td>Data preparation</td>
<td>A script for processing 7-class land cover data used in ecological
condition assessments to a 2-class (natural/not natural) version for use
in the plant Protection Level assessment.</td>
<td><ul>
<li><p>.Renviron</p></li>
<li><p>SANBI 7-class land cover data in .tif format</p></li>
</ul></td>
</tr>
<tr>
<td><a href="process-rl-data.R">process-rl-data.R</a></td>
<td>Data preparation</td>
<td>Processing plant SRLI data snapshot extracted from the plant Red
List database. This script assumes that PLAD taxonomy has already been
updated and aligned with the plant Red List database. Both occurrence
data and other Red List data (Red List status, population) are prepared
for integration into the PLAD.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="produce-suitable-habitat-maps.R">produce-suitable-habitat-maps.R</a></td>
<td>Data preparation</td>
<td>Script for generating suitable habitat maps for plant species.
Suitable habitat maps are the most widely used input variables for
calculating protected areas’ contributions to plant conservation
targets</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>Suitable habitat input variables (altitude, vegetation,
landforms)</p></li>
</ul></td>
</tr>
<tr>
<td><a href="qc-occurrence-data.R">qc-occurrence-data.R</a></td>
<td>Data preparation</td>
<td>Script for verifying occurrence records against suitable habitat
maps. Occurrence records that are not within a 10 km distance of
suitable habitat are excluded from the protection level assessment.
Earlier processes in prepare-map-data.R already checked for range
extensions, which where necessary resulted in new maps being generated
in produce-suitable-habitat-maps.R. This process is just a final check,
and specifically reviews iNaturalist data, which does not go through any
other verification processes.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>Suitable habitat maps for all species in .tif format</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="set-pa-effectiveness-2025-assessment.R">set-pa-effectiveness-2025-assessment.R</a></td>
<td>Data preparation</td>
<td>An archival script documenting how protected area effectiveness was
determined for the 2025 plant Protection Level assessment. This script
transfers effectiveness applied to the protected area spatial data used
in NBA 2018 to a new protected area layer representing current best
available knowledge on what was protected in 2017. Rules are applied to
assign effectiveness where there are multiple sources of data on
effectiveness (including from expert contributors).</td>
<td><ul>
<li><p>protected areas data used in NBA 2018 protection level
assessment</p></li>
<li><p>processed land cover data (outputs of
process-landcover.R)</p></li>
<li><p>protected areas data for 2017 and 2024 prepared from
SAPAD</p></li>
</ul></td>
</tr>
<tr>
<td><a href="set-species-targets.R">set-species-targets.R</a></td>
<td>Data preparation</td>
<td>Script for setting plant species conservation targets based on their
known or inferred population size.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="spatial-analysis-species-x-pa-area.R">spatial-analysis-species-x-pa-area.R</a></td>
<td>Assessment</td>
<td>Spatial analysis to calculate protected areas’ contribution to
area-based conservation targets.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>Species’ suitable habitat maps in .tif format</p></li>
<li><p>Protected area maps for each time point in assessment</p></li>
<li><p>Processed land cover data for each time point in
assessment</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="spatial-analysis-species-x-pa-localities.R">spatial-analysis-species-x-pa-localities.R</a></td>
<td>Assessment</td>
<td>Spatial analysis to calculate protected areas’ contribution to
subpopulation-based conservation targets.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>Protected area maps for each time point in assessment</p></li>
</ul></td>
</tr>
<tr>
<td><a
href="spatial-analysis-species-x-pa-population.R">spatial-analysis-species-x-pa-population.R</a></td>
<td>Assessment</td>
<td>Spatial analysis to calculate protected areas’ contribution to
population-based conservation targets.</td>
<td><ul>
<li><p>db-connection.R</p></li>
<li><p>.Renviron</p></li>
<li><p>Protected area maps for each time point in assessment</p></li>
</ul></td>
</tr>
</tbody>
</table>

## Technical workflow

Each plant Protection Level assessment is assigned a unique id
(`assessment_id`) in the PLAD. This id is recorded in all data tables in
the PLAD, and allows for the data used within each Protection Level
assessment to be clearly identifiable.

The first step in a new assessment is to add a new record to
`refassessments` table in the PLAD. This table determines the assessment
ID and records some metadata about the assessment (publication,
assessment date, assessors). Then, if the assessment includes multiple
time points (i.e. best available data is used to recalculate Protection
Level at specified points in the past), a record for each time point
needs to be added to `refassessmenttimepoints`.

Most of the workflow scripts require that the assessment ID of the
current assessment is specified at the start of the script. This ensures
that data processing scripts applies output data to the correct
assessment, and assessment scripts extract the correct data (occurrence
records, targets) applicable to the current assessment. Before running
any scripts, check that the current assessment id is correctly
specified.

### Data preparation

#### Check and update taxonomy

The plant Protection Level assessment is based on a random sample of 900
species that also informs South Africa’s national Red List Index for
plants. The list of species that is included in the sample is managed in
the national plant Red List database. Taxonomic changes to Red Listed
species are also implemented in this database. Therefore the first step
in the assessment is to ensure that the species data in the Protection
Level Assessment Database (PLAD) is aligned with the latest taxonomic
updates in the Red List database.

The first step is to extract the latest plant sample list from the plant
Red List database and compare it to `tblspecies` in the PLAD. Taxonomy
is managed through the following columns in `tblspecies`:

<div id="tbl-taxonomy">

Table 1

| Column | Purpose |
|----|----|
| `species_id` | Unique identifier (Primary Key) for identifying species and linking species data in the PLAD |
| `rl_id` | Unique identifier of species in the Red List database |
| `taxon` | The taxon’s name (without authors). |
| `current_name` | If the value is 1, then the species is currently part of the sample. Species concepts that drop out of the sample due to taxonomic changes are not removed from the list. Instead their value is set to 0. This makes it possible to cross reference to earlier assessments. |
| `new_id` | The `species_id` of the new name for the species in cases where the species concept has been revised. |

</div>

There are essentially two types of taxonomic changes, and they are dealt
with as follows when aligning `tblspecies` to the Red List taxonomy:

- **Simple name changes** - these are instances where the species
  concept remains the same, but taxonomic rules necessitates a name
  change. For example, a species is transferred to a new genus, or a
  subspecies is elevated to species level. For these species, their
  `species_id` remains the same, but its name (`taxon`) and Red List ID
  (`rl_id`) is updated to match the Red List database. (In the Red List
  database, all taxonomic changes result in the assignment of a new
  taxon identifier).

- **Taxonomic revisions** - these are instances where the species
  concept changes. For example one species is split into several
  separate species (may result in a contraction in distribution range),
  or a species is combined (lumped) with another species, because
  re-examination of specimens indicate that there is not sufficient
  differences between them for the two species to remain recognised
  separately. The latter instance may result in an expansion of the
  distribution range. Because these taxonomic changes affect the
  circumscription of species’ distribution range and possibly also known
  habitat preferences, they can have major impacts on the protection
  level assessment. Therefore such revisions are recognised by adding a
  new record to `tblspecies`. The connection to the previous concept is
  maintained through `current_name`, which is set to 0 for the species
  concept that no longer exists (previous name), and `new_id`, which
  links to the `species_id` of the new name for the species.

Cross-check with all the species in `tblspecies` in the Protection Level
Assessment Database (PLAD) where current_name = 1 (should also be 900
species).

Update and correct taxonomy to latest names where necessary - keep note
of any species where taxonomic concept was revised, as these species
will need to have new distribution maps generated. If it is a simple
name change, just change the `taxon` field and `rl_id` field in
`tblspecies`. If it is a revision, add the species’ new name as a new
record to `tblspecies`. Set the old name’s `current_name` value to 0,
and add the new name’s `species_id` to `new_id`.

In some cases, species concepts are revised but the name is not changed.
These species also need new maps. They are recorded in
`tblspecieschanges`.

#### Process new occurrence records

##### **Occurrence record sources**

1.  **Plant Red List database** - processed and imported to protection
    level database using
    [**workflow/process-rl-data.R**](process-rl-data.R)

2.  **Plant Survey App** - fetched, processed, and imported to
    protection level database using
    [**worklfow/fetch-plant-survey-app-plotlist.R**](fetch-plant-survey-app-plotlist.R)

3.  **New/recently updated records from iNaturalist** - fetched,
    processed and imported to protection level database using
    [**workflow/fetch-inat-occurrences.R**](fetch-inat-occurrences.R)

##### **Some notes on the process**

Most records come from the Plant Red List database. These records have
been checked as part of the Red List assessment process. With each PL
assessment, all the accepted records associated with each of the 900
sampled species are shared with the Protection Level assessment. To
avoid duplication, these records are cross-checked against the records
already in the protection level assessment database (table
`geospeciesoccurrences`) and only new records are appended. This process
is managed through the array column `assessments` in
`geospeciesoccurrences`, where each protection level assessment where
the data was used is recorded.

The Plant Survey App data also should not need checking, except in cases
of taxonomic revision. This is because the app builds its survey lists
based on the suitable habitat maps used in the protection level process.

iNaturalist records need to be checked against species’ distribution
maps, and only records within suitable habitat retained.

#### Update Red List assessment data

Red List assessment data - species’ Red List status, as well as
population size, guide the setting of species’ conservation targets.
With each Protection Level assessment, it is important to use the latest
assessment data, but also to record how changes in Red List status are
affecting Protection Level assessments, to enable detection of genuine
changes in Protection Level. Changes in species’ Red List status,
population size, as well as taxonomic revisions impacting PL assessments
are recorded in the table `tblspecieschanges`.

The initial work of comparing previous Red List data to new Red List
data is started in [**workflow/process-rl-data.R**](process-rl-data.R),
but also feeds into later work on revising targets, as population data
is stored in `tbltargets`.

#### Update suitable habitat maps

Suitable habitat maps are the basis for estimating protected areas’
contributions to Protection Level where the number of individuals of a
species in the PA is not known. Suitable habitat maps are modelled based
on three factors or aspects of species’ preferred habitat as described
in literature and on specimens:

1.  **Altitudinal range** (determined from verified occurrence records)
2.  **Vegetation types** aligned with the species’ preferred habitat (up
    until 2025 the 2018 version of the national vegetation map is used)
3.  **Landform** types matched to the species’ preferred habitat

Suitable habitat is mapped where these three factors overlap within the
species known range, which is defined using verified occurrence records.

Changes in species’ known range, for example, through new observations,
can trigger changes in Red List status. It may also mean that some or
all of the variables describing their preferred habitat need updating.
Species that had taxonomic revisions (detected through taxonomy checks
in the first step of the process) will also need to have their suitable
habitat variables revised, and their maps updated.

Species where only population size estimates were revised do not need
their maps to be updated.

Species that need their maps updated are identified through analyses in
[**workflow/prepare-map-data.R**](prepare-map-data.R). Species are
checked for changes in the size and overlap of convex hulls around
points supplied for current assessment vs the previous assessment. Where
there is \<90% overlap, species are checked one by one, and where there
has been range extensions, suitable habitat model input variables are
adjusted in the database to accommodate new observations. Also, all
species that have been taxonomically revised are checked and their
variables adjusted.

This script records which species need to have their maps updated in the
database table `tblspecieschanges`. From this table, input data for new
suitable habitat models are prepared.

Once species needing new maps are identified, and their suitable habitat
input variables checked and updated, new maps are generated using
[**workflow/produce-suitable-habitat-maps.R**](produce-suitable-habitat-maps.R).

#### Review occurrence records

It is necessary to check that occurrence records are not out of range
for species, as these might represent georeferencing problems or
misidentifications. These records should not be used in the Protection
Level assessment.

The script [**workflow/qc-occurrence-data.R**](qc-occurrence-data.R)
checks occurrence records against species’ generalized range maps
derived from their suitable habitat maps, and assigns occurrence records
that pass checks to the current assessment.

#### Set conservation targets

Conservation targets for most species are the extent of suitable habitat
that would include 10 000 individuals, considering that individuals
species occur at variable densities within their habitat. Some
exceptions are made for species with small populations, as well as
poorly known species. The process for considering various factors in
setting targets is described in the Technical Report for this indicator.
The workflow is implemented in
[**workflow/set-species-targets.R**.](set-species-targets.R)

#### Process land cover

Not all areas inside protected areas are in natural condition, and in
some protected areas, there is ongoing conversion of natural areas to
other land uses. Therefore, for each assessment, it is necessary to
prepare land cover data relevant to each time point included in the
assessment. This data has two purposes:

1.  To subtract areas of modelled suitable habitat that are no longer in
    natural condition from protected areas’ contributions to species
    targets.

2.  To assess protected area effectiveness - ongoing loss of natural
    areas inside protected areas is an indicator of poor effectiveness.

Land cover data preparation is implemented in
[**workflow/process-landcover.R**](process-landcover.R).

#### Prepare protected areas data

Protected area spatial data is sourced from the Department of Forestry,
Fisheries and the Environment’s database on protected areas - [SAPAD
(South African Protected Areas Database)](https://www.dffe.gov.za/egis).
This database contains all protected areas declared under the National
Environmental Management: Protected Areas Act, and also keeps track of
protected area degazettements.

This data is processed to produce a non-overlapping vector layer
representing the extent of the protected area network for each time
point included in the assessment that is used by all taxonomic groups to
ensure consistency. Therefore the preparation of protected areas data is
not part of this workflow, but technical documentation can be found at
\[link still to be added\].

**Review protected area effectiveness**

For each assessment, individual protected areas’ effectiveness in
mitigating pressures on plant species is reviewed by protected area
management effectiveness experts from South African conservation
agencies. This information is captured and processed and stored as
effectiveness ratings in `tblpaeffectiveness` in the PLAD. For the 2025
assessment, protected area effectiveness data were processed using
[**workflow/process-2025-pa-effectiveness-expert-contributions.R**](process-2025-pa-effectiveness-expert-contributions.R)
and
[**workflow/set-pa-effectiveness-2025-assessment.R**](set-pa-effectiveness-2025-assessment.R).

### Protection level assessment

#### Spatial analysis

The first step in the Protection Level assessment is to intersect
species’ distribution data (suitable habitat maps and occurrence
records) with protected areas. Intersected data is used to calculate
individual protected areas’ contributions to species conservation
targets. Three different methods of calculating contributions are used,
depending on the type of target set. There are therefore three separate
scripts for calculating protected area contributions, each dealing with
one of the three assessment methods:

1.  Area-based targets:
    [**workflow/spatial-analysis-species-x-pa-area.R**](spatial-analysis-species-x-pa-area.R)

2.  Locality/subpopulation-based targets:
    [**workflow/spatial-analysis-species-x-pa-localities.R**](spatial-analysis-species-x-pa-localities.R)

3.  Population-based targets:
    [**workflow/spatial-analysis-species-x-pa-population.R**](spatial-analysis-species-x-pa-population.R)

#### Apply effectiveness

Protected area effectiveness is scored for each protected area/protected
area section based on expert comments on how well the protected area is
mitigating pressures on plant species, but also what the specific
pressures are. For example, plant poaching only impacts species targeted
for horticultural or medicinal trade. It is therefore necessary to
moderate the general effectiveness scores for protected areas for
selected species according to their specific vulnerabilities. In the
PLAD, data on general protected area effectiveness as well as specific
pressures are stored in the table `tblpaeffectiveness`. This data is
then moderated for species-specific pressures using the script
[**workflow/apply-pa-effectiveness-x-species.R**](apply-pa-effectiveness-x-species.R)
and the outputs are recorded for individual species x protected area
combinations derived in the previous assessment step (`tblspeciesinpa`).

#### Calculate Protection Level

Protection Level is calculated by summing individual protected areas’
contributions and comparing it to species’ conservation targets. Species
are classified into four categories based on the percentage of their
conservation target met. Protection Level is calculated for each time
point in the assessment, to enable an assessment of changes in
Protection Level over time. Protection Level is also calculated with and
without the consideration of protected area effectiveness, to allow the
impact of ineffective protected area management to be quantified. All
these calculations are executed in
[**workflow/calculate-protection-level.R**](calculate-protection-level.R).
The results are stored in `tblplassessment`. The script
[**workflow/assessment-sense-checks.R**](assessment-sense-checks.R)
implements various tests on the results, to ensure logical consistencies
in Protection Level categories against targets, the impact of
effectiveness on Protection Level categories, as well as changes in
Protection Level between time points. Further exploratory analyses and
summaries of the assessment results are coded in
[**workflow/assessment-summary-statistics.qmd**](assessment-summary-statistics.qmd).
These analyses support the development of key messages around plant
Protection Level.
