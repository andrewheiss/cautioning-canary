
<!-- README.md is generated from README.Rmd. Please edit that file -->

# NGO Repression as a Predictor of Worsening Human Rights Abuses

[Suparna Chaudhry](http://www.suparnachaudhry.com/) • Department of
International Affairs • Lewis & Clark College  
[Andrew Heiss](https://www.andrewheiss.com/) • Andrew Young School of
Policy Studies • Georgia State University

------------------------------------------------------------------------

[![OSF
DOI](https://img.shields.io/badge/OSF-10.17605%2FOSF.IO%2FMTR6X-blue)](https://dx.doi.org/10.17605/OSF.IO/MTR6X)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5715402.svg)](https://doi.org/10.5281/zenodo.5715402)

> Suparna Chaudhry and Andrew Heiss. 2022. [“NGO Repression as a
> Predictor of Worsening Human Rights
> Abuses,”](https://doi.org/10.1177/0899764020971045) *Journal of Human
> Rights* (forthcoming).

**All this project’s materials are free and open:**

-   [Download the data](#data)
-   [See the analysis notebook
    website](https://stats.andrewheiss.com/cautioning-canary/)

![Open data](img/data_large_color.png)  
![Open](img/materials_large_color.png)

------------------------------------------------------------------------

## Abstract

An increasing number of countries have recently cracked down on
non-governmental organizations (NGOs). Much of this crackdown is
sanctioned by law and represents a bureaucratic form of repression that
could indicate more severe human rights abuses in the future. This is
especially the case for democracies, which unlike autocracies, may not
aggressively attack civic space. We explore if crackdowns on NGOs
predict broader human rights repression. Anti-NGO laws are among the
most subtle means of repression and attract lesser domestic and
international condemnation compared to the use of violence. Using
original data on NGO repression, we test whether NGO crackdown is a
predictor of political terror, and violations of physical integrity
rights and civil liberties. We find that while de jure anti-NGO laws
provide little information in predicting future repression, their
patterns of implementation—or de facto civil society repression—predicts
worsening respect for physical integrity rights and civil liberties.

------------------------------------------------------------------------

This repository contains the data and code for our paper. Our pre-print
is online here:

> Suparna Chaudhry and Andrew Heiss. 2022. “NGO Repression as a
> Predictor of Worsening Human Rights Abuses”“. Accessed January 5,
> 2022. Online at <https://dx.doi.org/10.17605/OSF.IO/MTR6X>

## How to download and replicate

You can either [download the compendium as a ZIP
file](/archive/master.zip) or use GitHub to clone or fork the compendium
repository (see the green “Clone or download” button at the top of the
GitHub page).

We use the [**renv**
package](https://rstudio.github.io/renv/articles/renv.html) to create a
stable version-specific library of packages, and we use the [**targets**
package](https://docs.ropensci.org/targets/) to manage all file
dependencies and run the analysis. ([See this for a short helpful
walkthrough of
**targets**.](https://books.ropensci.org/targets/walkthrough.html)).

To reproduce the findings and re-run the analysis, do the following:

1.  Download and install these fonts (if you’re using Windows, make sure
    you right click on the font files and choose “Install for all users”
    when installing these fonts):
    -   [Cochineal](https://fontesk.com/cochineal-typeface/)
    -   [Inter](https://fonts.google.com/specimen/Inter)
    -   [Linux Libertine
        O](https://www.cufonfonts.com/font/linux-libertine-o) (also
        [here](https://sourceforge.net/projects/linuxlibertine/))
    -   [Libertinus Math](https://github.com/alerque/libertinus)
    -   [InconsolataGo](https://github.com/ryanoasis/nerd-fonts/tree/master/patched-fonts/InconsolataGo)
2.  [Install R](https://cloud.r-project.org/) (and preferably
    [RStudio](https://www.rstudio.com/products/rstudio/download/#download)).
    -   If you’re using macOS, [install XQuartz
        too](https://www.xquartz.org/), so that you have access to the
        Cairo graphics library
    -   If you’re using Windows, [install RTools
        too](https://cran.r-project.org/bin/windows/Rtools/) and add it
        to your PATH so that you can install packages from source if
        needed
3.  Open `cautioning-canary.Rproj` to open an [RStudio
    Project](https://r4ds.had.co.nz/workflow-projects.html).
4.  Make sure you have a working installation of LaTeX:
    -   *Easy-and-recommended way*: Install the [**tinytex**
        package](https://yihui.org/tinytex/) by running
        `install.packages("tinytex")` in the R console, then running
        `tinytex::install_tinytex()`
    -   *Easy-but-requires-huge-4+-GB-download way*: Download TeX Live
        ([macOS](http://www.tug.org/mactex/);
        [Windows](https://miktex.org/))
5.  If it’s not installed already, R *should* try to install the
    **renv** package when you open the RStudio Project for the first
    time. If you don’t see a message about package installation, install
    it yourself by running `install.packages("renv")` in the R console.
6.  Run `renv::restore()` in the R console to install all the required
    packages for this project.
7.  Run `targets::tar_make()` in the R console to automatically download
    all data files, process the data, run the analysis, and compile the
    paper and appendix.

Running `targets::tar_make()` will create several helpful outputs:

1.  All project data in `data/`
2.  An analysis notebook website in `analysis/_site/index.html`
3.  PDF, HTML, and Word versions of the manuscript in
    `manuscript/output/`

## Data

<div style="color:#FF4136">

**NB**: If you’re reproducing this project, it is best if you rely on
the **targets** package and all the functions in
`R/funs_data-cleaning.R` to handle the cleaning, processing, tidying,
and merging. All the analysis in the project depends on having a
**targets**-created object named `panel`.

</div>

For reference, though, we export CSV and RDS files of the final dataset:

-   [**`data/derived_data/panel.csv`**](data/derived_data/panel.csv) and
    [**`data/derived_data/panel.rds`**](data/derived_data/panel.rds):
    CSV and RDS versions of our final combined dataset
-   [**`data/derived_data/panel_lagged.csv`**](data/derived_data/panel_lagged.csv)
    and
    [**`data/derived_data/panel_lagged.rds`**](data/derived_data/panel_lagged.rds):
    CSV and RDS versions of our final combined dataset with lagged and
    leaded versions of variables

This data is derived from many different original data sources. See the
[“Process and merge data” page of our analysis
notebook](https://stats.andrewheiss.com/cautioning-canary/01_data-overview.html)
for details about both the original data and the merging process.

-   **Chaudhry NGO restrictions**: We use counts of anti-NGO legal
    barriers from [the replication
    data](https://doi.org/10.7910/DVN/JHOGNX) for Suparna Chaudhry’s
    “The Assault on Civil Society: Explaining State Repression of NGOs”
    (*International Organization*, 2022).
    -   `data/raw_data/Chaudhry restrictions/SC_Expanded.dta`
-   **Political Terror Scores**: We use data from the [Political Terror
    Scale (PTS) project](http://www.politicalterrorscale.org/) to
    measure state repression. This project uses reports from the US
    State Department, Amnesty International, and Human Rights Watch and
    codes political repression on a scale of 1-5.
    -   `data/raw_data/Political Terror Scale/PTS-2019.RData`, v2019
-   **Latent Human Rights Protection Scores**: We use Chris Fariss’s
    [Latent Human Rights Protection
    Scores](https://doi.org/10.7910/DVN/RQ85GK), which are estimates
    from fancy Bayesian models that capture a country’s respect for
    physical integrity rights.
    -   `data/raw_data/Latent Human Rights Protection Scores/HumanRightsProtectionScores_v4.01.csv`,
        v4.01
-   **Varieties of Democracy data**: We use a bunch of variables from
    the [Varieties of Democracy (V-Dem)
    project](https://www.v-dem.net/en/).
    -   `data/raw_data/Country_Year_V-Dem_Full+others_R_v10/V-Dem-CY-Full+Others-v10.rds`,
        v10
-   **UN data**: We use [the **WDI**
    package](https://vincentarelbundock.github.io/WDI/) to collect data
    from the World Bank. However, we don’t use WDI data for GDP and % of
    GDP from trade because the WDI data is incomplete (especially
    pre-1990, but that’s not an issue in this project) To get around
    that, we create our own GDP and trade measures using data directly
    from the UN (at [UNData](https://data.un.org/)). They don’t have a
    neat API like the World Bank, so you have to go to their website and
    export the data manually. We collect three variables:
    -   [GDP at constant 2015
        prices](http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a102%3bcurrID%3aUSD%3bpcFlag%3a0)
        (`data/raw_data/UN data/UNdata_Export_20210118_034054729.csv`)
    -   [GDP at current
        prices](http://data.un.org/Data.aspx?q=gdp&d=SNAAMA&f=grID%3a101%3bcurrID%3aUSD%3bpcFlag%3a0)
        (`data/raw_data/UN data/UNdata_Export_20210118_034311252.csv`)
    -   [Population](https://population.un.org/wpp/Download/Standard/Population/)
        (`data/raw_data/UN data/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx`)
-   **UCDP/PRIO Armed Conflict Data**: We use [UCDP/PRIO Armed Conflict
    data](https://ucdp.uu.se/downloads/index.html#armedconflict) to
    create an indicator marking if a country-year was involved in armed
    conflict that resulted in at least 25 battle-related deaths.
    -   `data/raw_data/UCDP PRIO/ucdp-prio-acd-191.csv`, v19.1
-   **Natural Earth shapefiles**: We use the [“Admin 0 - Countries”
    1:110m cultural
    shapefiles](https://www.naturalearthdata.com/downloads/110m-cultural-vectors/)
    for maps.
    -   `data/raw_data/ne_110m_admin_0_countries/`
-   **2020 Civicus Monitor ratings**: CIVICUS rates countries [using a
    5-item scale of civic space
    openness](https://monitor.civicus.org/widgets/world/), but getting
    their data in a machine-readable format is a little tricky. We
    downloaded [the standalone embeddable
    widget](https://monitor.civicus.org/widgets/world/) as an HTML file
    with `wget https://monitor.civicus.org/widgets/world/` and saved it
    as `index_2021-03-19.html`. We then extracted the `COUNTRIES_DATA`
    variable embedded in a `<script>` tag
    (`xpath = /html/body/script[5]`), which is JSON-ish, but not quite.
    The **jsonlite** R package couldn’t parse it for whatever reason,
    but some random online JSON formatter and validator could, so we ran
    it through that and saved the resulting clean file.
    -   `data/raw_data/Civicus/index_2021-03-19.html`
    -   `data/raw_data/Civicus/civicus_2021-03-19.json`

**BUT AGAIN** we *highly* recommend just relying on the **targets**
pipeline to construct this data rather than doing it all yourself! ([See
this for a short helpful
walkthrough.](https://books.ropensci.org/targets/walkthrough.html))

## ⚠️🐦: “Cautioning canary” project name

Because project titles change all the time with revisions, rewriting,
and peer review, we use “cautioning canary” as an internal-to-us project
name that won’t change. (We chose this name before the
[**codename**](http://svmiller.com/codename/) was invented—had it
existed already, we would have used that to generate the name.)

## Licenses

**Text and figures:** All prose and images are licensed under Creative
Commons ([CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/))

**Code:** All code is licensed under the [MIT License](LICENSE.md).

## Contributions

We welcome contributions from everyone. Before you get started, please
see our [contributor guidelines](CONTRIBUTING.md). Please note that this
project is released with a [Contributor Code of Conduct](CONDUCT.md). By
participating in this project you agree to abide by its terms.
