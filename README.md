# PRB Monitoring website
This is the repository for the PRB monitoring website, owned by the PRB and managed by EUROCONTROL.
The PRB monitoring website is published on sesperformance.eu.

For more information contact oscar.alfaro@eurocontrol.int.

# Production
The master branch is **main**, so be **CAREFUL** about the changes to make on this branch.

# Concrete steps to publish to production
There are 2 possible scenarios:

1. merge from a development branch where docs has already been properly generated

2. change directly in master and regenerate either the whole site, or the pages (States/years) affected by the change

Option 2. should only be used for of emergency/tiny changes not worth the overhead of creating a new branch and develop/PR/test/merge...(even if it is a safer way of working.)

Option 1. is more for a release preparation where different contributions eventually from different branches have been combined for publication in next release.

# Development
## Development of a feature
Development of a feature or change in the dashboards can happen in each person's favorite branch. When the changes are deemed ready for next release they can be merged in the relevant release branch.

## Deployment to 'test'
Every time you run the  `build_site.R` script, a new site/pdf document will be built, either on the test or production folders, depending on your selected toggles. 

Keep `test_check <- TRUE` during the development phase so your site/pdf is deployed on the test folder, which is defined in the `params_project.R` script. 

You can visualise your pages either directly opening the .html files, or going to your web browser and replacing the string `\\ihx-vdm05\LIVE_var_www_performance$` in your folder address by `www.eurocontrol.int/performance`.

# Website/pdf generation
The dashboard is generated from templated Quarto documents (.qmd). There are different templates for the different types of pages/pdfs to be generated:
- SES RP4, 
- Network Manager,
- MUAC,
- State and 
- Home page. 

There are also different yaml files for web and pdf generation.

Each State/year combination is a fully independent quarto website, although it is all designed to look like an integrated website.

The master files .qmd, .yml and .css files are stored in the folder `\_original_files`.

The steps to which are laid out in the script `R\build_site.R`.

The script `R\create_pages.R` sets up the different quarto framework files (.yml, .qmd, etc) depending on the type of State/stakeholder.

# Source data/text
The source data and text feeding this dashboard is the responsibility of the PRB Secretariat. Before publication, it is their responsibility to review and validate all data and text in the Dashboard.

The data and the text are stored in the folders defined in the `r\params_project.R` and `r\params_site.R` scripts, respectively:
1. `data_folder`: contains all data and the text for the PRB conclusions
2. `data_folder_a2`: contains all the text for the PRU analysis and NSA input boxes in the dashboard

Note that if the folder 2. were to be moved to another location, the structure and names of its subfolders for the different years and KPAs should be kept so as the code can find them. 

There are four sources of information:
- Data: Coming mostly from PRU databases, but also NM and PRB Secretariat (mostly for Safety).
- NSA input: Coming from the NSA monitoring reports (methodology under development).
- PRU analysis: Coming from the NSA monitoring reports and PRU files (methodology under development).
- PRB conclusions: Provided by the PRB secretariat.

# Publication
Go to [app.netlify.com](app.netlify.com) and login with email with the user performancereviewunit@gmail.com. Enrico and Oscar have the password.
Select 'sesperformance.eu' in the list of sites, go to 'deploys' and in the 'drag & drop' section just drop the production folder:
- RP3: `\\ihx-vdm05\LIVE_var_www_performance$\oscar\prb-monitoring-prod`
- RP4: `\\ihx-vdm05\LIVE_var_www_performance$\prb-monitoring\rp4\prod`
