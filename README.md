## Open Cyber Page

<div class="nhsuk-warning-callout">
  <h3 class="nhsuk-warning-callout__label">
    Important<span class="nhsuk-u-visually-hidden">:</span>
  </h3>
  <p>This project is currently in development. An overview of methodology and caveats are given below. For more information please contact <a href="mailto:datascience@nhs.net">datascience@nhs.net</a>. <br>Opinions expressed in this page are not representative of the views of NHS England and any content here should not be regarded as official output in any form. For more information about the NHS England Transformation Directorate please visit our <a href="https://transform.england.nhs.uk/">official website</a>.
   </p>
</div>

The <b>Data Security and Protection Toolkit</b> is an online self-assessment tool that allows organisations to measure their performance against the National Data Guardian’s 10 data security standards.

All organisations that have access to NHS patient data and systems must use this toolkit to provide assurance that they are practising good data security and that personal information is handled correctly. This includes trusts, commissioners and CSUs.

The data displayed in this page will show all organisations open as of 16th of November 2023. Any organisations which have merged and do not have a DSPT status will inherit the DSPT status of the merged organisation. Any organisations which have failed to submit a 22/23 DSPT Status will be considered as 'Not Published'.

The following information displays data taken from a specific snapshot of the DSPT data. The latest status information was downloaded on <b>16th November 2023</b>.

You can find the previous iteration of this page showing the 21/22 DSPT data at <a href="https://nhsx.github.io/open-cyber/21_22">21/22 Open Cyber Page</a>

For more information on the Data Security and Protection Toolkit, please visit the <a href="https://www.dsptoolkit.nhs.uk/">DSPT portal</a>.

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

Used in the following projects:
 - [Open Source Healthcare Statistics](https://nhsx.github.io/open-health-statistics/) [[GitHub](https://github.com/nhsx/open-health-statistics)]
 - [NHSX Publications from PubMed API](nhsx.github.io/nhsx-publications) [[GitHub](https://github.com/nhsx/nhsx-publications)] (in development)
 - [Digital Health Insights](https://nhsx.github.io/digital-health-insights/) [[GitHub](https://github.com/nhsx/digital-health-insights)] (in development)
 - [The Prescribing Cost of Antibiotics](https://mattia-ficarelli.github.io/antibiotic_cost/) [[GitHub](https://github.com/mattia-ficarelli/antibiotic_cost)]
 - [Number of Patients Registered at GP Practices in London](https://mattia-ficarelli.github.io/gp_mapping/) [[GitHub](https://github.com/mattia-ficarelli/gp_mapping)]


Data Sources:
 - Please note that the dspt data has been omitted from this repository
 - Download the latest snapshot of the Data Security and Protection Toolkit from data from the [DSPT Portal](https://www.dsptoolkit.nhs.uk/OrganisationSearch)
 - Auxillary data (etr and ect) to filter for active trusts can be found on the NHS Digital website [Other NHS organisations](https://digital.nhs.uk/services/organisation-data-service/export-data-files/csv-downloads/other-nhs-organisations)
 - The lookup data and mapping from ONS codes to ODS codes can be found on the Open Geography Portal here [Sub ICB - ICB - Region Mapping Table]()https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=-created&tags=all(LUP_SICBL_ICB_NHSER)
 - The shapefiles used for plotting the leaflet maps can also be found on the Open Geography Portal here [Open Geography Portal Health Boundaries](https://geoportal.statistics.gov.uk/search?collection=Dataset&sort=-created&tags=all(LUP_SICBL_ICB_NHSER))

Python Version and Packages:
 - To run the python scripts please use Python v3.9.18
 - Install the relevant packages defined in requirements.txt using pip by running 'pip install -r requirements.txt' 

 Data Pre-processing:
 - Run the 'dspt_data_cuts_curation_22_23.ipynb' notebook to join with auxilary data and create separate cuts of the DSPT data for each organisation
 - Run 'main_jan23.R' to create the summary table and bar charts for CSUs, Trusts and ICBs
 - Run '02-geomapping-ICS_22_23.R' to generate the leaflet maps using the curated data
