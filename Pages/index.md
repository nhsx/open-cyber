,<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

# Open Statistics - Cyber Security
<!---
{% include update.html %}

<div class="nhsuk-warning-callout">
  <h3 class="nhsuk-warning-callout__label">
    Important<span class="nhsuk-u-visually-hidden">:</span>
  </h3>
  <p>This project is currently in development. For more information please contact <a
                class="nhsuk-footer__list-item-link"
                href="{{ site.github.owner_url }}"
                >{{ site.github.owner_name }}</a>
   </p>
</div>
-->

<div class="nhsuk-warning-callout">
  <h3 class="nhsuk-warning-callout__label">
    Important<span class="nhsuk-u-visually-hidden">:</span>
  </h3>
  <p>This project is currently in development. An overview of methodology and caveats are given below. For more information please contact <a href="mailto:TDAUgroup@england.nhs.uk">TDAUgroup@england.nhs.uk</a>. <br>Opinions expressed in this page are not representative of the views of NHS England and any content here should not be regarded as official output in any form. For more information about the NHS England Transformation Directorate please visit our <a href="https://transform.england.nhs.uk/">official website</a>.
   </p>
</div>

The <b>Data Security and Protection Toolkit</b> is an online self-assessment tool that allows organisations to measure their performance against the National Data Guardian’s 10 data security standards.

All organisations that have access to NHS patient data and systems must use this toolkit to provide assurance that they are practising good data security and that personal information is handled correctly. This includes trusts, commissioners and CSUs.

The data displayed in this page will show all organisations open as of 9th of September 2022. Any organisations which have merged and do not have a DSPT status will inherit the DSPT status of the merged organisation.

The following information displays data taken from a specific snapshot of the DSPT data. The latest status information was downloaded on the <b>9th September 2022</b>.

You can find the previous iteration of this page showing the 20/21 DSPT data at <a href="https://nhsx.github.io/open-cyber/index2">20/21 Open Cyber Page</a>

For more information on the Data Security and Protection Toolkit, please visit the <a href="https://www.dsptoolkit.nhs.uk/">DSPT portal</a>.

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Notes on Methodology

<b>Composite Metrics for ICS</b>

The DSPT status for Clinical Commissioning Groups (CCGs) and Trusts are coded (scored) as follows to create the proxy for Integrated Care System (ICS) summary metrics:

<li>Status Exceeded = 3</li>
<li>Status Met = 1</li>
<li>Approaching Standards = -1</li>
<li>Not Published/Not Met = -3</li>
<br>
CCG scores within an ICS are then weighted based on the resident population.

Trust scores within an ICS are weighted as a simple average.

Finally, for each ICS, the CCG and Trust scores are weighted equally to arrive at the ICS composite score.

The final scaling for each of the summary metrics displayed will have an upper bound of 3 and a lower bound of -3.

<!---
<b>Emergency Preparedness, Resilience and Response (EPRR) Score</b>

Organisations are ranked in a 4 tier system, to measure their impact of risk. This is used to calculate one of our summary metrics below. The Tier ranks are mapped as follows:

<li>Tier 1 - Major Trauma Centres/ Ambulance Trusts/ NHS Blood & Transplant = 4</li>
<li>Tier 2 - Trauma Units/ Specialist Hospitals = 3</li>
<li>Tier 3 - Geographically remote / High secure Mental Health = 2</li>
<li>Tier 4 - Community Hospitals = 1</li>
<br>
<b>Successor Organisations</b>
<br>
Organisations open 31st March 2021 are considered (the end of 20/21, the financial year of the last DSPT edition that closed).

Organisations without a published DSPT status as a result of a merger, inherit their DSPT status from it's successor organisation if published. Otherwise, the DSPT status will be mapped as 'Not Published'.
-->
## Summary of DSPT Compliance (2021/2022 edition).
<br>
Summary statistics from the DSPT 2021/22 toolkit are shown.

{% include cross_table_summary_21_222022-10-05.html %}

<iframe src="dspt_summary_chart_21_22.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


## CCGs and Trusts - Individual Compliance
<br>
The compliance of individual CCGs and Trusts are mapped below, with ICS boundaries. Toggle the boxes on the top right-hand side to add layers.


<iframe src="ccg_trusts_map_03_10_22.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


## CCGs and Proportions of Trusts Compliance - Colour Coded Population
<br>
The proportions of trust compliance within each ICS with boundaries (in black) are shown. The ICSs are colored to represent the patient population level. The darker the shade of blue the higher the patient population level. 

<iframe src="ICS_pie_chart_03_10_22.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## ICSs Composite Compliance - CCG and Trust Score Average Weighted For Population
<br>
The compliance of individual ICSs, made up of a composite score of 50% CCG scores which have been weighted for population and 50% of a simple average of Trust scores. Region boundaries are displayed in blue.


<iframe src="ICS_composite_map_03_10_22.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

<br>
Template for end-to-end open source analytics: [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).

Analytics leverages open source data and R libraries such as [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) for interactive maps, [plotly](https://plotly.com/r/) for other interactive visualisations and [summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html) for descriptive statistics.

