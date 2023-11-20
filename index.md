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

The data displayed in this page will show all organisations open as of 16th of November 2023. Any organisations which have merged and do not have a DSPT status will inherit the DSPT status of the merged organisation. Any organisations which have failed to submit a 22/23 DSPT Status will be considered as 'Not Published'.

The following information displays data taken from a specific snapshot of the DSPT data. The latest status information was downloaded on <b>16th November 2023</b>.

You can find the previous iteration of this page showing the 21/22 DSPT data at <a href="https://nhsx.github.io/open-cyber/index2">21/22 Open Cyber Page</a>

For more information on the Data Security and Protection Toolkit, please visit the <a href="https://www.dsptoolkit.nhs.uk/">DSPT portal</a>.

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Notes on Methodology

<b>Composite Metrics for ICB</b>

The DSPT status for Integrated Care Boards (ICB) and Trusts are coded (scored) as follows:

<li>Status Exceeded = 3</li>
<li>Status Met = 1</li>
<li>Approaching Standards = -1</li>
<li>Not Published/Not Met = -3</li>
<br>
Trust scores within an ICB are weighted as a simple average. 50% of the Trusts average score and 50% of the ICB score is used to calculate the total score for each ICB.

The final scaling for each of the summary metrics displayed will have an upper bound of 3 and a lower bound of -3.


## Summary of DSPT Compliance (2022/2023 edition).
<br>
Summary statistics from the DSPT 2021/22 toolkit are shown.

<iframe src="./outputs/data_DSPTmetric2023-11-20.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

<iframe src="./outputs/barchart_summary_FY2023_2023-11-20.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## ICBs and Trusts - Individual Compliance
<br>
The compliance of individual ICBs and Trusts are mapped below, with Region boundaries. Toggle the boxes on the top right-hand side to add layers.


<iframe src="icb_trusts_map_16_11_23.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


<br>

## ICBs and Trusts - Composite Score
The compliance of individual ICBs, made up of a composite score of 50% of the ICB score itself which and 50% of a simple average of Trust scores contained in the ICB. Region boundaries are displayed in blue.


<iframe src="icb_composite_map_16_11_23.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

<br>
Template for end-to-end open source analytics: [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).

Analytics leverages open source data and R libraries such as [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) for interactive maps, [plotly](https://plotly.com/r/) for other interactive visualisations and [summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html) for descriptive statistics.

