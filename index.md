<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

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
  <p>This project is currently in development. For more information please contact <a href="mailto:martina.fonseca@nhsx.nhs.uk">martina.fonseca@nhsx.nhs.uk</a>. <br>This is an exploratory piece leveraging open source data and widget tooling in R - it has not received formal QA. <br>Opinions expressed in this page are not representative of the views of NHSX and any content here should not be regarded as official output in any form. For more information about NHSX please visit our <a href="https://www.nhsx.nhs.uk/">official website.</a>.
   </p>
</div>

The <b>Data Security and Protection Toolkit</b> is an online self-assessment tool that allows organisations to measure their performance against the National Data Guardian’s 10 data security standards.

All organisations that have access to NHS patient data and systems must use this toolkit to provide assurance that they are practising good data security and that personal information is handled correctly. This includes trusts, commissioners and CSUs.

For more information on the Data Security and Protection Toolkit, please visit the <a href="https://www.dsptoolkit.nhs.uk/">DSPT portal</a>.

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Summary of DSPT Compliance (2020/2021 edition).
<br>
Summary statistics from the DSPT 2020/21 toolkit are shown. The latest status information was downloaded on the <b>8th February 2022</b>.

Note on method: organisations open 31st March 2021 are considered. However, if the organisation does not have a 20/21 DSPT status, it has been allowed to inherit one from its 21/22 successor organisation (if the successor submitted a 2020/21 return). The same applies for the subsections below.


<iframe src="outputs/data_DSPTmetric2022-02-08.html" height="600px" width="100%" style="border:none;"></iframe>


<iframe src="outputs/barchart_summary_FY2021_2022-02-28.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


## CCGs and Trusts - Individual Compliance
<br>
The compliance of individual CCGs and Trusts are mapped below, with ICS boundaries.

<iframe src="chloropleth_DSPT_CCG_Trusts.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


## CCGs and Proportions of Trusts Compliance - Population
<br>
The compliance of individual CCGs and proportions of trusts compliance within each ICS with boundaries. The width of each pie chart indicates the population level within each ICS.

<iframe src=" choropleth_DSPT_pie_charts 2022-03-03 .html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

</br>

## CCGs and Proportions of Trusts Compliance - EPRR
<br>
The compliance of individual CCGs and proportions of trusts compliance within each ICS with boundaries. The width of each pie chart indicates the level of EPRR risk assigned as tiers (1-4) for each trust.

<iframe src=" chloropleth_DSPT_pie_charts_eprr 2022-03-03 .html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

</br>


## ICSs Composite Compliance - CCG and Trust Score Average Weighted For Population
<br>
The compliance of individual ICSs, made up of a composite score of 50% CCG scores in the ICS and 50% Trust scores, weighted for patient population size. Region boundaries are displayed in blue
<iframe src="chlorepleth_DSPT_Regions.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

</br>


Despite showing editions side-by-side, it is important to note that the standards - and the level of evidence required to meet those standards - tend to get raised year-on-year. This means that a similar status in two editions is not directly comparable.










Template for end-to-end open source analytics: [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).

Analytics leverages open source data and R libraries such as [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) for interactive maps, [plotly](https://plotly.com/r/) for other interactive visualisations and [summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html) for descriptive statistics.
