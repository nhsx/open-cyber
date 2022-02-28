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
Summary statistics from the DSPT 2020/21 toolkit are shown. The latest status information was downloaded on the <b>10th October 2021</b>.

Note on method: organisations open 31st March 2021 are considered. However, if the organisation does not have a 20/21 DSPT status, it has been allowed to inherit one from its 21/22 successor organisation (if the successor submitted a 2020/21 return). The same applies for the subsections below.

<!---
# <iframe src="https://nhsx.github.io/open-cyber/myplotly.html" height="600px" width="100%" style="border:none;"></iframe>
-->

{% include crosstab_summary_FY2021.html %}

<iframe src="barchart_summary_FY2021_w.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">


## CCGs - Individual Compliance
<br>
The compliance of individual CCGs is mapped below.

<iframe src="choropleth_DSPT_CCG.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Trusts - DSPT Compliance over editions
<br>
Below we consider the overall evolution of DSPT status across editions, for trusts open end-financial year 2020/21.
Note that below, for trusts resulting from mergers, we do not reflect the status of their predecessors, unless their identifying organisational code has been kept.

Despite showing editions side-by-side, it is important to note that the standards - and the level of evidence required to meet those standards - tend to get raised year-on-year. This means that a similar status in two editions is not directly comparable.

<iframe src="SankeyTrustDSPT.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

Template for end-to-end open source analytics: [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).

Analytics leverages open source data and R libraries such as [leaflet](https://cran.r-project.org/web/packages/leaflet/index.html) for interactive maps, [plotly](https://plotly.com/r/) for other interactive visualisations and [summarytools](https://cran.r-project.org/web/packages/summarytools/vignettes/introduction.html) for descriptive statistics.
