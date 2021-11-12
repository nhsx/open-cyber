<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

# Open Statistics - Cyber Security
<!---
{% include update.html %}
-->
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

The Data Security and Protection Toolkit is an online self-assessment tool that allows organisations to measure their performance against the National Data Guardian’s 10 data security standards.

All organisations that have access to NHS patient data and systems must use this toolkit to provide assurance that they are practising good data security and that personal information is handled correctly. This includes trusts, providers and CSUs.

For more information on the Data Security and Protection Toolkit, please visit the <a href="https://www.dsptoolkit.nhs.uk/">DSPT portal</a>.

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Summary of DSPT Compliance (2020/2021 edition)

Summary statistics from the DSPT 2020/21 toolkit are shown. Latest status was downloaded on the 10th October 2021.

Note on method: organisations open 31st March 2021 are considered. However, if the organisation does not have a 20/21 DSPT status, it has been allowed to inherit one from its 21/22 successor organisation (if the successor submitted a 2020/21 return.)

<!---
# <iframe src="https://nhsx.github.io/open-cyber/myplotly.html" height="600px" width="100%" style="border:none;"></iframe>
-->

{% include crosstab_summary_FY2021.html %}

<iframe src="barchart_summary_FY2021_w.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Trusts - DSPT Compliance over editions

Below we consider the overall evolution of DSPT status across editions, for trusts open end-financial year 21/22.
Note that any pre-Trust merger statuses have not been reflected with respect to their respective post-merger organisations.
Despite showing editions side-by-side, it is important to note that the Standards - and the level of evidence withing the Standards - tends to get raised year-on-year. This means that a similar status in two editions is not directly comparable.

<iframe src="SankeyTrustDSPT.html" height="600px" width="100%" style="border:none;"></iframe>

Template for end-to-end open source analytics: python, [plotly](https://plotly.com/python/), [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).
