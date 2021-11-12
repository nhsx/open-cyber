<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>

# Open Statistics - Cyber Security

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

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

## Summary of DSPT Compliance (2020/2021 edition)

Summary statistics from the DSPT 2020/21 toolkit are shown. Latest status was downloaded

Note on method: organisations open 31st March 2021 are considered. However, if the organisation does not have a 20/21 DSPT status, it is allowed to inherit one from its successor organisation (if the successor submitted a 2020/21 return.)

<!---
# <iframe src="https://nhsx.github.io/open-cyber/myplotly.html" height="600px" width="100%" style="border:none;"></iframe>
-->

{% include crosstab_summary_FY2021.html %}

<iframe src="barchart_summary_FY2021_w.html" height="600px" width="100%" style="border:none;"></iframe>

<hr class="nhsuk-u-margin-top-0 nhsuk-u-margin-bottom-6">

Template for end-to-end open source analytics: python, [plotly](https://plotly.com/python/), [github.io](https://pages.github.com/), and [github actions](https://github.com/features/actions).
