# Data Viz

Files used for the November 21, 2024 presentation to the [Data Innovation and Coordinating Hub](https://maternalhealthhub.jhu.edu/johns-hopkins-university#:~:text=The%20Hub%20is%20a%20multidisciplinary,Medicine%20at%20Johns%20Hopkins%20University) for the [https://www.nih.gov/news-events/news-releases/nih-establishes-maternal-health-research-centers-excellence](NIH Maternal Health Research Centers of Excellence).

## Talk Recording

A recording of this presentation was published on YouTube. [Watch Here](https://www.youtube.com/watch?v=Flt7Cqbeyyw).

## Contents

The below files are included in this repository:

```
mhviz/
├── data/                              # Data used in the project
├── images/                            # Images used in the project
├── causal-model.Rmd                   # Generates the causal model DAGs
├── colors.R                           # Color definitions from TailwindCSS
├── correlations-visualized.Rmd        # Generates the correlation visualizations
├── data-simulations.Rmd               # Generates the simulated data
├── design.Rmd                         # Generates the design illustrations
├── misleading-viz.Rmd                 # Generates the misleading visualizations
├── sim-viz.Rmd                        # Generates visualizations of the simulated data
└── utils.R                            # Utility functions
```

These files are used to generate the visualizations and data used in the project. The data is simulated and should not be used for external analyses.

## Data

State population data came from the 2020 US Census. They are modified by hand for our purposes and should not be used for external analyses.

## Software Used

### Statistical Software

- [R](https://www.r-project.org/)
- [RStudio](https://www.rstudio.com/)
- [Tidyverse](https://www.tidyverse.org/)

All examples are made in R using `tidyverse` tools. You can render the `.Rmd` files in RStudio.

### Packages

The following R packages are used in this project. Copy below to install them.

```r

install.packages(c(
  "broom",
  "broom.mixed",
  "causaldata",
  "cowplot",
  "dagitty",
  "dplyr",
  "forcats",
  "ggdag",
  "ggplot2",
  "ggrepel",
  "ggtext",
  "janitor",
  "kableExtra",
  "lme4",
  "maps",
  "purrr",
  "readr",
  "sf",
  "showtext",
  "stringr",
  "tidyr",
  "usmap"
))

```

## References

- Massive credit goes to [Kieran Healy](https://kieranhealy.org/) for his [Data Visualization: A Practical Introduction](https://socviz.co/) book. The code in this repository is heavily influenced by his work.
- The data correlation pattern code is adapted from [Jan Vanhove](https://janhove.github.io/posts/2016-11-21-what-correlations-look-like/).
