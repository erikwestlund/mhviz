# Data Viz

Files used for the November 21, 2024 presentation to the Maternal Health Working Group at Johns Hopkins University.

## Contents

The below files are included in this repository:

```
project/
├── data/                              # Data used in the project
├── images/                            # Images used in the project
├── causal-model.Rmd                   # Generates the causal model DAGs
|── colors.R                           # Color definitions from TailwindCSS
├── correlations-visualized.Rmd        # Generates the correlation visualizations
├── data-simulations.Rmd               # Generates the simulated data
├── design.Rmd                         # Generates the design illustrations
|── misleading-viz.Rmd                 # Generates the misleading visualizations
└── sim-viz.Rmd                        # Generates visualizations of the simulated data
|── utils.R                            # Utility functions
```

These files are used to generate the visualizations and data used in the project. The data is simulated and should not be used for external analyses.

## Data

State population data came from the 2020 US Census. They are modified by hand for our purposes and should not be used for external analyses.

## References

Massive credit goes to [Kieran Healy](https://kieranhealy.org/) for his [Data Visualization: A Practical Introduction](https://socviz.co/) book. The code in this repository is heavily influenced by his work.