# Crime Analysis in India

Data analysis project focused on preprocessing Indian crime datasets, identifying trends, and communicating insights through visual analytics using R.

## Tech Stack

| Layer | Technology |
|-------|------------|
| **Language** | R |
| **Analysis** | Data Cleaning, Statistical Analysis |
| **Visualization** | ggplot2, shiny |
| **IDE** | RStudio |

## Project Structure

```
Crime_analysis_in_india/
├── app.R                   # Shiny application entry point
├── .RData                  # Saved R workspace
├── .Rhistory               # R command history
├── RProject.Rproj          # RStudio project file
└── README.md
```

## Features

- **Data Preprocessing**: Clean and transform raw crime data from NCRB (National Crime Records Bureau)
- **Statistical Analysis**: Descriptive statistics, trend analysis, correlation studies
- **Interactive Visualization**: Shiny-based dashboard for exploring crime trends
- **Geographic Analysis**: State-wise and district-wise crime patterns
- **Temporal Analysis**: Year-over-year trends, seasonal patterns

## Data Sources

- **Primary**: National Crime Records Bureau (NCRB) annual reports
- **Scope**: Indian states and union territories
- **Period**: Multi-year historical data

## Setup Instructions

### Prerequisites
- R 4.0+
- RStudio (recommended)
- Required R packages (see below)

### Installation

```bash
# Clone repository
git clone https://github.com/abhxxy45/Crime_analysis_in_india.git
cd Crime_analysis_in_india

# Open in RStudio
open RProject.Rproj

# Install required packages
install.packages(c("shiny", "ggplot2", "dplyr", "readr", "tidyr", "DT", "plotly"))

# Run Shiny app
shiny::runApp()
```

### Required R Packages

| Package | Purpose |
|---------|---------|
| `shiny` | Interactive web applications |
| `ggplot2` | Data visualization |
| `dplyr` | Data manipulation |
| `readr` | Fast CSV reading |
| `tidyr` | Data tidying |
| `DT` | Interactive tables |
| `plotly` | Interactive plots |

## Analysis Workflow

1. **Data Import**: Load NCRB CSV files
2. **Cleaning**: Handle missing values, standardize categories
3. **Transformation**: Aggregate by state, year, crime type
4. **Analysis**: Statistical summaries, trend detection
5. **Visualization**: Interactive charts and maps
6. **Reporting**: Export findings

## Key Visualizations

- State-wise crime rate heatmap
- Yearly trend lines by crime category
- Top 10 states for major crime types
- Correlation matrix of crime variables
- Interactive Shiny dashboard

## Screenshots

*To be added*

## License

MIT License — see [LICENSE](LICENSE) for details.

---

**Repository**: https://github.com/abhxxy45/Crime_analysis_in_india