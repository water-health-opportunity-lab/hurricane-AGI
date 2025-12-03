**Code repository for the REACH Center pilot project -- Aim 3**

*Examine health outcomes associated with private well contamination following hurricanes*

This project develops reproducible, data-driven workflows to analyze the effects of hurricanes on private well contamination and associated health risks. We use public datasets, electronic health records, and geospatial and econometric methods in R.

------------------------------------------------------------------------

## 📁 Project structure

``` text
hurricane-AGI/
├── hurricane-AGI.Rproj    # RStudio project file
├── README.md             # Project documentation
├── renv.lock             # Records the exact versions of R and R packages to ensure reproducibility
├── renv/                 # Directory containing renv infrastructure for the project
├── data/                 # Directory containing input and output data, do not commit to git
└── script/               # Main analysis scripts
```

## 🚀 Getting started with renv

This project uses renv to manage R package dependencies, ensuring reproducibility. Here's how to get started:

Clone the Repository:

``` text
git clone https://github.com/water-health-opportunity-lab/hurricane-AGI.git # Replace with the actual URL
cd hurricane-AGI
```

Open the Project in RStudio: Open the hurricane-AGI.Rproj file in RStudio. This will automatically activate the renv environment. Restore Package Library: Run the following command in the R console to install the correct package versions:

``` text
renv::restore()
```

This will install the exact package versions specified in the renv.lock file. Confirm Confirm that the packages have been installed by running:

``` text
renv::status()
```

This will show you the status of the packages in your project. Work in the Project: You're now ready to work in the project! Any packages you install will be managed by renv.

## 🔧 Requirements

This project uses R. To begin, open hurricane-AGI.Rproj in RStudio and run scripts in the script/ folder in order.

## 🔐 API Keys

Store your API keys (e.g., Census API) in an .Renviron file in your home directory:

``` text
CENSUS_API_KEY=your_key_here
```

Do not commit your `.Renviron` file to Git.

## 🚧 Branching Guidelines

This project uses the Git Flow workflow. Please follow these conventions when contributing:

Start from the develop branch:

``` text
git checkout develop
git pull origin develop
```

Create a new feature branch using the feat/ prefix:

``` text
git checkout -b feat/short-description
```

Push your feature branch to GitHub:

``` text
git push -u origin feat/short-description
```

Submit a pull request from your feature branch into develop. Tag @jahredwithanh for review.

## 📌 Project Status

This project is in active development under the REACH Center pilot grant.
