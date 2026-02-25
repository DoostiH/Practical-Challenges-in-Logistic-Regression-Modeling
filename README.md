# Practical Challenges in Logistic Regression Modeling

This repository contains the R code and datasets accompanying the book:

> **Practical Challenges in Logistic Regression Modeling**
> Hassan Doosti
> Taylor & Francis, 2026

---

## Repository Structure

Practical-Challenges-in-Logistic-Regression-Modeling/
├── README.md
├── data/
│   └── (datasets used in the book)
└── R/
    ├── Chapter01_Fundamentals.R
    ├── Chapter02_Separation.R
    ├── Chapter03_RareEvents.R
    ├── Chapter04_Overdispersion.R
    ├── Chapter05_VariableSelection.R
    ├── Chapter06_Multicollinearity.R
    ├── Chapter07_Nonlinearity.R
    ├── Chapter08_Interactions.R
    ├── Chapter09_Diagnostics.R
    ├── Chapter10_Validation.R
    ├── Chapter11_Longitudinal.R
    ├── Chapter12_MultinomialOrdinal.R
    ├── Chapter13_MissingData.R
    ├── Chapter14_SurveyData.R
    ├── Chapter15_BayesianCausal.R
    └── Chapter16_Reporting.R

---

## R Version and Packages

All code was written and tested using R version 4.3.3. The R packages
required for each chapter are listed at the beginning of the respective
R script and in the corresponding book chapter.

While every effort has been made to ensure the code runs correctly at the
time of writing, R packages are regularly updated and their behaviour,
function names, or default arguments may change over time. If you encounter
any issues, please check the package documentation for your installed version
and consult the package changelog for any breaking changes.

---

## How to Use

1. Clone or download the repository:

git clone https://github.com/DoostiH/Practical-Challenges-in-Logistic-Regression-Modeling.git

2. Open the R script corresponding to the chapter you are reading.

3. Install any required packages listed at the top of the script if you
have not already done so. For example:

install.packages(c("package1", "package2"))

4. Run the code in RStudio or any R environment of your choice.

---

## Datasets

The datasets used in this book are available in the data/ folder.
Where data are sourced from publicly available repositories or publications,
appropriate citations and links are provided within the relevant R scripts
and book chapters.

---

## Suggested Reading Paths

The book is designed so that most chapters can be read relatively independently
after Chapter 1. The following reading paths are suggested for readers with
specific interests:

- Clinical and epidemiological researchers: Ch 1 -> Ch 3 -> Ch 13 -> Ch 15 -> Ch 16
- Machine learning practitioners: Ch 1 -> Ch 5 -> Ch 6 -> Ch 7 -> Ch 10
- Survey researchers: Ch 1 -> Ch 14 -> Ch 13 -> Ch 9
- Researchers facing convergence problems: Ch 1 -> Ch 2 -> Ch 3 -> Ch 6
- Those developing prediction models: Ch 1 -> Ch 5 -> Ch 9 -> Ch 10 -> Ch 16
- Longitudinal data analysts: Ch 1 -> Ch 11 -> Ch 13 -> Ch 9
- Causal inference researchers: Ch 1 -> Ch 15 -> Ch 13 -> Ch 16

---

## Citation

If you use the code or datasets from this repository in your research,
please cite the book as:

Doosti, H. (2026). Practical Challenges in Logistic Regression Modeling.
Taylor & Francis.

---

## Feedback and Issues

If you encounter any problems with the code or have suggestions for improvement,
please open an issue on this repository or contact the author directly.
Your feedback is welcome and will help improve future editions of the book.

---

## License

The code in this repository is made available for educational and research
purposes. Please refer to the book for full terms of use.
