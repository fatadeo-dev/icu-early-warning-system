# icu-early-warning-system
ICU Early Warning System for Patient Deterioration

A predictive early-warning system designed to detect clinical deterioration (e.g., sepsis) in ICU patients using numerical time-series analysis and interpretable machine learning techniques.

Project Context
This project was developed as part of a postgraduate course in Health Informatics in collaboration with a student.

My Role:
Guided system design and modeling approach
Contributed to feature engineering and analytical framework
Supervised implementation and evaluation
Reviewed results and interpretability strategy

Problem Statement
Early detection of patient deterioration in ICU settings is difficult due to:

High-frequency physiological data
Noise and short-term variability
Subtle early warning patterns

This project addresses these challenges using signal processing and data-driven modeling.

Methodology
The system integrates multiple numerical and machine learning techniques:

Signal Processing
Savitzky–Golay filtering for noise reduction
Numerical differentiation to capture rate-of-change
Time-Frequency Analysis
Maximal Overlap Discrete Wavelet Transform (MODWT)
Extraction of high-frequency energy features
Feature Engineering & Modeling
Principal Component Analysis (PCA) for dimensionality reduction
Logistic-style risk scoring function
ROC curve and AUC evaluation using trapezoidal rule
Data
Synthetic physiological time-series data simulating septic progression

Note: Data is simulated and used for proof-of-concept validation only.

Key Contributions
Designed an interpretable early-warning system
Combined signal processing with predictive modeling
Enabled detection of evolving physiological instability
Demonstrated strong separation between stable and deteriorating states (simulated environment)

System Output
Risk score for patient deterioration
Feature-driven insights into physiological changes
Visualization of patient state trajectories

Evaluation
Model performance assessed using ROC analysis
AUC computed via numerical integration (trapezoidal rule)

Tech Stack
Python / R
NumPy, Pandas
Scikit-learn
Signal processing libraries
R Shiny 

Relevance

This project contributes to:
AI in healthcare
Time-series analysis for clinical systems
Early-warning systems in critical care
Interpretable machine learning

Disclaimer
This system is a research prototype and has not been validated on real clinical datasets. Future work will involve evaluation on datasets such as MIMIC-IV.
