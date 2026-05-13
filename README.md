# Aircraft Maintenance Cost Forecasting

**Comparing SARIMA, ARIMAX, and LSTM-RNN for quarterly maintenance cost prediction across Boeing and Airbus fleets**

> *Northern Arizona University — MAT/STA 477 | May 2026*
> *Sakina Lord · James Hope-Meek · Megan Ruza Dsouza*

---

## Overview

Airline operators managing large, aging fleets need accurate maintenance cost forecasts to manage budgets and supply chains. This project models **quarterly maintenance cost per air hour** for Boeing and Airbus aircraft from 2004 Q1 through 2025 Q3, comparing three forecasting approaches of increasing complexity:

| Model | Type | Key Strength |
|-------|------|-------------|
| SARIMA | Statistical baseline | Interpretable, minimal assumptions |
| ARIMAX | Statistical + exogenous | Adds real-world predictors + COVID flags |
| LSTM-RNN | Deep learning | Captures nonlinear, long-range dependencies |

---

## Data

- **Source:** [Bureau of Transportation Statistics](https://www.transtats.bts.gov/) — Form P-5.2 (Air Carrier Financial Data)
- **Coverage:** 2004 Q1 – 2025 Q3 | 87 quarters × 2 manufacturers = **174 observations**
- **Response variable:** `MAINT_PER_AIR_HR` — total direct maintenance cost divided by total air hours flown
- **Train/test split:** 80 quarters training, 7 quarters held out for evaluation

**Engineered features include:**
- `UTILISATION` — air hours per assigned day (fleet intensity proxy)
- `TOT_FLY_OPS` — total flight operations (demand proxy)
- `COVID shock/recovery flags` — binary indicators for 2020 Q1–Q4 disruption periods

---

## Methods

### Preprocessing
All series were **log-transformed** to stabilize COVID-era variance spikes, then **doubly differenced** (lag-1 + seasonal lag-4) to achieve stationarity for the SARIMA and ARIMAX models. The RNN was trained on the raw log-transformed series to allow the model to learn trend structure directly.

### SARIMA
Selected `SARIMA(0,1,1)×(0,1,1)₄` for both manufacturers via ACF/PACF analysis and AIC/BIC/AICC comparison. Residuals were uncorrelated (Ljung-Box) but non-normal due to COVID outliers.

### ARIMAX
Extended the SARIMA framework with exogenous predictors (flight operations, utilization, carrier count) and **data-driven COVID anomaly flags** identified via z-score thresholding (|z| > 2.5). Final models:
- **Boeing:** `ARIMAX(1,1,1)×(0,1,1)₄`
- **Airbus:** `ARIMAX(0,1,1)×(0,1,1)₄`

### LSTM-RNN
Built in **PyTorch** with a single LSTM layer, dropout regularization, and a fully connected output head. Hyperparameters tuned across 192 configurations via validation loss:

```
lookback: 16 | hidden units: 64 | learning rate: 0.005 | dropout: 0.2 | batch size: 16
```

SHAP values were computed to assess variable importance; `ENGINE_LABOR` and manufacturer identity were the top predictors.

---

## Results

Test set performance (log-scale RMSE and MAE, n = 7 quarters):

| Manufacturer | Model | RMSE | MAE |
|---|---|---|---|
| Boeing | SARIMA | 0.0794 | 0.0656 |
| Boeing | ARIMAX | 0.0620 | 0.0592 |
| **Boeing** | **RNN** | **0.0362** | **0.0244** |
| Airbus | SARIMA | 0.0585 | 0.0452 |
| **Airbus** | **ARIMAX** | **0.0378** | **0.0297** |
| Airbus | RNN | 0.0847 | 0.0781 |

**Key finding:** Model performance was manufacturer-dependent.
- For **Boeing**, complexity paid off — the RNN achieved RMSE 2.15× lower than the next best model.
- For **Airbus**, the ARIMAX outperformed all others, likely because the shared RNN overfit to Boeing's patterns.
- **Across both manufacturers**, ARIMAX had the best average RMSE (0.0499), edging out the RNN (0.0561) and SARIMA (0.0690).

---

## Limitations

- COVID quarters violate normality assumptions in all models; stated 95% prediction intervals should be interpreted with caution
- Reporting corrections (negative costs/hours) were removed, which may introduce upward bias
- A single shared RNN was trained on both manufacturers; separate models may improve Airbus performance
- Seasonal MA coefficient Θ̂₁ = −1 in ARIMAX suggests possible over-differencing

---

## Tech Stack

![R](https://img.shields.io/badge/R-276DC3?style=flat&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/Python-3776AB?style=flat&logo=python&logoColor=white)
![PyTorch](https://img.shields.io/badge/PyTorch-EE4C2C?style=flat&logo=pytorch&logoColor=white)
![scikit-learn](https://img.shields.io/badge/scikit--learn-F7931E?style=flat&logo=scikitlearn&logoColor=white)

**R packages:** `tidyverse`, `ggplot2`, `forecast`, `car`, `patchwork`  
**Python libraries:** `PyTorch`, `scikit-learn`, `pandas`, `numpy`, `shap`, `matplotlib`

---

## References

- Box, G.E.P. & Tiao, G.C. (1975). Intervention analysis with applications to economic and environmental problems. *JASA*, 70(349):70–79.
- Bureau of Transportation Statistics (2025). [Air Carrier Financial: Schedule P-5.2](https://www.transtats.bts.gov/Fields.asp?gnoyr_VQ=FMK)

   _Claude (Sonnet 4.6) was used to aid the writing of this README_
