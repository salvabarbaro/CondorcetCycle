# CondorcetCycle

**Replication Data: On the Prevalence of Condorcet's Paradox**  
Authors: Salvatore Barbaro and Anna Kurella

---

## 📂 Overview

This repository contains replication scripts for the study on the prevalence of Condorcet's Paradox. The analysis uses data from the Comparative Study of Electoral Systems (CSES). The scripts are written in R.

> **Note**: You will need to download the CSES data directly from the CSES project website. The source and DOI are provided in the paper's reference section and below (Section Data Accessibility).

---

## 📊 Data Accessibility

The paper uses data from the **CSES project**, which is subject to redistribution restrictions but can be freely downloaded after registration.  
- **Data DOI**: [10.7804/cses.imd.2020-12-08](https://doi.org/10.7804/cses.imd.2020-12-08)

---

## 🗂️ Dataset and Script List

### 1. **Main Replication File**
   - `Main.R`: Main script.

### 2. **Bootstrap and Random Noise Replication Files**
   - `bootstrap.R`: Performs 10,000 bootstrap replications on each party election.
   - `bootstrapCand.R`: Performs 10,000 bootstrap replications on each candidate election.
   - `Noise_HPC.R`: Performs 10,000 random noise replications on each election. Noise interval: [-1.1, 1.1].
   - `MainDPLYR.R`: Creates a list with the election outcomes that is required to run the bootstrap / noise analyses.

### 3. **Cabinet Analysis**
   - `cabinet.R`: Assesses how often the Condorcet winner (or loser) belongs to the cabinet after the respective election.

---

## 🖥️ Software Requirements

- **R Version**: 4.4.1 (Race for your life)
- **Required R Libraries**:
  - `vote`
  - `parallel`
  - `dplyr`
  - `data.table`
  - `stringr`
  - `tidyr`
  - `boot`

The scripts have been tested on both Linux-Debian and Windows systems. The scripts for bootstrap / noise replications we have run on the MOGON (HPC at the Johannes Gutenberg University (hpc.uni-mainz.de). Be aware that the parallel functions do not work on non-Linux-OS. Adjust the scripts accordingly.  

---

## ⚙️ Controlled Randomness

All bootstrap and Bayesian (Monte Carlo) replications use a fixed seed value for reproducibility:  
**Seed Value**: `55234` (the authors' zip code).

---

## ⏱️ Approximate Runtime

- **Main Scripts**: ~6 minutes on a standard (2024) desktop machine.  
- **Bootstrap**: ~8 hours each on a high-performance computer with 40 CPU cores.
- **Noise**: ~ 12 hours on a high-performance computer with 40 CPU cores. 

> If running the scripts on a non-UNIX or non-Linux system, consider revising the parallelization logic for optimal performance.

---

## 📜 Code Description

Detailed descriptions of the code functionality are included as comments within each script.

---

## 🚀 Instructions for Replicators

1. Start R.
2. Run `Main.R`.

For advanced replication (e.g., bootstrap and Random Noise), execute the respective scripts as per the instructions in the comments.

---

## 🙏 Acknowledgments

We thank the Comparative Study of Electoral Systems (CSES) project for providing the data. This work relies heavily on their comprehensive electoral data collection. We are grateful to the Mogon / HPC group for granting processor time on MOGON.

---

## 📖 License

This project is licensed under MIT. 

---


