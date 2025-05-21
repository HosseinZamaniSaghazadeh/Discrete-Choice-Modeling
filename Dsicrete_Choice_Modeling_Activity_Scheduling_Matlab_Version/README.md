# Dynamic Discrete Choice Modeling of Activity-Scheduling (MVP Framework) – MATLAB

This repository implements an **initial MATLAB version** of an **activity-scheduling problem** using a **Markovian Value Process (MVP) framework** solved via **Dynamic Programming (DP)**. The approach is based on the research paper:

**"A Dynamic Discrete Choice Activity-Based Travel Demand Model"**  
*Oskar Blom Västberg, Anders Karlström, Daniel Jonsson, Marcus Sundberg*

---

## Project Overview

- Models individual daily activity decisions using a **discrete choice framework** over a finite time horizon.
- Uses **Dynamic Programming** to solve for the optimal policy in the **MVP** setting.
- Decision-making accounts for:
  - Activity utility (e.g., home, work, stay, travel)
  - Temporal and spatial constraints
  - Travel time and cost matrices
  - Individual-specific preferences

---

## Core Components

| Module | Description |
|--------|-------------|
| `Main_Code.m` | Main script to initialize data and run the DP model |
| `Observed_Activity_Dataset.csv` | Input dataset with observed behavior (used for validation/comparison) |
| `Travel_Time_and_Cost_Matrix.csv` | Travel times and generalized costs between zones |
| `get_C_general.m`, `get_C_individual.m` | Cost utility computation (generalized and personalized) |
| `get_C_stay.m`, `get_C_travel_general.m`, `get_C_travel_individual.m` | Utility functions for staying or traveling |
| `u_act.m`, `u_start.m`, `u_travel.m`, `u_work_constraint.m`, etc. | Component utility functions for activity preferences |
| `get_P_n.m`, `get_M_n.m` | State transition and probability matrices |
| `TC_lookup.m`, `TT_lookup.m` | Functions to retrieve travel time and cost |

---

