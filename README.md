Disposition Dashboard — Overview
The Disposition Dashboard is an interactive Shiny application designed to review and visualize subject disposition data from two clinical studies (Berg and Juni). It provides a unified interface for exploring study progress, discontinuation patterns, and subject listings using dynamic filters and interactive charts.

📌 Key Features
1. Dual‑Study Support
Loads two Excel sheets (Berg and Juni) and switches logic automatically based on study selection.

Each study uses its own disposition variables (e.g., COMPSTDI vs COMPSTDO).

2. Dynamic Filtering Logic
Study selector drives all downstream UI and data.

Phase selector appears only for Berg.

Cohort selector appears only for Berg → Induction.

Filters applied reactively:

Safety flags (SAFFIFL, SAFFMFL, OLEFL)

Cohort selection

Phase selection

3. Interactive Visualizations (Plotly)
Study Disposition Pie Chart

Study Discontinuation Reasons Bar Chart

Drug Disposition Pie Chart

Drug Discontinuation Reasons Bar Chart

All charts use a custom color theme:

Cadet Blue (#5F9EA0)

Tomato (#FF6347)

4. Courier‑Styled Dashboard
Global font override applies Courier New across the entire UI for a consistent, monospaced clinical‑style appearance.

5. Summary Table
Displays aggregated counts of disposition categories based on selected study.

6. Subject Listing
Full reactive dataset shown in a paginated DT table.

Automatically updates with study, phase, and cohort filters.

📂 Technical Highlights
Built with shinydashboard, plotly, DT, and dplyr.

Uses reactive data pipelines to ensure all charts and tables update instantly.

Clean separation of UI and server logic.

Custom color palette for clinical readability.

Conditional UI elements based on study logic.

