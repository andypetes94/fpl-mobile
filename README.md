# 🧙‍♂️ FPL Data Vizard — Mobile Edition

**FPL Data Vizard (Mobile)** is an interactive, mobile-optimized Shiny app designed for Fantasy Premier League (FPL) managers who want **data-driven insights on the go**.
Built with [`shinyMobile`](https://rinterface.github.io/shinyMobile/), this app delivers a native iOS/Android-like experience, combining the analytical power of R with a sleek, responsive design.

---

## 📱 Features

### ⚽ Player Analysis

* Compare FPL players across multiple metrics (xG, xA, BPS, Points, etc.)
* Filter by position, value, minutes, and gameweeks
* Generate downloadable player comparison plots and performance tables

### 🏟️ Team Analysis

* Explore aggregated team statistics across the 25/26 FPL season
* Filter by team, position, and performance thresholds
* Visualize top performers within each squad

### 📊 Total Metrics

* Analyze total and per-90 metrics across all FPL players
* Identify top-performing players for any metric (Goals, Assists, BPS, etc.)
* Interactive sliders for dynamic filtering

---

## 🛠️ Tech Stack

* **Framework:** [Shiny](https://shiny.posit.co/)
* **Mobile UI:** [shinyMobile](https://rinterface.github.io/shinyMobile/)
* **Visualization:** `ggplot2`, `ggtext`, `gghighlight`, `shadowtext`, `ggh4x`, `ggborderline`
* **Data Handling:** `dplyr`, `tidyverse`, `tidytext`, `jsonlite`, `httr`
* **Tables:** `DT`
* **Styling & Fonts:** `extrafont`, `showtext`

---

## 📂 Project Structure

```
FPL-Mobile-App/
├── app.R                   # Main Shiny app file (UI + Server)
├── Mobile_Functions.R      # Helper functions for data processing
├── fpl_data_update.R       # Script to fetch and update FPL CSV datasets
├── 25_26/Players_History.csv  # Player performance dataset
├── 25_26/Players_Gameweek.csv # Player gameweek dataset
├── 25_26/General_Info.csv     # Player general info
├── 25_26/Fixtures.csv         # Fixtures dataset
├── www/
│   ├── Logo4.png
│   ├── Player_Analysis.png
│   ├── Team_Analysis.png
│   ├── Total_Metrics.png
│   └── custom.css        
└── README.md
```

---

## 🚀 Updating the Data

The app reads pre-processed CSV files from the `/25_26/` folder.
You can update the datasets to the latest season data using the new **`fpl_data_update.R`** script.

### Features of `fpl_data_update.R`

* Fetches **all players, fixtures, and team data** directly from the [FPL API](https://fantasy.premierleague.com/api/)
* Outputs the following CSVs in `/25_26/`:

  * `Players_History.csv`
  * `Players_Gameweek.csv`
  * `General_Info.csv`
  * `Fixtures.csv`
* Supports **command-line arguments**:

  ```bash
  Rscript fpl_data_update.R --output /path/to/25_26/
  ```

  If `--output` is not provided, files are saved in the default `/25_26/` folder.

---

## 🚀 Running the App

1. **Clone the repository:**

   ```bash
   git clone https://github.com/yourusername/FPL-Mobile-App.git
   cd FPL-Mobile-App
   ```

2. **Update datasets** (optional, recommended before first run):

   ```bash
   Rscript fpl_data_update.R
   ```

3. **Open the project in RStudio.**

4. **Install dependencies (only once):**

   ```r
   install.packages(c("shiny", "shinyMobile", "tidyverse", "DT", "jsonlite", 
                      "httr", "ggtext", "shadowtext", "ggh4x", "extrafont", "showtext"))
   ```

5. **Run the app locally:**

   ```r
   shiny::runApp()
   ```

6. The app will open automatically in your web browser — it’s optimized for **mobile**!

---

## 🌐 Deployment

To make your app accessible online:

* Deploy via [**shinyapps.io**](https://www.shinyapps.io)
* Or host on your own Shiny Server
* Embed in a Quarto website if desired:

  ````markdown
  ```{r, echo=FALSE}
  shinyAppDir("path/to/FPL-Mobile-App")
  ````

  ```
  ```

---

## 🧠 Future Enhancements

* 🧩 Gameweek-based live updates from the FPL API
* 🎨 Theme switcher (dark/light mode customization)
* 📈 Animated player performance visualizations (`gganimate`)
* 📊 Interactive plots with Plotly

---

## 👨‍💻 Author

**Andrew Peters**
*Data Scientist & Visualiser*
📊 Transforming FPL data into actionable insights
💼 Portfolio : [Click Here](https://andypeters94.quarto.pub/andrew-peters-background/)

---

## 📜 License

This project is licensed under the **MIT License** — feel free to use, modify, and share.

---

## ⭐ Acknowledgements

* [Fantasy Premier League API](https://fantasy.premierleague.com/)
* R community & Shiny developers
* All data © Premier League, used for non-commercial, analytical purposes