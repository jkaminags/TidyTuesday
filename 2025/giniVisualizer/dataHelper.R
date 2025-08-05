suppressWarnings({# Helper script for loading, formatting, and cleaning the Gini data
# and creating a dumbbell plot from it
tuesdata <- tidytuesdayR::tt_load('2025-08-05')
income_inequality_processed <- tuesdata$income_inequality_processed
income_inequality_raw <- tuesdata$income_inequality_raw

# Most unequal countries: Colombia, Brazil, Paraguay, Mexico, Bulgaria
# Most equal countries: Sweden, Netherlands, Austria, Norway, Belgium
# Earliest date on inequality for countries is 1975.
countries_gini <- income_inequality_processed |>
  select(-Code) |>
  filter(Year >= 1975 & Entity %in% c("Colombia", "Brazil", "Paraguay",
                                      "Mexico", "Bulgaria", "Sweden",
                                      "Netherlands", "Austria", "Norway",
                                      "Belgium")) |>
  pivot_longer(cols = c(gini_mi_eq, gini_dhi_eq),
               values_to = "GiniScore") |>
  mutate(
    .by = c(Entity, Year),
    GiniScore = round(GiniScore, 2),
    TaxStatus = if_else(name == "gini_mi_eq", "Pre-tax", "Post-tax") |>
      fct_relevel("Pre-tax", "Post-tax")
  )


mean_gini <- countries_gini |>
  mutate(.by = c(Year, TaxStatus),
         GiniScore = mean(GiniScore, na.rm = TRUE) |>
           round(2),
         Entity = "<b>Mean Gini Score</b>") |>
  distinct()

full_gini <- rbind(countries_gini, mean_gini) |>
  mutate(
    Entity = Entity |>
      fct_relevel("Belgium", "Norway", "Austria", "Netherlands", "Sweden",
                  "Bulgaria", "Mexico", "Paraguay", "Brazil", "Colombia",
                  "<b>Mean Gini Score</b>")
  )

y.cols <- c(rep("#006A8EFF", 5), rep("#B1283AFF", 5), "#4f4f4f")

make_gini_plot <- function(df_gini, year) {
    p <- suppressWarnings({df_gini |> filter(Year == year) |>
      ggplot(aes(x = GiniScore, y = Entity)) +
            geom_line(aes(group = Entity), linewidth = 4,
                      color = "#d8d8d8") +
            geom_point(aes(color = TaxStatus), size = 5.5) +
            geom_text(aes(x = GiniScore, label=GiniScore, color=TaxStatus),
                      size = 5.75,
                      nudge_x = if_else(
                        df_gini$name == "gini_mi_eq", 0.015, -0.015)
            ) +
            geom_text(aes(label = TaxStatus, color = TaxStatus),
                      data = ~. |> filter(Entity == "<b>Mean Gini Score</b>"),
                      nudge_y = 0.5,
                      fontface = "bold",
                      size = 5.5) +
            scale_x_continuous(limits = c(0.2, 0.65)) +
            # Don't drop countries with no data for a certain year
            scale_y_discrete(drop = FALSE) +
            scale_color_paletteer_d("ltc::trio1") +
            labs(x = "Gini coefficient", y = NULL) +
            coord_cartesian(ylim = c(1, 11.6)) +
            theme_minimal() +
            theme(legend.position = "none",
                  axis.text.y = element_markdown(size = 15.5,
                                                 color = y.cols),
                  axis.text.x = element_text(size = 16),
                  axis.title.x = element_text(size = 18),
                  panel.grid.major.y = element_blank(),
                  panel.grid.minor.x = element_blank())})
    p
}

})
