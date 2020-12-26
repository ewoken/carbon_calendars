print("Check dependencies...")
list.of.packages <- c("ggplot2", "tidyverse", "lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(ggplot2)
library(tidyverse)
library(lubridate)

get_month_week = function(d) {
  month_day = day(d)
  d2 = date(d)
  day(d2) <- 1
  offset = wday(d2) - 1
  return(ceiling((offset + month_day) / 7))
}

years_germany = seq(2010, 2019)
get_all_data_germany = function() {
    data = tibble()

    for (year in years_germany) {
        year_data = read_delim(paste("./data/germany/", year, ".csv", sep=""), delim = ",")
        
        if (year > 2018) {
            year_data = year_data %>%
                mutate(day_hour = paste(date(startDate), hour(startDate))) %>%
                group_by(day_hour) %>%
                summarise(
                    startDate = first(startDate),
                    endDate = last(endDate),
                    trade = mean(trade),
                    hydro = mean(hydro),
                    biomass = mean(biomass),
                    nuclear = mean(nuclear),
                    brown_coal = mean(brown_coal),
                    hard_coal = mean(hard_coal),
                    gas = mean(gas),
                    oil = mean(oil),
                    wind = mean(wind),
                    solar = mean(solar),
                    pumped_storage = mean(pumped_storage),
                    seasonal_storage = mean(seasonal_storage),
                    others = mean(others)
                ) %>%
                select(-day_hour)
        }
        
        data = bind_rows(data, year_data)
    }

    data = data %>%
        select(-endDate) %>%
        rename(date = startDate) %>%
        mutate(day_date = date(date)) %>%
        mutate(import = if_else(trade > 0, trade, 0)) %>%
        mutate(export = if_else(trade < 0, -trade, 0)) %>%
        mutate(prod = hydro + biomass + nuclear + brown_coal + hard_coal + gas + oil + solar + wind + pumped_storage + seasonal_storage + others) %>%
        mutate(year = year(date)) %>%
        mutate(month = month(date)) %>%
        mutate(month_label = month(date, TRUE, TRUE, locale="en_US")) %>%
        mutate(week_day = wday(day_date)) %>%
        mutate(week_day_label = wday(day_date, TRUE, TRUE, locale="en_US")) %>%
        mutate(day_type = if_else(week_day == 1 | week_day == 7, "weekend", "weekday")) %>%
        mutate(
            co2_kg = (
                brown_coal * 0.820 +
                hard_coal * 0.820 +
                gas * 0.490 +
                (hydro + pumped_storage) * 0.024 +
                nuclear * 0.012 +
                oil * 0.65 +
                solar * 0.045 +
                wind * 0.011 +
                biomass * 0.230
            ) * 500,
        ) %>%
        mutate(conso_co2_kg = co2_kg + import * 500 * 0.420) %>%
        mutate(co2_kg_kwh = co2_kg / (500 * prod)) %>%
        mutate(conso_co2_kg_kwh = conso_co2_kg / (500 * (prod + import)))

    return(data)
}
data_germany = get_all_data_germany()

germany_calendar_data = data_germany %>%
    group_by(day_date) %>%
    summarise(
        prod = sum(prod),
        import = sum(import),
        conso_co2_kg = sum(conso_co2_kg),
        week_day_label = first(week_day_label),
        year = last(year),
        month_label = last(month_label),
    ) %>%
    mutate(month_week = mapply(get_month_week, day_date)) %>%
    mutate(conso_co2_kg_kwh = conso_co2_kg / ((prod + import) * 500))

ggplot(germany_calendar_data, aes(x = week_day_label, y = month_week, fill = conso_co2_kg_kwh)) +
    geom_tile(color = "white") +
    facet_grid(year~month_label) +
    scale_y_reverse() +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
    ) +
    scale_fill_gradientn(
     colours = c('#2AA364', '#F5EB4D', '#9E4229', '#381d02'),
     values = c(0, 150, 600, 750) / 750,
     limits = c(0, 0.750)
    ) +
    labs(
        title = "GHG emissions of german electricity consumption by day",
        x = "",
        y = "",
        fill = "GHG Emissions\n(kg CO2eq/kWh)",
        caption = "Data: energy-charts.de, Emission factors: IPCC 2014"
    ) +
    ggsave("./germany.png", width = unit(12, "cm"), height = unit(6, "cm"))
