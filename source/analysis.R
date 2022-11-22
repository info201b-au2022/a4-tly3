library(tidyverse)

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return ("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num=6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#
# Your functions and variables might go here ... <todo: update comment>
#----------------------------------------------------------------------------#

# getting the data
data <- get_data()

# what is the percentage of the state's population vs. percentage of jail + prison population
# for each group of people?
# which group of people most often constitutes a larger part of prisons and
# jails but makes up a smaller percentage of the state's population?

# calculates (for each state + DC) the total population, the total population
# of those age 15-64, total population of aapi, black, latinx, native, and white
# people
state_pop_percent <- data %>%
  filter(year == 2016) %>%
  select(state, total_pop, total_pop_15to64, aapi_pop_15to64, black_pop_15to64,
         latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  group_by(state) %>%
  summarise(across(c(total_pop, total_pop_15to64, aapi_pop_15to64,
                     black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
                     white_pop_15to64), sum)) %>%
  mutate(aapi_15to64_percent = aapi_pop_15to64 / total_pop_15to64 * 100,
         black_15to64_percent = black_pop_15to64 / total_pop_15to64 * 100,
         latinx_15to64_percent = latinx_pop_15to64 / total_pop_15to64 * 100,
         native_15to64_percent = native_pop_15to64 / total_pop_15to64 * 100,
         white_15to64_percent = white_pop_15to64 / total_pop_15to64 * 100) %>%
  select(state, aapi_15to64_percent, black_15to64_percent,
         latinx_15to64_percent, native_15to64_percent, white_15to64_percent)

# calculates the total incarceration (prison + jail) for each state + DC for
# each group of people (aapi, black, latinx, native, white)
incarceration_pop_percent <- data %>%
  filter(year == 2016) %>%
  select(state, total_jail_pop, aapi_jail_pop, black_jail_pop, latinx_jail_pop,
         native_jail_pop, white_jail_pop, total_prison_pop, aapi_prison_pop,
         black_prison_pop, latinx_prison_pop, native_prison_pop, 
         white_prison_pop) %>%
  group_by(state) %>%
  summarise(across(c(everything()), sum, na.rm = TRUE)) %>%
  mutate(total_incarcerated = total_jail_pop + total_prison_pop,
         aapi_incarcerated = aapi_jail_pop + aapi_prison_pop,
         black_incarcerated = black_jail_pop + black_prison_pop,
         latinx_incarcerated = latinx_jail_pop + latinx_prison_pop,
         native_incarcerated = native_jail_pop + native_prison_pop,
         white_incarcerated = white_jail_pop, white_prison_pop) %>%
  mutate(
    aapi_incarcerated_percent = aapi_incarcerated / total_incarcerated * 100,
    black_incarcerated_percent = black_incarcerated / total_incarcerated * 100,
    latinx_incarcerated_percent = latinx_incarcerated / total_incarcerated * 100,
    native_incarcerated_percent = native_incarcerated / total_incarcerated * 100,
    white_incarcerated_percent = white_incarcerated / total_incarcerated * 100
    ) %>%
  select(state, aapi_incarcerated_percent, black_incarcerated_percent,
         latinx_incarcerated_percent, native_incarcerated_percent,
         white_incarcerated_percent)

# does a group's 15 to 64 population percentage - that groups' incarceration
# percentage to see which group has the largest negative difference (large 
# negative means that there's a higher percentage of people from that group
# incarcerated compared to their population). Then across all races, it finds
# the group of people with the largest negative number for each state and stores
# both the minimum value (min_val) and the group of people who has that minimum
# value (min_col).
#
# For example, AK (Alaska) shows that native people has the largest difference
# between population (15-64) and incarceration. They make up 15.16% of the 
# 15-64 population but they make up 41.67% of the incarcerated population. This
# results in a native_diff of -26.51%, the largest among the 5 races of that
# state. Thus, min_val = -26.51 and min_col = native_diff
state_pop_v_incarceration_pop <- 
  inner_join(state_pop_percent,incarceration_pop_percent, by="state") %>%
  mutate(aapi_diff = aapi_15to64_percent - aapi_incarcerated_percent,
         black_diff = black_15to64_percent - black_incarcerated_percent,
         latinx_diff = latinx_15to64_percent - latinx_incarcerated_percent,
         native_diff = native_15to64_percent - native_incarcerated_percent,
         white_diff = white_15to64_percent - white_incarcerated_percent) %>%
  gather(group, num, contains('diff')) %>%
  group_by(state) %>%
  mutate(min_val = min(num), 
         min_col = group[num == min_val]) %>%
  spread(group, num)

# ****** Values To Be Used *******

# race that has the most largest negative difference across the states + DC
group_with_most_diff <-
  names(which.max(table(state_pop_v_incarceration_pop$min_col)))

# aapi_diff out of 50 states
aapi_diff_count <- state_pop_v_incarceration_pop %>%
  filter(min_col == "aapi_diff") %>%
  nrow()

# black_diff out of 50 states + DC
black_diff_count <- state_pop_v_incarceration_pop %>%
  filter(min_col == "black_diff") %>%
  nrow()

# latinx_diff out of 50 states + DC
latinx_diff_count <- state_pop_v_incarceration_pop %>%
  filter(min_col == "latinx_diff") %>%
  nrow()

# native_diff out of 50 states + DC
native_diff_count <- state_pop_v_incarceration_pop %>%
  filter(min_col == "native_diff") %>%
  nrow()

# white_diff out of 50 states + DC
white_diff_count <- state_pop_v_incarceration_pop %>%
  filter(min_col == "white_diff") %>%
  nrow()

# Which state has the highest prison and jail population rate?
state_most_incarcerated <- data %>%
  filter(year == 2016) %>%
  rowwise() %>%
  mutate(total = sum(total_jail_pop_rate, total_prison_pop_rate, na.rm = T)) %>%
  ungroup() %>%
  group_by(state) %>%
  summarise(total_state = sum(total)) %>%
  filter(total_state == max(total_state)) %>%
  pull(state)

# which region has the highest incarceration rate (prison + jail)
areas_most_incarcerated <- data %>%
  filter(year == 2016) %>%
  rowwise() %>%
  mutate(total = sum(total_jail_pop_rate, total_prison_pop_rate, na.rm = T)) %>%
  ungroup() %>%
  group_by(region) %>%
  summarise(total_region = sum(total)) %>%
  filter(total_region == max(total_region)) %>%
  pull(region)

# which urbanicity has the highest incarceration rate (prison + jail)
urbanicity_most_incarcerated <- data %>%
  filter(year == 2016) %>%
  rowwise() %>%
  mutate(total = sum(total_jail_pop_rate, total_prison_pop_rate, na.rm = T)) %>%
  ungroup() %>%
  group_by(urbanicity) %>%
  summarise(total_urbanicity = sum(total)) %>%
  filter(total_urbanicity == max(total_urbanicity)) %>%
  pull(urbanicity)

# which state has the highest percent of women incarcerated?
state_highest_women_incarcerated <- data %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarise(state_pop = sum(total_pop), women_incarcerated_pop = 
           sum(female_jail_pop, female_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = women_incarcerated_pop / state_pop * 100) %>%
  filter(percent == max(percent)) %>%
  pull(state)

# which state has the highest percent of men incarcerated?
state_highest_men_incarcerated <- data %>%
  filter(year == 2016) %>%
  group_by(state) %>%
  summarise(state_pop = sum(total_pop), men_incarcerated_pop = 
              sum(male_jail_pop, male_prison_pop, na.rm = TRUE)) %>%
  mutate(percent = men_incarcerated_pop / state_pop * 100) %>%
  filter(percent == max(percent)) %>%
  pull(state)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#

# gets the jail population for each year
get_year_jail_pop <- function() {
  jail_pop_by_year <- data %>%
    group_by(year) %>%
    summarise(total = sum(total_jail_pop, na.rm = T))
return(jail_pop_by_year)   
}

# plots the jail population for each year using a bar graph
plot_jail_pop_for_us <- function()  {
  to_plot <- get_year_jail_pop()
  
  bar_chart <- 
    ggplot(to_plot, aes(x = year, y = total)) +
    labs(title="Increase of Jail Population in the U.S. (1970-2018)", x="Year",
         y="Total Jail Population",
         caption="The growth of the jail population in the United States over
         the years from 1970 to 2018.") +
    geom_bar(width = 0.9, stat = "identity") +
    scale_y_continuous(labels = scales::comma)
  
  return(bar_chart)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# gets the jail population for the states passed in
get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- data %>%
    group_by(year, state) %>%
    summarise(total = sum(total_jail_pop, na.rm = T)) %>%
    filter(state %in% states)
  
  return(jail_pop_by_states)
}

# plots the jail population for the states passed in on a line graph
plot_jail_pop_by_states <- function(states) {
  to_plot <- get_jail_pop_by_states(states)
  
  line_graph <- ggplot(to_plot, aes(x = year, y = total, color = state)) +
    labs(title="Increase of Jail Population in the U.S. by State(s) (1970-2018)"
         , x="Year",
         y="Total Jail Population",
         caption="Displays the growth of the jail population for the specified
         states from the year 1970 to 2018") +
    scale_y_continuous(labels = scales::comma) +
    geom_line()
  
  return(line_graph)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# gets the black population percentage for every state + DC, and the black
# incarceration population percentage for every state + DC
get_state_pop_v_jail_pop_for_black_pop <- function() {
  state_pop_15to64 <- data %>%
    filter(year == 2016) %>%
    select(state, total_pop_15to64) %>%
    group_by(state) %>%
    summarise(total_pop_15to64 = sum(total_pop_15to64))
  
  state_pop_black_15to64 <- data %>%
    filter(year == 2016) %>%
    select(state, black_pop_15to64) %>%
    group_by(state) %>%
    summarise(black_pop_15to64 = sum(black_pop_15to64))
  
  state_black_pop_percent <- left_join(state_pop_15to64,
                                       state_pop_black_15to64) %>%
    mutate(black_15to64_percent = black_pop_15to64 / total_pop_15to64 * 100) %>%
    select(state, black_15to64_percent)
    
    #summarise(across(c(total_pop, total_pop_15to64, black_pop_15to64), sum)) %>%
    #mutate(black_15to64_percent = black_pop_15to64 / total_pop_15to64 * 100) %>%
    #select(state, black_15to64_percent)
  
  incarceration_pop_percent_black <- data %>%
    filter(year == 2016) %>%
    select(state, total_jail_pop, black_jail_pop, total_prison_pop,
           black_prison_pop) %>%
    group_by(state) %>%
    summarise(total_jail_pop = sum(total_jail_pop, na.rm = TRUE),
              black_jail_pop = sum(black_jail_pop, na.rm = TRUE),
              total_prison_pop = sum(total_prison_pop, na.rm = TRUE),
              black_prison_pop = sum(black_prison_pop, na.rm = TRUE)) %>%
    mutate(total_incarcerated = total_jail_pop + total_prison_pop,
           black_incarcerated = black_jail_pop + black_prison_pop) %>%
    mutate(black_incarcerated_percent =
             black_incarcerated / total_incarcerated * 100) %>%
    select(state, black_incarcerated_percent)
    
    #summarise(across(everything(), sum, na.rm = TRUE))
    #mutate(total_incarcerated = total_jail_pop + total_prison_pop,
    #       black_incarcerated = black_jail_pop + black_prison_pop) %>%
    #mutate(black_incarcerated_percent = black_incarcerated / total_incarcerated * 100) %>%
    #select(state, black_incarcerated_percent)

  state_pop_v_incarceration_pop_black_pop <- inner_join(state_pop_percent,
                                              incarceration_pop_percent, by="state")
  return(state_pop_v_incarceration_pop)
}

# plot the lack population percentage for every state + DC, and the black
# incarceration population percentage for every state + DC using a scatterplot
plot_state_pop_v_jail_pop_for_black_pop <- function() {
  to_plot <- get_state_pop_v_jail_pop_for_black_pop()
  
  scatterplot <- to_plot %>%
    ggplot(aes(x=black_15to64_percent, y=black_incarcerated_percent)) +
    geom_point(size=1.8, colour = 4) +
    labs(title="Proportion of Black Population in a State vs. 
         Proportion in that State's Jails", 
         x = "Percentage of the State's population who are Black 
         (15 - 64 year olds)",
         y = paste0("Percentage of the State's Prison and Jail", 
                    "\n", "Population who are black (all ages)"),
         caption = "Each state is represented by a dot. Displays the percentage 
         of a state's population who are black in relation to the percentage of
         the state's jail and prison population who are black.") +
    geom_text(aes(label = state), 
              size = 2, 
              position = position_dodge(width = 1),
              vjust = -.8, hjust = 1) +
    xlim(0, 100) +
    ylim(0, 100) +
    theme(axis.title = element_text(size = 11)) 
  
  return(scatterplot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

# gets the Texas county latitude and longitude to create map, and gets the
# latinx incarceration population for each county in Texas
get_latinx_and_texas_data <- function() {
  texas_count_latinx_incarceration <- data %>%
    filter(year == 2016, state == "TX") %>%
    rowwise() %>%
    mutate(total_incar = sum(latinx_jail_pop, latinx_prison_pop, na.rm = T)) %>%
    select(state, county_name, total_incar)
  
  texas_count_latinx_incarceration$county_name <- 
    sub(" .*", "", texas_count_latinx_incarceration$county_name)
  
  county_shapes <- map_data("county") %>%
    filter(region == "texas") %>%
    rename(county_name = subregion)
  
  texas_map_data <- county_shapes %>%
    left_join(texas_count_latinx_incarceration %>%
              mutate(county_name = tolower(county_name)))
  return(texas_map_data)
}

# creates a map of texas and its counties, and plots the latinx incarceration
# population
plot_latinx_incar_pop_texas <- function() {
  texas_map_data <- get_latinx_and_texas_data()
  
  map <- ggplot(texas_map_data) +
    geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = total_incar)) +
    coord_map() +
    labs(title="Latinx Incarceration Population in Texas",
         caption = "For each county in Texas, displays the number of latinx 
         people incarcerated. Dark gray indicates missing data.") +
    guides(fill=guide_legend(title="Incarceration Pop.")) +
    scale_fill_gradient(low = "grey", high = "red") +
    theme_void()
  
  return(map)
}

## Load data frame ---- 


