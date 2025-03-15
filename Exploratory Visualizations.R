# Load required packages
library(tidyverse)
library(plotly)
library(htmlwidgets)
library(RColorBrewer)

create_and_save_plots <- function() {
  # Custom Theme
  custom_theme <- theme_minimal() + theme(
    text = element_text(size = 12, family = "Arial"),
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
  
  # Color Palette
  color_palette <- brewer.pal(9, "Set1")
  
  # 1. Top 10 Medical Conditions (Static PNG + Interactive HTML)
  condition_overall <- data_clean %>%
    group_by(Medical.Condition) %>%
    summarise(Total = n(), .groups = 'drop') %>%
    arrange(desc(Total)) %>%
    slice(1:10)
  
  p1_static <- ggplot(condition_overall, 
                      aes(x = reorder(Medical.Condition, -Total), y = Total, 
                          fill = Medical.Condition)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_palette) +
    labs(title = "Top 10 Medical Conditions", x = "Condition", y = "Cases") +
    custom_theme
  
  # Save static
  ggsave("top_conditions.png", p1_static, width = 10, height = 6, dpi = 300)
  
  # Save interactive
  p1_interactive <- ggplotly(p1_static, tooltip = c("x", "y", "fill")) %>% 
    layout(hoverlabel = list(bgcolor = "white", font = list(size = 12)))
  saveWidget(p1_interactive, "top_conditions.html")
  
  # 2. Age Group Distribution (Static PNG + Interactive HTML)
  age_distribution <- data_clean %>%
    group_by(AgeGroup) %>%
    summarise(Count = n(), .groups = 'drop')
  
  p2_static <- ggplot(age_distribution, 
                      aes(x = AgeGroup, y = Count, fill = AgeGroup)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color_palette) +
    labs(title = "Age Group Distribution", x = "Age Group", y = "Count") +
    custom_theme
  
  ggsave("age_groups.png", p2_static, width = 8, height = 6, dpi = 300)
  
  p2_interactive <- ggplotly(p2_static, 
                             tooltip = c("x", "y")) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  saveWidget(p2_interactive, "age_groups.html")
  
  # 3. Hospital-Condition Heatmap (Static PNG + Interactive HTML)
  condition_hospital <- data_clean %>%
    group_by(Hospital, Medical.Condition) %>%
    summarise(Count = n(), .groups = 'drop') %>%
    filter(Count > 5)
  
  p3_static <- ggplot(condition_hospital, 
                      aes(x = Hospital, y = Medical.Condition, fill = Count)) +
    geom_tile() +
    scale_fill_gradient(low = "#D3E4CD", high = "#344E41") +
    labs(title = "Condition Frequency by Hospital") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  ggsave("hospital_heatmap.png", p3_static, width = 12, height = 8, dpi = 300)
  
  p3_interactive <- ggplotly(p3_static, 
                             tooltip = c("x", "y", "fill")) %>%
    layout(hoverlabel = list(bgcolor = "white"))
  saveWidget(p3_interactive, "hospital_heatmap.html")
  
  # 4. Time Trends (If available)
  if("Date.of.Admission" %in% colnames(data_clean)) {
    admission_trend <- data_clean %>%
      mutate(Date = as.Date(Date.of.Admission)) %>%
      group_by(Date) %>%
      summarise(Admissions = n(), .groups = 'drop')
    
    p4_static <- ggplot(admission_trend, aes(x = Date, y = Admissions)) +
      geom_line(color = "#1F77B4") +
      labs(title = "Admission Trends") +
      custom_theme
    
    ggsave("admissions_trend.png", p4_static, width = 10, height = 6, dpi = 300)
    
    p4_interactive <- ggplotly(p4_static, 
                               tooltip = c("x", "y")) %>%
      layout(hoverlabel = list(bgcolor = "white"))
    saveWidget(p4_interactive, "admissions_trend.html")
  }
}

# Execute the function
create_and_save_plots()