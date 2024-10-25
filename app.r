# app.R
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)


# 렌즈 SAG 계산 함수
calc_lens_sag <- function(radius, aperture) {
  return(radius - sqrt(radius^2 - (aperture/2)^2))
}

# 렌즈 aperture 계산 함수
calc_lens_aperture <- function(radius, sag) {
  return(2 * sqrt(2 * radius * sag - sag^2))
}

# 렌즈 왜곡 분석 함수들
simulate_line_distortion <- function(radius_of_curvature, x_positions, n, line_width, lens_height) {
  air_n <- 1.0
  y_lens <- numeric(length(x_positions))
  apparent_width <- rep(line_width, length(x_positions))
  
  # 렌즈 높이로부터 aperture 계산
  aperture <- calc_lens_aperture(radius_of_curvature, lens_height)
  half_aperture <- aperture / 2
  
  # 렌즈 영역 내의 점들에 대해서만 계산
  mask <- abs(x_positions) <= half_aperture
  x_valid <- x_positions[mask]
  
  # 렌즈 프로필 계산
  y_lens[mask] <- radius_of_curvature - 
    sqrt(radius_of_curvature^2 - x_valid^2)
  
  # 입사각과 굴절각 계산
  incident_angles <- atan2(x_valid, radius_of_curvature - y_lens[mask])
  refracted_angles <- asin(air_n / n * sin(incident_angles))
  
  # 왜곡된 선폭 계산
  apparent_width[mask] <- line_width * cos(incident_angles - refracted_angles)
  
  return(list(
    apparent_width = apparent_width,
    y_lens = y_lens,
    aperture = aperture
  ))
}


# UI 정의
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .irs-grid-text { font-size: 12px; }
      .well { background-color: #f8f9fa; }
      .plot-container { margin-top: 20px; }
    "))
  ),
  
  titlePanel("렌즈 왜곡 분석기 (SAG 높이 고려)"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # 슬라이더 입력들
      sliderInput("refractive_index",
                  "굴절률",
                  min = 1.3,
                  max = 2.0,
                  value = 1.65,
                  step = 0.01,
                  animate = list(interval = 300)),
      
      sliderInput("lens_height",
                  "렌즈 SAG 높이 (μm)",
                  min = 1,
                  max = 15,
                  value = 7,
                  step = 0.5,
                  animate = list(interval = 300)),
      
      sliderInput("radius_of_curvature",
                  "곡률 반경 (μm)",
                  min = 10,
                  max = 50,
                  value = 20,
                  step = 1,
                  animate = list(interval = 300)),
      
      hr(),
      
      # 계산된 결과 표시
      div(
        style = "background-color: white; padding: 15px; border-radius: 5px;",
        h4("렌즈 특성:", style = "color: #2c3e50;"),
        uiOutput("optical_properties")
      )
    ),
    
    mainPanel(
      width = 9,
      fluidRow(
        column(12,
               div(
                 class = "plot-container",
                 plotlyOutput("distortion_plot", height = "400px")
               )
        )
      ),
      fluidRow(
        column(12,
               div(
                 class = "plot-container",
                 plotlyOutput("lens_profile_plot", height = "400px")
               )
        )
      )
    )
  )
)


# Server 로직
server <- function(input, output, session) {
  # 반응형 데이터 생성
  simulation_data <- reactive({
    x_positions <- seq(-50, 50, length.out = 500)
    results <- simulate_line_distortion(
      input$radius_of_curvature,
      x_positions,
      input$refractive_index,
      5.0,  # 기본 배선 폭
      input$lens_height
    )
    
    list(
      x = x_positions,
      apparent_width = results$apparent_width,
      lens_profile = results$y_lens,
      aperture = results$aperture
    )
  })
  
  # 왜곡 플롯
  output$distortion_plot <- renderPlotly({
    data <- data.frame(
      x = simulation_data()$x,
      width = simulation_data()$apparent_width
    )
    
    half_aperture <- simulation_data()$aperture / 2
    
    p <- plot_ly(data, x = ~x) %>%
      add_lines(y = ~width, 
                name = "왜곡된 폭",
                line = list(color = 'rgb(31, 119, 180)', width = 2)) %>%
      add_lines(y = rep(5.0, length(data$x)), 
                name = "원래 폭",
                line = list(color = 'rgb(255, 65, 54)', 
                            width = 2, 
                            dash = 'dash')) %>%
      add_segments(x = -half_aperture, xend = -half_aperture,
                   y = 4, yend = 6,
                   line = list(color = 'gray', dash = 'dot'),
                   name = "렌즈 경계") %>%
      add_segments(x = half_aperture, xend = half_aperture,
                   y = 4, yend = 6,
                   line = list(color = 'gray', dash = 'dot'),
                   showlegend = FALSE) %>%
      layout(
        title = list(
          text = "배선 폭 왜곡 프로필",
          font = list(size = 20)
        ),
        xaxis = list(title = "위치 (μm)"),
        yaxis = list(title = "배선 폭 (μm)"),
        hovermode = "closest",
        showlegend = TRUE
      )
  })
  
  # 렌즈 프로필 플롯
  output$lens_profile_plot <- renderPlotly({
    data <- data.frame(
      x = simulation_data()$x,
      y = simulation_data()$lens_profile
    )
    
    half_aperture <- simulation_data()$aperture / 2
    
    p <- plot_ly(data, x = ~x, y = ~-y+max(y)) %>%
      add_lines(line = list(color = 'rgb(44, 160, 44)', width = 2),
                name = "렌즈 프로필") %>%
      add_segments(x = -half_aperture, xend = -half_aperture,
                   y = 0, yend = +input$lens_height,
                   line = list(color = 'gray', dash = 'dot'),
                   name = "렌즈 경계") %>%
      add_segments(x = half_aperture, xend = half_aperture,
                   y = 0, yend = +input$lens_height,
                   line = list(color = 'gray', dash = 'dot'),
                   showlegend = FALSE) %>%
      layout(
        title = list(
          text = "렌즈 단면 프로필",
          font = list(size = 20)
        ),
        xaxis = list(title = "위치 (μm)"),
        yaxis = list(
          title = "높이 (μm)", 
          scaleanchor = "x", 
          scaleratio = 1,
          range = c(0, input$lens_height * 1.2)
        ),
        showlegend = FALSE
      )
  })
  
# 광학 특성 출력
  output$optical_properties <- renderUI({
    aperture <- simulation_data()$aperture
    focal_length <- input$radius_of_curvature / (2 * (input$refractive_index - 1))
    max_distortion <- min(simulation_data()$apparent_width) / 5.0
    
    tagList(
      div(
        style = "font-size: 16px; margin-bottom: 10px;",
        sprintf("초점거리: %.1f μm", focal_length)
      ),
      div(
        style = "font-size: 16px; margin-bottom: 10px;",
        sprintf("유효 구경(Aperture): %.1f μm", aperture)
      ),
      div(
        style = "font-size: 16px; margin-bottom: 10px;",
        sprintf("최대 왜곡률: %.1f%%", (1 - max_distortion) * 100)
      ),
      div(
        style = "font-size: 16px;",
        sprintf("유효 개구수(NA): %.3f", 
                sin(atan(aperture/(2*focal_length))) * input$refractive_index)
      )
    )
  })
  
  # SAG 높이에 따른 곡률 반경 제한
  observe({
    min_radius <- input$lens_height / 2
    
    if (input$radius_of_curvature < min_radius) {
      updateSliderInput(session, "radius_of_curvature",
                       value = min_radius)
    }
  })
}

# 앱 실행
shinyApp(ui = ui, server = server)
