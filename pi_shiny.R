library(shiny)
library(ggplot2)

# Função para calcular Pi usando método de Monte Carlo
calcular_pi_monte_carlo <- function(pontos) {
  set.seed(123)
  x <- runif(pontos, -1, 1)
  y <- runif(pontos, -1, 1)
  
  dentro_circulo <- sum(x^2 + y^2 <= 1)
  pi_estimado <- 4 * dentro_circulo / pontos
  
  return(pi_estimado)
}

# Função para calcular Pi usando série de Leibniz
calcular_pi_leibniz <- function(iteracoes) {
  pi_estimado <- 0
  for (k in 0:iteracoes) {
    pi_estimado <- pi_estimado + (-1)^k / (2*k + 1)
  }
  return(4 * pi_estimado)
}

# Interface do usuário
ui <- fluidPage(
  titlePanel("Calculadora de Pi"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("metodo", "Método de Cálculo:",
                  choices = c("Monte Carlo", "Série de Leibniz")),
      
      sliderInput("precisao", "Precisão:",
                  min = 100, max = 100000, 
                  value = 1000, step = 100),
      
      actionButton("calcular", "Calcular Pi")
    ),
    
    mainPanel(
      h3("Resultado:"),
      verbatimTextOutput("resultado_pi"),
      
      plotOutput("grafico_convergencia")
    )
  )
)

# Servidor
server <- function(input, output, session) {
  resultado <- eventReactive(input$calcular, {
    if (input$metodo == "Monte Carlo") {
      calcular_pi_monte_carlo(input$precisao)
    } else {
      calcular_pi_leibniz(input$precisao)
    }
  })
  
  output$resultado_pi <- renderText({
    paste("Valor estimado de Pi:", round(resultado(), 6),
          "\nValor real de Pi:   ", round(pi, 6),
          "\nErro:              ", abs(round(resultado() - pi, 6)))
  })
  
  output$grafico_convergencia <- renderPlot({
    pontos <- seq(100, input$precisao, length.out = 50)
    
    if (input$metodo == "Monte Carlo") {
      valores <- sapply(pontos, calcular_pi_monte_carlo)
    } else {
      valores <- sapply(pontos, calcular_pi_leibniz)
    }
    
    df <- data.frame(pontos = pontos, valores = valores)
    
    ggplot(df, aes(x = pontos, y = valores)) +
      geom_line(color = "blue") +
      geom_hline(yintercept = pi, color = "red", linetype = "dashed") +
      labs(title = paste("Convergência de Pi -", input$metodo),
           x = "Número de Pontos/Iterações",
           y = "Valor Estimado") +
      theme_minimal()
  })
}

# Rodar o aplicativo
shinyApp(ui, server)