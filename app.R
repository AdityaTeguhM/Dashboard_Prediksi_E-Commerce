##########Bagian A###########
commerce <- data.frame(
  Month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),
  x1 = c(150000, 160000, 170000, 180000, 190000, 200000, 210000, 220000, 230000, 240000, 250000, 260000),
  x2 = c(8000, 9500, 10000, 10500, 11000, 9000, 11500, 12000, 12500, 13000, 14000, 15000),
  x3 = c(5, 4.5, 4.8, 4.6, 5.1, 4.7, 4.9, 5.0, 5.2, 5.3, 5.4, 5.5),
  x4 = c(8.5, 8.2, 8.4, 8.5, 8.6, 8.7, 8.8, 8.9, 8.7, 8.8, 8.9, 9.0),
  x5 = c(20000, 22000, 25000, 23000, 30000, 28000, 27000, 35000, 40000, 45000, 50000, 60000),
  y = c(120, 150, 160, 165, 180, 170, 190, 210, 230, 250, 300, 350)
)

library(shiny)
library(shinydashboard)

model <- lm(y ~ x1 + x2 + x3 + x4 + x5, data = commerce)



# Modifikasi UI dengan menambahkan menuItem
ui <- dashboardPage(
  dashboardHeader(title = "Aditya Teguh M"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Hasil Prediksi", tabName = "hasil_prediksi"),
      menuItem("Ringkasan Data", tabName = "ringkasan_data")
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(
        HTML("
          /* Mengatur lebar sidebarPanel */
          .sidebar {
            width: 250px; /* Atur sesuai kebutuhan */
          }

          /* Mengatur lebar mainPanel */
          .content-wrapper {
            margin-left: 260px; /* Lebar sidebarPanel + 10px untuk jarak */
          }
        ")
      )
    ),
    tabItems(
      tabItem(
        tabName = "hasil_prediksi",
        fluidPage(
          titlePanel("Prediksi Penjualan E-commerce"),
          sidebarLayout(
            sidebarPanel(
              numericInput("x1", "Jumlah Pengunjung Website:", value = 10000),
              numericInput("x2", "Jumlah Transaksi:", value = 500),
              numericInput("x3", "Rata-rata Item per Transaksi:", value = 3),
              sliderInput("x4", "Peringkat Kepuasan Pelanggan:", min = 1, max = 10, value = 5, step = 0.1),
              numericInput("x5", "Jumlah Iklan Online:", value = 20),
              actionButton("prediksi_button", "Hitung Prediksi")
            ),
            mainPanel(
              textOutput("prediksi_penjualan"),
              verbatimTextOutput("evaluasi_model"),
              HTML("<h3>Penjelasan Singkat tentang Model</h3>
                   <p>Model regresi linier ini dibuat untuk memprediksi penjualan bulanan berdasarkan variabel-variabel seperti jumlah pengunjung website, jumlah transaksi, rata-rata item per transaksi, peringkat kepuasan pelanggan, dan jumlah iklan online.</p>
                   <p>Faktor yang paling mempengaruhi penjualan dapat dilihat dari koefisien regresi masing-masing variabel dalam ringkasan evaluasi model di atas adalah variabel x2 (Jumlah Transaksi) dan variabel x5 (Jumlah Iklan Online).</p>")
            )
          )
        )
      ),
      tabItem(
        tabName = "ringkasan_data",
        fluidPage(
          titlePanel("Data & Ringkasan Data"),
          sidebarLayout(
            sidebarPanel(
              HTML("<p>Pilih dua variabel untuk scatterplot:</p>"),
              selectInput("var1", "Variabel X:", choices = setdiff(names(commerce), c("Month", "y"))),
              selectInput("var2", "Variabel Y:", choices = "y")
            ),
            mainPanel(
              plotOutput("scatterplot"), # Menampilkan scatterplot
              br(),
              fluidRow(
                column(
                  width = 6,
                  HTML("<p>Data dua belas bulan terakhir dari perusahaan e-commerce:</p>"),
                  tableOutput("data")
                ),
                column(
                  width = 6,
                  verbatimTextOutput("summary_data")
                )
              ),
              tags$style(HTML(".shiny-output-plot { margin-bottom: 20px; }"))
            )
          )
        )
      )
    )
  )
)


# Menambahkan event untuk button prediksi
server <- function(input, output) {
  output$summary_data <- renderPrint({
    summary_data <- summary(commerce)
    cat("\nRingkasan Data:\n")
    print(summary_data)
  })
  
  # Menampilkan data dalam format tabel
  output$data <- renderTable({
    commerce # Menampilkan beberapa baris pertama dari dataset
  })
  
  # Menampilkan scatterplot
  output$scatterplot <- renderPlot({
    req(input$var1, input$var2) # Memastikan kedua variabel dipilih sebelum plotting
    
    plot(commerce[, input$var1], commerce[, input$var2], 
         xlab = input$var1, ylab = input$var2,
         main = paste("Scatterplot", input$var1, "vs", input$var2))
  })
  
  observeEvent(input$prediksi_button, {
    prediksi <- predict(model, newdata = data.frame(
      x1 = input$x1,
      x2 = input$x2,
      x3 = input$x3,
      x4 = input$x4,
      x5 = input$x5
    ))
    output$prediksi_penjualan <- renderText({
      paste("Prediksi Penjualan Bulanan: $", round(prediksi, 2))
    })
  })
  
  output$evaluasi_model <- renderPrint({
    hasil_evaluasi <- summary(model)
    cat("\nRingkasan Evaluasi Model:\n")
    print(hasil_evaluasi)
  })  
}

# Menjalankan aplikasi Shiny
shinyApp(ui = ui, server = server)

