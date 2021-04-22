function(input, output, session) {
  
  output$textOne <- renderText({ "Rosenbrock Funktion" })
 
  output$plotOne <- renderPlot({

    rosenbrock <- function(x, y){
      (1-x)^2+100*(y-x^2)^2
    }
   # func(x,y) <- reactive({
   #   if(input$functione == "rosenbrock"){
   #     (1-x)^2+100*(y-x^2)^2
   #   }
   #   else{
   #     return (x^2+y-11)^2+(x+y^2-7)^2
   #   }})

    x <- y <- seq(-1, 1, length= 20)
    z <- outer(x, y, rosenbrock )
    persp(x, y, z, 
          main = "Rosenbrock-Funktion mit a=1, b=100)",
          theta = input$thetaR, phi = input$phiR,#150 20 
          shade = 0.4, col = "green")
  })
  
  output$textTwo <- renderText({ "Himmelblau Funktion" })
  
  output$plotTwo <- renderPlot({
    himmelblau <- function(x, y){
      (x^2+y-11)^2+(x+y^2-7)^2
    }
      x <- y <- seq(-5, 5, length= 20)
      z <- outer(x, y, himmelblau )
      
      persp(x, y, z,
            main="Himmelblau-Funktion",
            theta = input$thetaH, phi = input$phiH,
            col = "orange", shade = 0.5)
    
  })
  
}