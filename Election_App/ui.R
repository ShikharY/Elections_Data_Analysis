fluidPage(
  titlePanel("Gender Ratio in Indian Elections"),
 sidebarLayout(
   sidebarPanel(
   selectInput(
    "state",
    "State",
    choices = c(
      "Andhra Pradesh",
      "Arunachal Pradesh",
      "Assam" ,
      "Bihar",
      "Chhattisgarh",
      "Goa",
      "Gujarat",
      "Haryana",
      "Himachal Pradesh",
      "Jammu & Kashmir",
      "Jharkhand",
      "Karnataka",
      "Kerala",
      "Madhya Pradesh",
      "Maharashtra",
      "Manipur",
      "Meghalaya",
      "Mizoram",
      "Nagaland",
      "National Capital Territory Of Delhi",
      "Odisha",
      "Puducherry",
      "Punjab",
      "Rajasthan",
      "Sikkim",
      "Tamil Nadu",
      "Tripura",
      "Uttar Pradesh",
      "Uttarakhand",
      "West Bengal"),
    multiple = TRUE,
    selected = c("West Bengal", "Kerala")
  ),
  sliderInput("y_axis", "Set Y axis", min = 1, max = 100, value = 100 ),
  sliderInput("year", "Year", min = 1978, max = 2015, value = c(1977,2015), step = 1, sep = "" ),
   ), 
  mainPanel(
  plotOutput("plot")
  
  )
)
)