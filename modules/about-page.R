about_UI <- function(id) {
  ns <- NS(id) 
  
  tagList(
    fluidRow(
      column(width = 12, 
             tags$div(
               class="about",
               tags$h1("About SHO-WR"),
               tags$p("
                     The SHO-WR application demonstrates the power of open data paired with open source analytics and visualization tools for California water resources management. The application  has been developed iteratively as part of a demonstration project led by the Sacramento River Settlement Contractors (SRSC). The primary objective of this demonstration project is to integrate diverse flow, water operations, fishery, and water quality data into a single, open data platform that facilitates more data-driven and timely decision making.  On the section of the Sacramento River immediately below Lake Shasta, the fishery agencies have targeted water temperature as the most critical resource to successful spawning of winter-run Chinook salmon from late April through September.  This single parameter controls the operation of Shasta Reservoir, SRSC diversions, the Central Valley Project (CVP), other project reservoirs, and the Bay Delta.",
                      tags$br(),
                      tags$br(),
                      "SHO-WR brings real-time and historical data together in a user-designed way to improve decision making shared between the U.S. Bureau of Reclamation (Reclamation), the National Marine Fisheries Service (NMFS), the California Department of Fish and Wildlife (CDFW), and the State Water Resources Control Board (SWRCB). In addition, SHO-WR provides real-time tracking and accounting of operations based on those decisions, and is compiling a database of historical operations and decision data that will provide critical information for future decision making.", 
                      tags$br(),
                      tags$br(),
                      "Key decisions on operations and fishery protection begin in the spring each year and continue through September.  The fishery agencies, Reclamation, and the SWRCB meet monthly, or more often if needed, as the Sacramento River Temperature Task Group (SRTTG) to discuss and decide on operations and adjustments.  SHO-WR could be used to inform actions from that work group as well as provide members with real-time and historical data that will be useful between meetings, and facilitate more productive meetings.
                     "), 
               tags$br(),
               tags$a(tags$img(src = 'TransLogoTreb.png', width = '150px', style = 'display:inline-block;'),
                      href = 'http://www.flowwest.com/', target = '_blank'),
               tags$h6('App created and maintained by:', tags$a(href = 'mailto:erodriguez@flowwest.com', 'Emanuel Rodriguez', target = '_blank'),
                       'and', tags$a(href = 'mailto:sgill@flowwest.com', 'Sadie Gill', target = '_blank'), 
                       style = 'display:inline-block; margin-left:15px;'),
               tags$a(tags$img(src = 'GitHub-Mark-32px.png'), href = 'https://github.com/FlowWest/showr-application', target = '_blank', style = 'margin-left:15px;')
             ))
    ) 
  )
} 

about_server <- function(input, output, session) {
  #
}  