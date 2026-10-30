
#### Function to switch pages from the navigation bar ####
switch_sidebar_pages <- function(input, nav_id = "sidebar_pages", navset_id = "selected_page"){
    observeEvent(eventExpr = input[[nav_id]], {
      nav_select(id = navset_id, selected = input[[nav_id]])
  })
} 