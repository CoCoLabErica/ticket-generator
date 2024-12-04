library(ggplot2)
library(grid)

# Ticket customisation parameters
num_tickets <- 10 # Number of tickets to generate (minimum is 1 ticket, and setting it to 200 tickets still gave a fast PDF output)
ticket_color <- "blue4" # Theme color
event_name <- "Art for HDR Wellbeing" # Event name
event_date <- "29 November 2024" # Event date
event_info <- "AGNSW at 3:00pm on 29 November 2024" # Event info
organiser <- "by HDR Peer Community" # Event organiser
content <- "Keep this ticket to join our guided art gallery tour" # Ticket content
output_file <- "tickets.pdf" # Output file name

# Function to create a single ticket
create_ticket <- function(number, ticket_color, event_name, organiser, event_date, event_info, content) {
  ggplot() +
    theme_void() +
    # Left side: For record (1/3 of total width)
    geom_rect(aes(xmin = 0, xmax = 2, ymin = 0, ymax = 2), fill = "white", color = "black") +
    annotate("text", x = 1, y = 1.8, label = event_name, size = 2.8, fontface = "bold") +
    annotate("text", x = 1, y = 1.5, label = organiser, size = 2.8, fontface = "italic") +
    annotate("text", x = 1, y = 1, label = sprintf("Ticket #%02d", number), size = 6, fontface = "bold", color = ticket_color) +
    annotate("text", x = 0.1, y = 0.5, label = "Ticket holder: _________________", size = 3, hjust = 0) +
    annotate("text", x = 1, y = 0.2, label = event_date, size = 2.8) +
    # Right side: For attendee (2/3 of total width)
    geom_rect(aes(xmin = 2, xmax = 6, ymin = 0, ymax = 2), fill = ticket_color, color = "white") +
    annotate("text", x = 4, y = 1.8, label = event_name, size = 5, fontface = "bold", color = "white") +
    annotate("text", x = 4, y = 1.5, label = organiser, size = 4, fontface = "italic", color = "white") +
    annotate("text", x = 4, y = 1, label = sprintf("Ticket #%02d", number), size = 6, fontface = "bold", color = "white") +
    annotate("text", x = 4, y = 0.5, label = content, size = 4, color = "white") +
    annotate("text", x = 4, y = 0.2, label = event_info, size = 3, color = "white") +
    # Dashed line between left and right parts
    geom_segment(aes(x = 2, xend = 2, y = 0, yend = 2), linetype = "dashed", color = "black") +
    theme(plot.margin = margin(10, 10, 10, 10))
}

# Function to generate and save tickets as a PDF
generate_tickets <- function(num_tickets, ticket_color, event_name, organiser, event_date, event_info, content, output_file) {
  tickets_per_page <- 5 # 5 tickets per page (1 column x 5 rows)
  pdf(output_file, width = 8.5, height = 11) # Letter-sized PDF (8.5 × 11 inches)

  for (i in seq(1, num_tickets, by = tickets_per_page)) {
    grid::grid.newpage()

    for (j in 0:(tickets_per_page - 1)) {
      ticket_number <- i + j
      if (ticket_number > num_tickets) break

      # Ticket position in grid (single column, 5 rows)
      y_pos <- 1 - (j * 0.2)

      # Create a viewport for the ticket
      vp <- grid::viewport(x = 0.5, y = y_pos - 0.1, width = 0.9, height = 0.2)
      print(create_ticket(ticket_number, ticket_color, event_name, organiser, event_date, event_info, content), vp = vp)
    }
  }
  dev.off()

  cat(sprintf("A PDF file with %d tickets has been saved as '%s'\n", num_tickets, output_file))
}

# Generate tickets and output the PDF file
generate_tickets(num_tickets, ticket_color, event_name, organiser, event_date, event_info, content, output_file)

