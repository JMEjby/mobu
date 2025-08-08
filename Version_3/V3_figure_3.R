# Load required library
library(DiagrammeR)
library(RColorBrewer)

cm_to_pixels <- function(cm, dpi = 600) {
  inches <- cm / 2.54  # Convert cm to inches
  pixels <- inches * dpi
  return(round(pixels))
  }

# initialization graph# Get colors for consistency using Set3 palette
set_colors <- brewer.pal(8, "Set3")

# Create the main conceptual flow diagram with Set3 colors
flow_diagram <- grViz("
digraph model_flow {
  
  # Graph attributes
  graph [layout = dot, rankdir = TB, bgcolor = white, fontsize = 16]
  
  # Node attributes
  node [shape = box, style = filled, fontname = 'Arial', fontsize = 14]
  
  # Define node styles using Set3 palette colors 5-8 (blue, orange, green, pink)
  subgraph cluster_1 {
    label = 'AGENT INITIALIZATION'
    style = filled
    fillcolor = '#80B1D3'  # Set3 blue (color 5)
    fontsize = 16
    fontname = 'Arial'
    
    # Population generation
    pop [label = 'Generate Population (N agents)', fillcolor = '#B3D1E6']  # Lighter blue
    
    # Value assignment  
    values [label = 'Assign Value Profiles (A-E categories)\nBased on EA alignment', fillcolor = '#B3D1E6']
    
    # Agent types
    types [label = 'Assign Agent Types:\n• 1 Leader (highly value aligned)\n• N_EA EAs (mod-highly value aligned)\n• Non-EAs (low-moderately aligned)', fillcolor = '#B3D1E6']
    
    # Initial signals
    signals [label = 'Initialize 5 Signals for each Agent\nSignals are binary or categorical (5 levels)', 
                      fillcolor = '#B3D1E6']
  }
  
  subgraph cluster_2 {
    label = 'ALIGNMENT & CONNECTIONS'
    style = filled
    fillcolor = '#B3DE69'  # Set3 green (color 7)
    fontsize = 16
    fontname = 'Arial'
    
    # Connections
    connect [label = 'Create Full Network (all agents connected)', fillcolor = '#D1E9A4']  # Lighter green
    
    # Alignement calculation
    alignement [label = 'Calculate Perceived Alignment and Misalignment between all agent pairs\nBased on signal overlap', fillcolor = '#D1E9A4']
  }
  
  
  # Define edges with labels
  pop -> values [label = '']
  values -> types [label = '']
  types -> signals [label = '']
  signals -> connect [label = '']
  connect -> alignement [label = '']
  
  }
")

# Display the diagram
flow_diagram

DiagrammeR::export_graph(flow_diagram, file_name = "flow_diagram.png", 
                         file_type = "png",
                         width = cm_to_pixels(10),)


# Alternative version with more detailed styling
temp_diagram <- grViz("
  digraph detailed_circular_ea {
    
    # Graph layout - using neato to respect pos coordinates
    graph [layout = neato,
           overlap = false,
           splines = curved,
           bgcolor = 'transparent',
           pad = 0.5]
    
    # Node styling
    node [shape = ellipse,
          style = 'filled,bold',
          fontname = 'Helvetica',
          fontsize = 16,
          fontcolor = 'white',
          width = 2.2,
          height = 1.4]
    
    # Edge styling
    edge [fontname = 'Helvetica',
          fontsize = 9,
          color = '#34495E',
          arrowsize = 1.5,
          penwidth = 2]
    
    # Nodes with enhanced styling - alignment at top
    alignment [label = 'Perceived\nAlignment &\nMisalignment\nInference', 
               fillcolor = '#E74C3C',
               pos = '0,2!']
    
    updating [label = 'Value\nUpdating', 
              fillcolor = '#F39C12',
              pos = '2,0.8!']
    
    recruitment [label = 'EA\nRecruitment', 
                 fillcolor = '#27AE60',
                 pos = '1,-1.5!']
    
    desertion [label = 'EA\nDesertion', 
               fillcolor = '#8E44AD',
               pos = '-1,-1.5!']
    
    signals [label = 'New Signal\nGeneration\n(Stochastically)', 
             fillcolor = '#2980B9',
             pos = '-2,0.8!']
    
    # Circular flow connections
    alignment -> updating [label = '']
    updating -> recruitment [label = '']
    recruitment -> desertion [label = '']
    desertion -> signals [label = '']
    signals -> alignment [label = '']
  }
")

temp_diagram
DiagrammeR::export_graph(temp_diagram, file_name = "temp_diagram.png", 
                         file_type = "png",
                         width = cm_to_pixels(10),)
