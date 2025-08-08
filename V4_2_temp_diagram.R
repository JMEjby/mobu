# Load required library
library(DiagrammeR)

cm_to_pixels <- function(cm, dpi = 600) {
  inches <- cm / 2.54  # Convert cm to inches
  pixels <- inches * dpi
  return(round(pixels))
}

# detailed 

## position coordinates
sapply(c(32.7*5, 32.7*4, 32.7*3, 32.7*2, 32.7, 0, 32.7*10, 32.7*9, 32.7*8, 32.7*7, 32.7*6), function(angle) {
  angle_radians <- angle * pi / 180  # Convert degrees to radians
  paste0(round(4 * sin(angle_radians), 2), ",", -1 * round(3 * cos(angle_radians), 2),"!")
})

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
          fontsize = 18,
          fontcolor = 'white',
          width = 2.2,
          height = 1.4]
    
    # Edge styling
    edge [fontname = 'Helvetica',
          fontsize = 9,
          color = '#34495E',
          arrowsize = 1.5,
          penwidth = 2]
    
    reassignment [label = 'Type\nReassignment', 
                  fillcolor = '#E74C3C',
                  pos = '0,4!']
                  
    aging [label = 'Increment Year &\nTime as Type', 
           fillcolor = '#E67E22',
           pos = '3.24,3.37!']
                  
    graduation [label = 'Year 4 \nStudent\nGraduation',
                 fillcolor = '#F39C12',
                 pos = '5.46,1.66!']
    
    election [label = 'EA Leader\nElection',
                fillcolor = '#F1C40F',
                pos = '5.94,-0.57!'] 
    
    induction [label = 'Year 1 \nStudent \nInduction',
              fillcolor = '#2ECC71',
              pos = '4.53,-2.62!']
    
    alignment [label = 'Perceived\nAlignment &\nMisalignment\nInference', 
               fillcolor = '#1ABC9C',
              pos = '1.71,-3.84!']
    
    updating [label = 'EA Value\nUpdating', 
              fillcolor = '#16A085',
              pos = '-1.71,-3.84!'] 
    
    recruitment [label = 'EA\nRecruitment', 
                 fillcolor = '#3498DB',
                 pos = '-4.53,-2.62!']
    
    desertion [label = 'EA\nDesertion', 
               fillcolor = '#2980B9',
               pos = '-5.94,-0.57!']
    
    signals [label = 'New Signal\nGeneration\n(Stochastically)', 
             fillcolor = '#8E44AD',
             pos = '-5.46,1.66!']
    
    reposition [label = 'Update\nLayout', 
                fillcolor = '#9B59B6',
                pos = '-3.24,3.37!']  
    
    # Circular flow connections
    reassignment -> aging [label = '']
    aging -> graduation [label = '']
    graduation -> election [label = '']
    election -> induction [label = '']
    induction -> alignment [label = '']
    alignment -> updating [label = '']
    updating -> recruitment [label = '']
    recruitment -> desertion [label = '']
    desertion -> signals [label = '']
    signals -> reposition [label = '']
    reposition -> reassignment [label = '']
  }
")

temp_diagram

# conceptual

## position coordinates
sapply(c(51.43*3, 51.43*2, 51.43, 0, 51.43*6, 51.43*5, 51.43*4), function(angle) {
  angle_radians <- angle * pi / 180  # Convert degrees to radians
  paste0(round(4 * sin(angle_radians), 2), ",", -1 * round(3 * cos(angle_radians), 2),"!")
})

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
          fontsize = 18,
          fontcolor = 'white',
          width = 2.2,
          height = 1.4]
    
    # Edge styling
    edge [fontname = 'Helvetica',
          fontsize = 9,
          color = '#34495E',
          arrowsize = 1.5,
          penwidth = 2]
   
   initialization [label = 'Model \nInitialisation',
                    fillcolor = '#34495E',  # Dark Gray
                    pos = '1.74, 5.2!'] 
   
   election [label = 'EA Leader\\nElection',
              fillcolor = '#E74C3C',    # Red
              pos = '1.74,2.7!']
    
    graduation [label = 'Student\\nGraduation &\\nInduction',
                fillcolor = '#E67E22',  # Orange
                pos = '3.9,0.67!'] 
    
    alignment [label = 'Perceived\\nAlignment &\\nMisalignment\\nInference', 
               fillcolor = '#F1C40F',   # Yellow
               pos = '3.13,-1.87!']
    
    updating [label = 'EA Value\\nUpdating', 
              fillcolor = '#27AE60',    # Green
              pos = '0,-3!'] 
    
    recruitment [label = 'EA\\nRecruitment', 
                 fillcolor = '#3498DB',  # Blue
                 pos = '-3.13,-1.87!']
    
    desertion [label = 'EA\\nDesertion', 
               fillcolor = '#8E44AD',   # Purple
               pos = '-3.9,0.67!']
    
    signals [label = 'New Signal\\nGeneration\\n(Stochastically)', 
             fillcolor = '#E91E63',    # Pink/Magenta
             pos = '-1.74,2.7!']
    
    
    # Circular flow connections
    initialization -> election
    election -> graduation [label = '']
    graduation -> alignment [label = '']
    alignment -> updating [label = '']
    updating -> recruitment [label = '']
    recruitment -> desertion [label = '']
    desertion -> signals [label = '']
    signals -> election [label = '']
  }
")
temp_diagram
