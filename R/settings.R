# Setting maximum size for .adg files
options(shiny.maxRequestSize=100*1024^2)

# Setting lists of inputs
sex <- c("...", "male", "female", "intersex", "undefined", "prefer not to say")
device <- c("...", "7164", "GT1M", "GT3X", "GT3X+", "wGT3X+", "wGT3X-BT", "GT9X")
position <- c("hip", "back")
side <- c("...", "right", "left")
filter <- c("...", "normal", "LFE")
axis_weartime <- c("vector magnitude", "vertical axis")
metrics <- c("axis1", "axis2", "axis3", "vm", "steps")
equations <- c("...",
               "Freedson et al. (1998) [Adults]",
               "Santos-Lozano et al. (2013) [Adults]",
               "Santos-Lozano et al. (2013) [Older adults]",
               "Sasaki et al. (2011) [Adults]"
               )
sed_cutpoint <- c("...", 
                   "Aguilar-Farias et al. (2014) [Older adults]", 
                   "Personalized...")
mvpa_cutpoint <- c("...", 
                   "Freedson et al. (1998) [Adults]",
                   "Santos-Lozano et al. (2013) [Adults]",
                   "Santos-Lozano et al. (2013) [Older adults]",
                   "Sasaki et al. (2011) [Adults]", 
                    "Personalized...")
perso_sed_axis <- c("vector magnitude", "vertical axis")
perso_mvpa_axis <- c("vector magnitude", "vertical axis")


