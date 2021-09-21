#Required libraries
library(gridExtra)
library(reshape2)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinythemes)
library(tidyverse)

#SERVER FUNCTION SECTION BEGINS HERE########################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#SERVER FUNCTION GENERAL SECTION############################################################################################
############################################################################################################################

# Set all parameter names for inputed conditions to pass to simulation function
sim_Names <- c("K_solid_lead_hydroxide", "K_solid_cerussite", "K_solid_hydrocerussite", "K_solid_hydroxypyromorphite",
               "K_solid_pyromorphite", "K_solid_primary_lead_ortho", "K_solid_secondary_lead_ortho", "K_solid_tertiary_lead_ortho",
               "K_solid_anglesite", "K_solid_laurionite",
               "B_1_OH", "B_2_OH", "B_3_OH", "B_4_OH", "B_2_1_OH", "B_3_4_OH", "B_4_4_OH", "B_6_8_OH",
               "K_1_Cl", "B_2_Cl", "B_3_Cl", "B_4_Cl",
               "K_s", "K_1_SO4", "B_2_SO4",
               "K_c_1", "K_c_2", "K_1_CO3", "K_2_CO3", "K_3_CO3",
               "K_p_1", "K_p_2", "K_p_3", "K_1_PO4", "K_2_PO4",
               "sim_type", "pH_single", "pH_range", "IS_mM_single", "IS_mM_range",
               "Cl_minus_mg_L_single", "Cl_minus_mg_L_range",
               "DIC_mg_L_single", "DIC_mg_L_range","TOTP_mg_L_single", "TOTP_mg_L_range",
               "TOTSO4_mg_L_single", "TOTSO4_mg_L_range", "solids_include")

# Define color schemes for plots

  # Molar plot color scheme
  palette_1 <- c(
    "#000000",
    "#CC79A7", "#E69F00", "#56B4E9", "#009E73",
    "#0072B2", "#D55E00", "#F0E442", "#999999",
    "#000000", "#CC79A7", "#E69F00", "#56B4E9",
    "#000000", "#CC79A7",
    "#000000", "#CC79A7", "#E69F00",
    "#000000", "#CC79A7",
#    "#000000", "#CC79A7", "#E69F00",
#    "#000000", "#CC79A7", "#E69F00", "#56B4E9",
    "#000000",
    "#CC79A7")

  # mg/L plot color scheme
  palette_2 <- c(
    "#000000",
    "#CC79A7", "#E69F00", "#56B4E9", "#009E73",
    "#0072B2", "#D55E00", "#F0E442", "#999999",
    "#000000", "#CC79A7", "#E69F00", "#56B4E9",
    "#000000", "#CC79A7",
    "#000000", "#CC79A7", "#E69F00",
    "#000000", "#CC79A7",
#    "#000000", "#CC79A7", "#E69F00",
#    "#000000", "#CC79A7", "#E69F00", "#56B4E9",
    "#000000",
    "#CC79A7")

# Define linetypes for plots

  # Molar plot linetypes
  linesets_1 <- c(
    1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    2, 2, 2, 2,
    3, 3,
    4, 4, 4,
    5, 5,
#    6, 6, 6,
#    7, 7, 7, 7,
    1,
    1)

  # mg/L plot linetypes
  linesets_2 <- c(
    1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    2, 2, 2, 2,
    3, 3,
    4, 4, 4,
    5, 5,
#    6, 6, 6,
#    7, 7, 7, 7,
    1,
    1)

# Define line sizes for plots

  # Molar plot line sizes
  linesize_1 <- c(
    1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1,
    1, 1, 1,
    1, 1,
#    1, 1, 1,
#    1, 1, 1, 1,
    1.5,
    1.5)

  # mg/L plot line sizes
  linesize_2 <- c(
    1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1, 1, 1,
    1, 1,
    1, 1, 1,
    1, 1,
#    1, 1, 1,
#    1, 1, 1, 1,
    1.5,
    1.5)

# Define formatted names for plot legends

  # Molar plot legend names
  series_1 <- c(expression(
    'Pb'^'2+',
    'PbOH'^'+',
    'Pb(OH)'[2]*"(aq)",
    'Pb(OH)'[3]^'-',
    'Pb(OH)'[4]^'2-',
    'Pb'[2]*'OH'^'3+',
    'Pb'[3]*'(OH)'[4]^'2+',
    'Pb'[4]*'(OH)'[4]^'4+',
    'Pb'[6]*'(OH)'[8]^'4+',
    'PbCl'^'+',
    'PbCl'[2]*"(aq)",
    'PbCl'[3]^'-',
    'PbCl'[4]^'2-',
    'PbSO'[4]*"(aq)",
    'Pb(SO'[4]*')'[2]^'2-',
    'PbHCO'[3]^'+',
    'PbCO'[3]*"(aq)",
    'Pb(CO'[3]*')'[2]^'2-',
    'PbHPO'[4]*"(aq)",
    'PbH'[2]*'PO'[4]^'+',
#    'H'[2]*'CO'[3],
#    'HCO'[3]^'-',
#    'CO'[3]^'2-',
#    'H'[3]*'PO'[4],
#    'H'[2]*'PO'[4]^'-',
#    'HPO'[4]^'2-',
#    'PO'[4]^'3-',
    'Total'~'Soluble'~'Lead',
    'Action'~'Level')
    )

  # mg/L plot legend names
  series_2 <- c(expression(
    'Pb'^'2+',
    'PbOH'^'+',
    'Pb(OH)'[2]*"(aq)",
    'Pb(OH)'[3]^'-',
    'Pb(OH)'[4]^'2-',
    'Pb'[2]*'OH'^'3+',
    'Pb'[3]*'(OH)'[4]^'2+',
    'Pb'[4]*'(OH)'[4]^'4+',
    'Pb'[6]*'(OH)'[8]^'4+',
    'PbCl'^'+',
    'PbCl'[2]*"(aq)",
    'PbCl'[3]^'-',
    'PbCl'[4]^'2-',
    'PbSO'[4]*"(aq)",
    'Pb(SO'[4]*')'[2]^'2-',
    'PbHCO'[3]^'+',
    'PbCO'[3]*"(aq)",
    'Pb(CO'[3]*')'[2]^'2-',
    'PbHPO'[4]*"(aq)",
    'PbH'[2]*'PO'[4]^'+',
#    'H'[2]*'CO'[3],
#    'HCO'[3]^'-',
#    'CO'[3]^'2-',
#    'H'[3]*'PO'[4],
#    'H'[2]*'PO'[4]^'-',
#    'HPO'[4]^'2-',
#    'PO'[4]^'3-',
    'Total'~'Soluble'~'Lead',
    'Action'~'Level')
    )

# Set list of solid names
solids <- c("Lead Hydroxide",
            "Cerussite",
            "Hydrocerussite",
            "Hydroxypyromorphite",
            "Pyromorphite",
            "Primary Lead Orthophosphate",
            "Secondary Lead Orthophosphate",
            "Tertiary Lead Orthophosphate",
            "Anglesite",
            "Laurionite")

# Define basic theme used for all plots
mytheme <-  theme(
  panel.background = element_rect(fill = "white", colour = NA),
  panel.grid.major = element_line(colour = "grey70", size = 0.2),
  panel.grid.minor = element_line(colour = "grey85", size = 0.5),
  legend.background = element_blank(),
  legend.key = element_blank(),
  legend.key.size = unit (2, "cm"),
  legend.key.height = unit (1.3, "line"),
  legend.title = element_blank(),
  legend.text = element_text(size = rel(0.9)),
  legend.position = "right",
  legend.direction = "vertical",
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = rel (1.5)),
  axis.ticks = element_line(colour = "black", size = 1),
  axis.line = element_line(colour = "black", size = 1, lineend = "square"),
  axis.text.x = element_text(colour = "black", size = 12),
  axis.text.y = element_text(colour = "black", size = 12),
  axis.title.x = element_text(size = 14),
  axis.title.y = element_text(size = 14)
  )

# Set molecular weights
Pb_MW <- 207.2
chloride_MW <- 35.453
sulfate_MW <- 32.065 + 4 * 15.999
DIC_MW <- 12.011
phosphate_MW <- 94.97

# Create function to copy simulation conditions from one simulation to another for general simulation conditions
update_IC <- function(session, from, to, input) {

   # Update simulation type selected
  updateRadioButtons(session, paste0(to, "_", "sim_type"), selected = input[[paste0(from, "_", "sim_type")]])

  # Update simulation condition for slider inputs
  updateSliderInput(session, paste0(to, "_", "pH_single"), value = input[[paste0(from, "_", "pH_single")]])
  updateSliderInput(session, paste0(to, "_", "pH_range"), value = input[[paste0(from, "_", "pH_range")]])
  updateSliderInput(session, paste0(to, "_", "IS_mM_single"), value = input[[paste0(from, "_", "IS_mM_single")]])
  updateSliderInput(session, paste0(to, "_", "IS_mM_range"), value = input[[paste0(from, "_", "IS_mM_range")]])
  updateSliderInput(session, paste0(to, "_", "Cl_minus_mg_L_single"), value = input[[paste0(from, "_", "Cl_minus_mg_L_single")]])
  updateSliderInput(session, paste0(to, "_", "Cl_minus_mg_L_range"), value = input[[paste0(from, "_", "Cl_minus_mg_L_range")]])
  updateSliderInput(session, paste0(to, "_", "TOTSO4_mg_L_single"), value = input[[paste0(from, "_", "TOTSO4_mg_L_single")]])
  updateSliderInput(session, paste0(to, "_", "TOTSO4_mg_L_range"), value = input[[paste0(from, "_", "TOTSO4_mg_L_range")]])
  updateSliderInput(session, paste0(to, "_", "DIC_mg_L_single"), value = input[[paste0(from, "_", "DIC_mg_L_single")]])
  updateSliderInput(session, paste0(to, "_", "DIC_mg_L_range"), value = input[[paste0(from, "_", "DIC_mg_L_range")]])
  updateSliderInput(session, paste0(to, "_", "TOTP_mg_L_single"), value = input[[paste0(from, "_", "TOTP_mg_L_single")]])
  updateSliderInput(session, paste0(to, "_", "TOTP_mg_L_range"), value = input[[paste0(from, "_", "TOTP_mg_L_range")]])

  # Update simulation condition for solids to include in analysis
  updateCheckboxGroupInput(session, paste0(to, "_", "solids_include"), selected = input[[paste0(from, "_", "solids_include")]])
}

# Create function to copy simulation conditions from one simulation to another for equilibrium constants
update_IC_EQ <- function(session, from, to, input) {

  # Update simulation condition for solid constants
  updateRadioButtons(session, paste0(to, "_", "K_solid_lead_hydroxide"), selected = input[[paste0(from, "_", "K_solid_lead_hydroxide")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_cerussite"), selected = input[[paste0(from, "_", "K_solid_cerussite")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_hydrocerussite"), selected = input[[paste0(from, "_", "K_solid_hydrocerussite")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_hydroxypyromorphite"), selected = input[[paste0(from, "_", "K_solid_hydroxypyromorphite")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_pyromorphite"), selected = input[[paste0(from, "_", "K_solid_pyromorphite")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_primary_lead_ortho"), selected = input[[paste0(from, "_", "K_solid_primary_lead_ortho")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_secondary_lead_ortho"), selected = input[[paste0(from, "_", "K_solid_secondary_lead_ortho")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_tertiary_lead_ortho"), selected = input[[paste0(from, "_", "K_solid_tertiary_lead_ortho")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_anglesite"), selected = input[[paste0(from, "_", "K_solid_anglesite")]])
  updateRadioButtons(session, paste0(to, "_", "K_solid_laurionite"), selected = input[[paste0(from, "_", "K_solid_laurionite")]])

  # Update simulation condition for hydroxide constants
  updateRadioButtons(session, paste0(to, "_", "B_1_OH"), selected = input[[paste0(from, "_", "B_1_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_2_OH"), selected = input[[paste0(from, "_", "B_2_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_3_OH"), selected = input[[paste0(from, "_", "B_3_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_4_OH"), selected = input[[paste0(from, "_", "B_4_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_2_1_OH"), selected = input[[paste0(from, "_", "B_2_1_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_3_4_OH"), selected = input[[paste0(from, "_", "B_3_4_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_4_4_OH"), selected = input[[paste0(from, "_", "B_4_4_OH")]])
  updateRadioButtons(session, paste0(to, "_", "B_6_8_OH"), selected = input[[paste0(from, "_", "B_6_8_OH")]])

  # Update simulation condition for chloride constants
  updateRadioButtons(session, paste0(to, "_", "K_1_Cl"), selected = input[[paste0(from, "_", "K_1_Cl")]])
  updateRadioButtons(session, paste0(to, "_", "B_2_Cl"), selected = input[[paste0(from, "_", "B_2_Cl")]])
  updateRadioButtons(session, paste0(to, "_", "B_3_Cl"), selected = input[[paste0(from, "_", "B_3_Cl")]])
  updateRadioButtons(session, paste0(to, "_", "B_4_Cl"), selected = input[[paste0(from, "_", "B_4_Cl")]])

  # Update simulation condition for sulfate constants
  updateRadioButtons(session, paste0(to, "_", "K_s"), selected = input[[paste0(from, "_", "K_s")]])
  updateRadioButtons(session, paste0(to, "_", "K_1_SO4"), selected = input[[paste0(from, "_", "K_1_SO4")]])
  updateRadioButtons(session, paste0(to, "_", "B_2_SO4"), selected = input[[paste0(from, "_", "B_2_SO4")]])

  # Update simulation condition for carbonate constants
  updateRadioButtons(session, paste0(to, "_", "K_c_1"), selected = input[[paste0(from, "_", "K_c_1")]])
  updateRadioButtons(session, paste0(to, "_", "K_c_2"), selected = input[[paste0(from, "_", "K_c_2")]])
  updateRadioButtons(session, paste0(to, "_", "K_1_CO3"), selected = input[[paste0(from, "_", "K_1_CO3")]])
  updateRadioButtons(session, paste0(to, "_", "K_2_CO3"), selected = input[[paste0(from, "_", "K_2_CO3")]])
  updateRadioButtons(session, paste0(to, "_", "K_3_CO3"), selected = input[[paste0(from, "_", "K_3_CO3")]])

  # Update simulation condition for phosphate constants
  updateRadioButtons(session, paste0(to, "_", "K_p_1"), selected = input[[paste0(from, "_", "K_p_1")]])
  updateRadioButtons(session, paste0(to, "_", "K_p_2"), selected = input[[paste0(from, "_", "K_p_2")]])
  updateRadioButtons(session, paste0(to, "_", "K_p_3"), selected = input[[paste0(from, "_", "K_p_3")]])
  updateRadioButtons(session, paste0(to, "_", "K_1_PO4"), selected = input[[paste0(from, "_", "K_1_PO4")]])
  updateRadioButtons(session, paste0(to, "_", "K_2_PO4"), selected = input[[paste0(from, "_", "K_2_PO4")]])
}

# Define function to simulate lead solubility
simulate_solubility <- function(K_solid_lead_hydroxide, K_solid_cerussite, K_solid_hydrocerussite, K_solid_hydroxypyromorphite,
                                K_solid_pyromorphite, K_solid_primary_lead_ortho, K_solid_secondary_lead_ortho, K_solid_tertiary_lead_ortho,
                                K_solid_anglesite, K_solid_laurionite,
                                B_1_OH, B_2_OH, B_3_OH, B_4_OH, B_2_1_OH, B_3_4_OH, B_4_4_OH, B_6_8_OH,
                                K_1_Cl, B_2_Cl, B_3_Cl, B_4_Cl,
                                K_s, K_1_SO4, B_2_SO4,
                                K_c_1, K_c_2, K_1_CO3, K_2_CO3, K_3_CO3,
                                K_p_1, K_p_2, K_p_3, K_1_PO4, K_2_PO4,
                                sim_type, pH_single, pH_range, IS_mM_single, IS_mM_range,
                                Cl_minus_mg_L_single, Cl_minus_mg_L_range,
                                DIC_mg_L_single, DIC_mg_L_range, TOTP_mg_L_single, TOTP_mg_L_range,
                                TOTSO4_mg_L_single, TOTSO4_mg_L_range, solids_include) {

  # Simulation calculations

    # Convert user selected concentrations to molar concentrations
    IS_M_single <- IS_mM_single / 1000
    IS_M_range <- IS_mM_range / 1000
    Cl_minus_M_single <- Cl_minus_mg_L_single / chloride_MW / 1000
    Cl_minus_M_range <- Cl_minus_mg_L_range / chloride_MW / 1000
    TOTSO4_M_single <- TOTSO4_mg_L_single / sulfate_MW / 1000
    TOTSO4_M_range <- TOTSO4_mg_L_range / sulfate_MW / 1000
    DIC_M_single <- DIC_mg_L_single / DIC_MW / 1000
    DIC_M_range <- DIC_mg_L_range / DIC_MW / 1000
    TOTP_M_single <- TOTP_mg_L_single / phosphate_MW / 1000
    TOTP_M_range <- TOTP_mg_L_range / phosphate_MW / 1000

    # Set pH, DIC, phosphate, sulfate, ionic strength, and chloride per user selections
    if (sim_type == "multi_pH") {
      pH <- seq(pH_range[1], pH_range[2], 0.1)
      DIC <- rep(DIC_M_single, length(pH))
      TOTP <- rep(TOTP_M_single, length(pH))
      TOTSO4 <- rep(TOTSO4_M_single,length(pH))
      IS <- rep(IS_M_single,length(pH))
      Cl_minus <- rep(Cl_minus_M_single,length(pH))
    } else if (sim_type == "multi_DIC") {
      DIC <- seq(DIC_M_range[1], DIC_M_range[2], 1 / DIC_MW / 1000)
      pH <- rep(pH_single, length(DIC))
      TOTP <- rep(TOTP_M_single, length(DIC))
      TOTSO4 <- rep(TOTSO4_M_single,length(DIC))
      IS <- rep(IS_M_single,length(DIC))
      Cl_minus <- rep(Cl_minus_M_single,length(DIC))
    } else if (sim_type == "multi_phosphate") {
      TOTP <- seq(TOTP_M_range[1], TOTP_M_range[2], 0.1 / phosphate_MW / 1000)
      pH <- rep(pH_single, length(TOTP))
      DIC <- rep(DIC_M_single, length(TOTP))
      TOTSO4 <- rep(TOTSO4_M_single,length(TOTP))
      IS <- rep(IS_M_single,length(TOTP))
      Cl_minus <- rep(Cl_minus_M_single,length(TOTP))
    } else if (sim_type == "multi_sulfate") {
      TOTSO4 <- seq(TOTSO4_M_range[1], TOTSO4_M_range[2], 10 / sulfate_MW / 1000)
      pH <- rep(pH_single, length(TOTSO4))
      DIC <- rep(DIC_M_single, length(TOTSO4))
      TOTP <- rep(TOTP_M_single, length(TOTSO4))
      IS <- rep(IS_M_single,length(TOTSO4))
      Cl_minus <- rep(Cl_minus_M_single,length(TOTSO4))
    } else if (sim_type == "multi_IS") {
      IS <- seq(IS_M_range[1], IS_M_range[2], 1 / 1000)
      pH <- rep(pH_single, length(IS))
      DIC <- rep(DIC_M_single, length(IS))
      TOTSO4 <- rep(TOTSO4_M_single,length(IS))
      TOTP <- rep(TOTP_M_single, length(IS))
      Cl_minus <- rep(Cl_minus_M_single,length(IS))
    } else if (sim_type == "multi_chloride") {
      Cl_minus <- seq(Cl_minus_M_range[1], Cl_minus_M_range[2], 10 / chloride_MW / 1000)
      pH <- rep(pH_single, length(Cl_minus))
      DIC <- rep(DIC_M_single, length(Cl_minus))
      TOTSO4 <- rep(TOTSO4_M_single,length(Cl_minus))
      TOTP <- rep(TOTP_M_single, length(Cl_minus))
      IS <- rep(IS_M_single,length(Cl_minus))
    } else if (sim_type == "single") {
      pH <- c(pH_single, pH_single + 0.00001)
      DIC <- rep(DIC_M_single, length(pH))
      TOTP <- rep(TOTP_M_single, length(pH))
      TOTSO4 <- rep(TOTSO4_M_single,length(pH))
      IS <- rep(IS_M_single,length(pH))
      Cl_minus <- rep(Cl_minus_M_single,length(pH))
    }

    # Calculate hydrogen ion activity
    H_plus_a <- 10^-pH

    # Calculate the number of conditions based on pH sequence length
    conditions <- length(pH)

    # Set temperature in Celcius
    T_C <- 25

    # Convert temperature to Kelvin
    T_K <- T_C + 273.15

    # Activity calculations
    epsilon <- 87.74 - 0.4008 * T_C + T_C^2 * 0.0009398 - T_C^3 * 0.00000141

    A <- 1824830 * (epsilon * T_K)^-1.5

    gamma_1 <- 10^(-A * 1^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
    gamma_2 <- 10^(-A * 2^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
    gamma_3 <- 10^(-A * 3^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))
    gamma_4 <- 10^(-A * 4^2 * (IS^0.5 / (1 + IS^0.5) - 0.3 * IS))

    # Calculations for carbonate acid-base species

      # Correct constants for ionic strength
      K_c_1_c <- as.numeric(K_c_1) / gamma_1
      K_c_2_c <- gamma_1 * as.numeric(K_c_2) / gamma_2

      # Calculate carbonate alpha values
      alpha_0_c <- 1 / (1 + K_c_1_c / H_plus_a + K_c_1_c * K_c_2_c / H_plus_a^2)
      alpha_1_c <- 1 / (H_plus_a / K_c_1_c + 1 + K_c_2_c / H_plus_a)
      alpha_2_c <- 1 / (H_plus_a^2 / (K_c_1_c * K_c_2_c) + H_plus_a / K_c_2_c + 1)

      # Calculate carbonate species concentrations
      H2CO3 <- alpha_0_c * DIC
      HCO3_minus <- alpha_1_c * DIC
      CO3_2_minus <- alpha_2_c * DIC

    # Calculations for phosphate acid-base species

      # Correct constants for ionic strength
      K_p_1_c <- as.numeric(K_p_1) / gamma_1
      K_p_2_c <- gamma_1 * as.numeric(K_p_2) / gamma_2
      K_p_3_c <- gamma_2 * as.numeric(K_p_3) / gamma_3

      # Calculate phosphate alpha values
      alpha_0_p <- 1 / (1 + K_p_1_c / H_plus_a + K_p_1_c * K_p_2_c / H_plus_a^2 + K_p_1_c * K_p_2_c * K_p_3_c / H_plus_a^3)
      alpha_1_p <- 1 / (H_plus_a / K_p_1_c + 1 + K_p_2_c / H_plus_a + K_p_2_c * K_p_3_c / H_plus_a^2)
      alpha_2_p <- 1 / (H_plus_a^2 / K_p_1_c * K_p_2_c + H_plus_a / K_p_2_c + 1 + K_p_3_c / H_plus_a)
      alpha_3_p <- 1 / (H_plus_a^3 / (K_p_1_c * K_p_2_c * K_p_3_c) + H_plus_a^2 / (K_p_2_c * K_p_3_c) + H_plus_a / K_p_3_c + 1)

      # Calculate phosphate species concentrations
      H3PO4 <- alpha_0_p * TOTP
      H2PO4_minus <- alpha_1_p * TOTP
      HPO4_2_minus <- alpha_2_p * TOTP
      PO4_3_minus <- alpha_3_p * TOTP

    # Calculations for sulfate acid-base species

      # Correct constants for ionic strength
      K_s_c <- gamma_1 * as.numeric(K_s) / gamma_2

      # Calculate sulfate alpha values
      alpha_0_s <- 1 / (1 + K_s_c / H_plus_a)
      alpha_1_s <- 1 / (H_plus_a / K_s_c + 1)

      # Calculate sulfate species concentrations
      HSO4_minus <- alpha_0_s * TOTSO4
      SO4_2_minus <- alpha_1_s * TOTSO4

    # Calculate lead solid solubility based on controlling solid

      #Set constants for each solid

      # Lead Hydroxide: Pb(OH)2(s) + 2H+ --> Pb2+ + 2H2O
      Pb_2_plus_lead_hydroxide <- as.numeric(K_solid_lead_hydroxide) * H_plus_a^2 / gamma_2

      # Cerussite: PbCO3(s) --> Pb2+ + CO32-
      Pb_2_plus_cerussite <- as.numeric(K_solid_cerussite) / (gamma_2^2 * CO3_2_minus)

      # Hydrocerussite: Pb3(CO3)2(OH)2(s) + 2H+ --> 3Pb2+ + 2CO32- + 2H2O
      Pb_2_plus_hydrocerussite <- (as.numeric(K_solid_hydrocerussite) * H_plus_a^2 / (gamma_2^5 * CO3_2_minus^2))^(1/3)

      # Hydroxypyromorphite: Pb5(PO4)3OH(s) + H+ --> 5Pb2+ + 3PO43- + H2O
      Pb_2_plus_hydroxypyromorphite <- (as.numeric(K_solid_hydroxypyromorphite) * H_plus_a / (gamma_2^5 * gamma_3^3 * PO4_3_minus^3))^(1/5)

      # Pyromorphite: Pb5(PO4)3Cl(s) --> 5Pb2+ + 3PO43- + Cl-
      Pb_2_plus_pyromorphite <- (as.numeric(K_solid_pyromorphite) / (gamma_1 * gamma_2^5 * gamma_3^3 * PO4_3_minus^3 * Cl_minus))^(1/5)

      # Primary Lead Orthophosphate: Pb(H2PO4)2(s) --> Pb2+ + 2PO43- + 4H+
      Pb_2_plus_primary_lead_ortho <- as.numeric(K_solid_primary_lead_ortho) / (gamma_2 * gamma_3^2 * PO4_3_minus^2 * H_plus_a^4)

      # Secondary Lead Orthophosphate: PbHPO4(s) --> Pb2+ + PO43- + H+
      Pb_2_plus_secondary_lead_ortho <- as.numeric(K_solid_secondary_lead_ortho) / (gamma_2 * gamma_3 * PO4_3_minus * H_plus_a)

      # Tertiary Lead Orthophosphate: Pb3(PO4)2(s) --> 3Pb2+ + 2PO43- + H+
      Pb_2_plus_tertiary_lead_ortho <- (as.numeric(K_solid_tertiary_lead_ortho) / (gamma_2^3 * gamma_3^2 * PO4_3_minus^2))^(1/3)

      # Anglesite: PbSO4(s) --> Pb2+ + SO42-
      Pb_2_plus_anglesite <- as.numeric(K_solid_anglesite) / (gamma_2^2 * SO4_2_minus)

      # Laurionite: PbClOH(s) + H+ --> Pb2+ + Cl- + H2O
      Pb_2_plus_laurionite <- as.numeric(K_solid_laurionite) * H_plus_a / (gamma_2 * gamma_1 * Cl_minus)

  #Create list of lead 2+ concentrations in formatted data frame for each controlling solid
  Pb_2_plus <- c(Pb_2_plus_lead_hydroxide,
                 Pb_2_plus_cerussite,
                 Pb_2_plus_hydrocerussite,
                 Pb_2_plus_hydroxypyromorphite,
                 Pb_2_plus_pyromorphite,
                 Pb_2_plus_primary_lead_ortho,
                 Pb_2_plus_secondary_lead_ortho,
                 Pb_2_plus_tertiary_lead_ortho,
                 Pb_2_plus_anglesite,
                 Pb_2_plus_laurionite)

  Pb_2_plus_solid <- as.data.frame(lapply(solids, function(x) rep(x,conditions)))

  colnames(Pb_2_plus_solid) <- solids

  Pb_2_plus_solid$pH <- pH
  Pb_2_plus_solid$DIC_mg_L <- DIC * DIC_MW * 1000
  Pb_2_plus_solid$TOTP_mg_L <- TOTP * phosphate_MW * 1000
  Pb_2_plus_solid$TOTSO4_mg_L <- TOTSO4 * sulfate_MW * 1000
  Pb_2_plus_solid$IS_mM <- IS * 1000
  Pb_2_plus_solid$Cl_minus_mg_L <- Cl_minus * chloride_MW * 1000

  Pb_conc <- melt(Pb_2_plus_solid, id.vars = c("pH", "DIC_mg_L", "TOTP_mg_L", "TOTSO4_mg_L", "IS_mM", "Cl_minus_mg_L"), variable.name = "lead_solid")
  Pb_conc <- subset(Pb_conc, select = c(pH, DIC_mg_L, TOTP_mg_L, TOTSO4_mg_L, IS_mM, Cl_minus_mg_L, lead_solid))

  Pb_conc$analysis <- Pb_conc$lead_solid
  Pb_conc$Pb_2_plus <- Pb_2_plus

  # Calculation of complex concentrations

  # Calculate lead-hydroxide complex concentrations
  Pb_conc$PbOH_plus <- as.numeric(B_1_OH) * gamma_2 * Pb_conc$Pb_2_plus / (gamma_1 * H_plus_a)
  Pb_conc$PbOH2 <- as.numeric(B_2_OH) * gamma_2 * Pb_conc$Pb_2_plus / H_plus_a^2
  Pb_conc$PbOH3_minus <- as.numeric(B_3_OH) * gamma_2 * Pb_conc$Pb_2_plus / (gamma_1 * H_plus_a^3)
  Pb_conc$PbOH4_2_minus <- as.numeric(B_4_OH) * Pb_conc$Pb_2_plus / H_plus_a^4
  Pb_conc$Pb2OH_3_plus <- as.numeric(B_2_1_OH) * gamma_2^2 * Pb_conc$Pb_2_plus^2 / (gamma_3 * H_plus_a)
  Pb_conc$Pb3OH4_2_plus <- as.numeric(B_3_4_OH) * gamma_2^2 * Pb_conc$Pb_2_plus^3 / H_plus_a^4
  Pb_conc$Pb4OH4_4_plus <- as.numeric(B_4_4_OH) * gamma_2^4 * Pb_conc$Pb_2_plus^4 / (gamma_4 * H_plus_a^4)
  Pb_conc$Pb6OH8_4_plus <- as.numeric(B_6_8_OH) * gamma_2^6 * Pb_conc$Pb_2_plus^6 / (gamma_4 * H_plus_a^8)

  # Calculate lead-chloride complex concentrations
  Pb_conc$PbCl_plus <- as.numeric(K_1_Cl) * gamma_2 * Pb_conc$Pb_2_plus * Cl_minus
  Pb_conc$PbCl2 <- as.numeric(B_2_Cl) * gamma_2 * Pb_conc$Pb_2_plus * gamma_1^2 * Cl_minus^2
  Pb_conc$PbCl3_minus <- as.numeric(B_3_Cl) * gamma_2 * Pb_conc$Pb_2_plus * gamma_1^2 * Cl_minus^3
  Pb_conc$PbCl4_2_minus <- as.numeric(B_4_Cl) * Pb_conc$Pb_2_plus * gamma_1^4 * Cl_minus^4

  # Calculate lead-sulfate complex concentrations
  Pb_conc$PbSO4 <- as.numeric(K_1_SO4) * gamma_2^2 * Pb_conc$Pb_2_plus * SO4_2_minus
  Pb_conc$PbSO42_2_minus <- as.numeric(B_2_SO4) * gamma_2^2 * Pb_conc$Pb_2_plus * SO4_2_minus^2

  # Calculate lead-carbonate complex concentrations
  Pb_conc$PbHCO3_plus <- (as.numeric(K_1_CO3) * H_plus_a * gamma_2^2 * Pb_conc$Pb_2_plus * CO3_2_minus) / gamma_1
  Pb_conc$PbCO3 <- as.numeric(K_2_CO3) * gamma_2^2 * Pb_conc$Pb_2_plus * CO3_2_minus
  Pb_conc$PbCO32_2_minus <- as.numeric(K_3_CO3) * gamma_2^2 * Pb_conc$Pb_2_plus * CO3_2_minus^2

  # Calculate lead-phosphate complex concentrations
  Pb_conc$PbHPO4 <- as.numeric(K_1_PO4) * H_plus_a * gamma_2 * gamma_3 * Pb_conc$Pb_2_plus * PO4_3_minus
  Pb_conc$PbH2PO4_plus <- as.numeric(K_2_PO4) * H_plus_a^2 * gamma_2 * gamma_3 * Pb_conc$Pb_2_plus * PO4_3_minus / gamma_1

  # Calculate total dissolved lead molar concentration
  Pb_conc$TOTSOLPb <- Pb_conc$Pb_2_plus +
    Pb_conc$PbOH_plus + Pb_conc$PbOH2 + Pb_conc$PbOH3_minus + Pb_conc$PbOH4_2_minus +
    2 * Pb_conc$Pb2OH_3_plus + 3 * Pb_conc$Pb3OH4_2_plus + 4 * Pb_conc$Pb4OH4_4_plus + 6 * Pb_conc$Pb6OH8_4_plus +
    Pb_conc$PbCl_plus + Pb_conc$PbCl2 + Pb_conc$PbCl3_minus + Pb_conc$PbCl4_2_minus +
    Pb_conc$PbSO4 + Pb_conc$PbSO42_2_minus +
    Pb_conc$PbHCO3_plus + Pb_conc$PbCO3 + Pb_conc$PbCO32_2_minus +
    Pb_conc$PbHPO4 + Pb_conc$PbH2PO4_plus

  # Lead Action Level
  Pb_conc$action_level_mg_L <- rep(0.015, conditions)
  Pb_conc$action_level <- Pb_conc$action_level_mg_L / 1000 / Pb_MW

  # Calculate log molar concentrations
  Pb_conc$log_Pb_2_plus       <- log10(Pb_conc$Pb_2_plus)
  Pb_conc$log_PbOH_plus       <- log10(Pb_conc$PbOH_plus)
  Pb_conc$log_PbOH2           <- log10(Pb_conc$PbOH2)
  Pb_conc$log_PbOH3_minus     <- log10(Pb_conc$PbOH3_minus)
  Pb_conc$log_PbOH4_2_minus   <- log10(Pb_conc$PbOH4_2_minus)
  Pb_conc$log_Pb2OH_3_plus    <- log10(Pb_conc$Pb2OH_3_plus)
  Pb_conc$log_Pb3OH4_2_plus   <- log10(Pb_conc$Pb3OH4_2_plus)
  Pb_conc$log_Pb4OH4_4_plus   <- log10(Pb_conc$Pb4OH4_4_plus)
  Pb_conc$log_Pb6OH8_4_plus   <- log10(Pb_conc$Pb6OH8_4_plus)
  Pb_conc$log_PbCl_plus       <- log10(Pb_conc$PbCl_plus)
  Pb_conc$log_PbCl2           <- log10(Pb_conc$PbCl2)
  Pb_conc$log_PbCl3_minus     <- log10(Pb_conc$PbCl3_minus)
  Pb_conc$log_PbCl4_2_minus   <- log10(Pb_conc$PbCl4_2_minus)
  Pb_conc$log_PbSO4           <- log10(Pb_conc$PbSO4)
  Pb_conc$log_PbSO42_2_minus  <- log10(Pb_conc$PbSO42_2_minus)
  Pb_conc$log_PbHCO3_plus     <- log10(Pb_conc$PbHCO3_plus)
  Pb_conc$log_PbCO3           <- log10(Pb_conc$PbCO3)
  Pb_conc$log_PbCO32_2_minus   <- log10(Pb_conc$PbCO32_2_minus)
  Pb_conc$log_PbHPO4          <- log10(Pb_conc$PbHPO4)
  Pb_conc$log_PbH2PO4_plus    <- log10(Pb_conc$PbH2PO4_plus)
  #  Pb_conc$log_H2CO3        <- log10(Pb_conc$H2CO3)
  #  Pb_conc$log_HCO3_minus   <- log10(Pb_conc$HCO3_minus)
  #  Pb_conc$log_CO3_2_minus  <- log10(Pb_conc$CO3_2_minus)
  #  Pb_conc$log_H3PO4        <- log10(Pb_conc$H3PO4)
  #  Pb_conc$log_H2PO4_minus  <- log10(Pb_conc$H2PO4_minus)
  #  Pb_conc$log_HPO4_2_minus <- log10(Pb_conc$HPO4_2_minus)
  #  Pb_conc$log_PO4_3_minus  <- log10(Pb_conc$PO4_3_minus)
  Pb_conc$log_TOTSOLPb           <- log10(Pb_conc$TOTSOLPb)
  Pb_conc$log_action_level    <- log10(Pb_conc$action_level)

  # Calculate log mg lead/L concentrations
  Pb_conc$log_Pb_2_plus_mg_L        <- log10(Pb_conc$Pb_2_plus * 1000 * Pb_MW)
  Pb_conc$log_PbOH_plus_mg_L        <- log10(Pb_conc$PbOH_plus * 1000 * Pb_MW)
  Pb_conc$log_PbOH2_mg_L            <- log10(Pb_conc$PbOH2 * 1000 * Pb_MW)
  Pb_conc$log_PbOH3_minus_mg_L      <- log10(Pb_conc$PbOH3_minus * 1000 * Pb_MW)
  Pb_conc$log_PbOH4_2_minus_mg_L    <- log10(Pb_conc$PbOH4_2_minus * 1000 * Pb_MW)
  Pb_conc$log_Pb2OH_3_plus_mg_L     <- log10(Pb_conc$Pb2OH_3_plus * 1000 * Pb_MW * 2)
  Pb_conc$log_Pb3OH4_2_plus_mg_L    <- log10(Pb_conc$Pb3OH4_2_plus * 1000 * Pb_MW * 3)
  Pb_conc$log_Pb4OH4_4_plus_mg_L    <- log10(Pb_conc$Pb4OH4_4_plus * 1000 * Pb_MW * 4)
  Pb_conc$log_Pb6OH8_4_plus_mg_L    <- log10(Pb_conc$Pb6OH8_4_plus  * 1000 * Pb_MW * 6)
  Pb_conc$log_PbCl_plus_mg_L        <- log10(Pb_conc$PbCl_plus * 1000 * Pb_MW)
  Pb_conc$log_PbCl2_mg_L            <- log10(Pb_conc$PbCl2 * 1000 * Pb_MW)
  Pb_conc$log_PbCl3_minus_mg_L      <- log10(Pb_conc$PbCl3_minus * 1000 * Pb_MW)
  Pb_conc$log_PbCl4_2_minus_mg_L    <- log10(Pb_conc$PbCl4_2_minus * 1000 * Pb_MW)
  Pb_conc$log_PbSO4_mg_L            <- log10(Pb_conc$PbSO4 * 1000 * Pb_MW)
  Pb_conc$log_PbSO42_2_minus_mg_L   <- log10(Pb_conc$PbSO42_2_minus * 1000 * Pb_MW)
  Pb_conc$log_PbHCO3_plus_mg_L      <- log10(Pb_conc$PbHCO3_plus * 1000 * Pb_MW)
  Pb_conc$log_PbCO3_mg_L            <- log10(Pb_conc$PbCO3 * 1000 * Pb_MW)
  Pb_conc$log_PbCO32_2_minus_mg_L    <- log10(Pb_conc$PbCO32_2_minus * 1000 * Pb_MW)
  Pb_conc$log_PbHPO4_mg_L           <- log10(Pb_conc$PbHPO4 * 1000 * Pb_MW)
  Pb_conc$log_PbH2PO4_plus_mg_L     <- log10(Pb_conc$PbH2PO4_plus * 1000 * Pb_MW)
  #  Pb_conc$log_H2CO3_mg_L         <- log10(Pb_conc$H2CO3 * 1000 * DIC_MW)
  #  Pb_conc$log_HCO3_minus_mg_L    <- log10(Pb_conc$HCO3_minus * 1000 * DIC_MW)
  #  Pb_conc$log_CO3_2_minus_mg_L   <- log10(Pb_conc$CO3_2_minus * 1000 * DIC_MW)
  #  Pb_conc$log_H3PO4_mg_L         <- log10(Pb_conc$H3PO4 * 1000 * phosphate_MW)
  #  Pb_conc$log_H2PO4_minus_mg_L   <- log10(Pb_conc$H2PO4_minus * 1000 * phosphate_MW)
  #  Pb_conc$log_HPO4_2_minus_mg_L  <- log10(Pb_conc$HPO4_2_minus * 1000 * phosphate_MW)
  #  Pb_conc$log_PO4_3_minus_mg_L   <- log10(Pb_conc$PO4_3_minus * 1000 * phosphate_MW)
  Pb_conc$log_TOTSOLPb_mg_L            <- log10(Pb_conc$TOTSOLPb * 1000 * Pb_MW)
  Pb_conc$log_action_level_mg_L     <- log10(Pb_conc$action_level * 1000 * Pb_MW)

  #Create multiple solid solubility data set
  Pb_conc_multiple <- Pb_conc %>%
    filter(lead_solid %in% solids_include) %>%
    group_by(pH, DIC_mg_L, TOTP_mg_L, TOTSO4_mg_L, IS_mM, Cl_minus_mg_L) %>%
    filter(Pb_2_plus == min(Pb_2_plus))

  #Arrange to make plotting easier
  Pb_conc_multiple <- if (sim_type == "multi_pH") {
      arrange(Pb_conc_multiple, pH)
    } else if (sim_type == "multi_DIC") {
      arrange(Pb_conc_multiple, DIC_mg_L)
    } else if (sim_type == "multi_phosphate") {
      arrange(Pb_conc_multiple, TOTP_mg_L)
    } else if (sim_type == "multi_sulfate") {
      arrange(Pb_conc_multiple, TOTSO4_mg_L)
    } else if (sim_type == "multi_IS") {
      arrange(Pb_conc_multiple, IS_mM)
    } else if (sim_type == "multi_chloride") {
      arrange(Pb_conc_multiple, Cl_minus_mg_L)
    } else {
      arrange(Pb_conc_multiple, pH)
    }

  Pb_conc_multiple$analysis <- "Multiple"

  #Add multiple solid solubility to main data frame (value returned from simulation function call)
  sim <- bind_rows(Pb_conc, Pb_conc_multiple)
  return(sim)
}

# Define function to plot simulation results in various formats
plot_sim <- function(sim, solid_plot) {

  # Set required plot variables
  if (length(unique(sim$pH)) > 1) {
    type <- "pH"
    x_name <- "pH"
    step <- 1
  } else if (length(unique(sim$DIC_mg_L)) > 1) {
    type <- "DIC_mg_L"
    x_name <- "Dissolved Inorganic Carbon Concentration (mg carbon/L)"
    step <- 5
  } else if (length(unique(sim$TOTP_mg_L)) > 1) {
    type <- "TOTP_mg_L"
    step <- 1
    x_name <- "Total Phosphate Concentration (mg phosphate/L)"
  } else if (length(unique(sim$TOTSO4_mg_L)) > 1) {
    type <- "TOTSO4_mg_L"
    step <- 50
    x_name <- "Total Sulfate Concentration (mg sulfate/L)"
  } else if (length(unique(sim$IS_mM)) > 1) {
    type <- "IS_mM"
    step <- 5
    x_name <- "Ionic Strength (mM)"
  } else if (length(unique(sim$Cl_minus_mg_L)) > 1) {
    type <- "Cl_minus_mg_L"
    step <- 50
    x_name <- "Chloride Concentration (mg chloride/L)"
  } else {
    type <- "pH"
    x_name <- "pH"
    step <- 1
  }

  Pb <- sim %>%
    filter(analysis == solid_plot)

  Pb_DF_combo <- Pb %>%
    select(pH,
           DIC_mg_L,
           TOTP_mg_L,
           TOTSO4_mg_L,
           IS_mM,
           Cl_minus_mg_L,
           log_Pb_2_plus,
           log_PbOH_plus,
           log_PbOH2,
           log_PbOH3_minus,
           log_PbOH4_2_minus,
           log_Pb2OH_3_plus,
           log_Pb3OH4_2_plus,
           log_Pb4OH4_4_plus,
           log_Pb6OH8_4_plus,
           log_PbCl_plus,
           log_PbCl2,
           log_PbCl3_minus,
           log_PbCl4_2_minus,
           log_PbSO4,
           log_PbSO42_2_minus,
           log_PbHCO3_plus,
           log_PbCO3,
           log_PbCO32_2_minus,
           log_PbHPO4,
           log_PbH2PO4_plus,
           #         log_H2CO3,
           #         log_HCO3_minus,
           #         log_CO3_2_minus,
           #         log_H3PO4,
           #         log_H2PO4_minus,
           #         log_HPO4_2_minus,
           #         log_PO4_3_minus,
           log_TOTSOLPb,
           log_action_level)

  if (type == "pH") {
    Pb_DF <- Pb_DF_combo %>%
      select(-DIC_mg_L, -TOTP_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "DIC_mg_L") {
    Pb_DF <- Pb_DF_combo %>%
      select(-pH, -TOTP_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "TOTP_mg_L") {
    Pb_DF <- Pb_DF_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "TOTSO4_mg_L") {
    Pb_DF <- Pb_DF_combo %>%
      select(-pH, -DIC_mg_L, -TOTP_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "IS_mM") {
    Pb_DF <- Pb_DF_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -TOTP_mg_L, -Cl_minus_mg_L)
  } else if (type == "Cl_minus_mg_L") {
    Pb_DF <- Pb_DF_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -TOTP_mg_L, -IS_mM)
  }

  # Assemble data frame of log mg lead/L concentrations versus pH
  Pb_DF_mg_L_combo <- Pb %>%
    select(pH,
           DIC_mg_L,
           TOTP_mg_L,
           TOTSO4_mg_L,
           IS_mM,
           Cl_minus_mg_L,
           log_Pb_2_plus_mg_L,
           log_PbOH_plus_mg_L,
           log_PbOH2_mg_L,
           log_PbOH3_minus_mg_L,
           log_PbOH4_2_minus_mg_L,
           log_Pb2OH_3_plus_mg_L,
           log_Pb3OH4_2_plus_mg_L,
           log_Pb4OH4_4_plus_mg_L,
           log_Pb6OH8_4_plus_mg_L,
           log_PbCl_plus_mg_L,
           log_PbCl2_mg_L,
           log_PbCl3_minus_mg_L,
           log_PbCl4_2_minus_mg_L,
           log_PbSO4_mg_L,
           log_PbSO42_2_minus_mg_L,
           log_PbHCO3_plus_mg_L,
           log_PbCO3_mg_L,
           log_PbCO32_2_minus_mg_L,
           log_PbHPO4_mg_L,
           log_PbH2PO4_plus_mg_L,
           #         log_H2CO3_mg_L,
           #         log_HCO3_minus_mg_L,
           #         log_CO3_2_minus_mg_L,
           #         log_H3PO4_mg_L,
           #         log_H2PO4_minus_mg_L,
           #         log_HPO4_2_minus_mg_L,
           #         log_PO4_3_minus_mg_L,
           log_TOTSOLPb_mg_L,
           log_action_level_mg_L)

  if (type == "pH") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-DIC_mg_L, -TOTP_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "DIC_mg_L") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-pH, -TOTP_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "TOTP_mg_L") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "TOTSO4_mg_L") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-pH, -DIC_mg_L, -TOTP_mg_L, -IS_mM, -Cl_minus_mg_L)
  } else if (type == "IS_mM") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -TOTP_mg_L, -Cl_minus_mg_L)
  } else if (type == "Cl_minus_mg_L") {
    Pb_DF_mg_L <- Pb_DF_mg_L_combo %>%
      select(-pH, -DIC_mg_L, -TOTSO4_mg_L, -TOTP_mg_L, -IS_mM)
  }

  # Rename column names in data frames
  col_names <- c(type,
                 "Pb 2+",
                 "PbOH +", "Pb(OH)2", "Pb(OH)3 -", "Pb(OH)4 2-",
                 "Pb2OH 3+", "Pb3(OH)4 2+", "Pb4(OH)4 4+", "Pb6(OH)8 8+",
                 "PbCl +", "PbCl2", "PbCl3 -", "PbCl4 2-",
                 "PbSO4", "Pb(SO4)2 2-",
                 "PbHCO3 +", "PbCO3", "Pb(CO3)2 2-",
                 "PbHPO4", "PbH2PO4 +",
                 #               "H2CO3", "HCO3 -", "CO3 2-",
                 #               "H3PO4", "H2PO4 -", "HPO4 2-", "PO4 3-",
                 "TOTSOLPb",
                 "AL")

  colnames(Pb_DF) <- col_names
  colnames(Pb_DF_mg_L) <- col_names

  # Restructure data frames
  Pb_melt <- melt(Pb_DF, id.vars= type, variable.name = "chemical", value.name = "log_concentration_molar")

  Pb_melt_min <- Pb_melt %>%
    filter(chemical == "TOTSOLPb") %>%
    filter(log_concentration_molar == min(log_concentration_molar))

  Pb_melt_max <- Pb_melt %>%
    filter(chemical == "TOTSOLPb") %>%
    filter(log_concentration_molar == max(log_concentration_molar))

  Pb_melt_mg_L <- melt(Pb_DF_mg_L, id.vars= type, variable.name = "chemical", value.name = "log_concentration_mg_L")

  Pb_melt_mg_L_min <- Pb_melt_mg_L %>%
    filter(chemical == "TOTSOLPb") %>%
    filter(log_concentration_mg_L == min(log_concentration_mg_L))

  Pb_melt_mg_L_max <- Pb_melt_mg_L %>%
    filter(chemical == "TOTSOLPb") %>%
    filter(log_concentration_mg_L == max(log_concentration_mg_L))

  # Lead Action Level
  log_AL_mg_L <- log10(0.015)
  log_AL <- log10(0.015 / 1000 / Pb_MW)

  # Set column names for plotting
  colnames(Pb_melt) <- c("x_variable", "chemical", "log_concentration_molar")
  colnames(Pb_melt_mg_L) <- c("x_variable", "chemical", "log_concentration_mg_L")

  # Create molar log C - pH diagram
  plot1 <- ggplot(Pb_melt, aes(x = x_variable, y = log_concentration_molar, group = chemical, colour = chemical, linetype = chemical, size = chemical)) +
    geom_line() +
    xlab(x_name) +
    ylab("Log Molar Concentration") +
    scale_x_continuous(breaks = seq(min(Pb_melt$x_variable), max(Pb_melt$x_variable), step)) +
    scale_y_continuous(lim = c(min(Pb_melt_min[,3] - 4, log_AL),
                               max(Pb_melt_max[,3], log_AL)),
                       breaks = seq(round(min(Pb_melt_min[,3] - 4, log_AL)),
                                    round(max(Pb_melt_max[,3], log_AL)),
                                    max(round((max(Pb_melt_max[,3], log_AL) - min(Pb_melt_min[,3] - 4, log_AL))/15), 1))) +
    scale_colour_manual(labels = series_1, values = palette_1) +
    scale_linetype_manual(labels = series_1, values = linesets_1) +
    scale_size_manual(labels = series_1, values = linesize_1) +
    guides(colour = guide_legend(ncol = 1)) +
    mytheme

  # Create mg/L log C - pH Diagram
  plot2 <- ggplot(Pb_melt_mg_L, aes(x = x_variable, y = log_concentration_mg_L, color = chemical, linetype = chemical, size = chemical)) +
    geom_line() +
    xlab(x_name) +
    ylab("Log mg/L as Lead Concentration") +
    scale_x_continuous(breaks = seq(min(Pb_melt_mg_L$x_variable), max(Pb_melt_mg_L$x_variable), step)) +
    scale_y_continuous(lim = c(min(Pb_melt_mg_L_min[,3] - 4, log_AL_mg_L),
                               max(Pb_melt_mg_L_max[,3], log_AL_mg_L)),
                       breaks = seq(round(min(Pb_melt_mg_L_min[,3] - 4, log_AL_mg_L)),
                                    round(max(Pb_melt_mg_L_max[,3], log_AL_mg_L)),
                                    max(round((max(Pb_melt_mg_L_max[,3], log_AL_mg_L) - min(Pb_melt_mg_L_min[,3] - 4, log_AL_mg_L))/15), 1))) +
    scale_colour_manual(labels = series_2, values = palette_2) +
    scale_linetype_manual(labels = series_2, values = linesets_2) +
    scale_size_manual(labels = series_2, values = linesize_2) +
    guides(colour = guide_legend(ncol = 1)) +
    mytheme

  # Return plots
  plot <- grid.arrange(plot1, plot2, ncol=1)
}

# Define function to plot controlling solid
plot_control <- function(sim) {

  # Set required plot variables
  if (length(unique(sim$pH)) > 1) {
    type <- "pH"
    x_name <- "pH"
    step <- 1
  } else if (length(unique(sim$DIC_mg_L)) > 1) {
    type <- "DIC_mg_L"
    x_name <- "Dissolved Inorganic Carbon Concentration (mg carbon/L)"
    step <- 5
  } else if (length(unique(sim$TOTP_mg_L)) > 1) {
    type <- "TOTP_mg_L"
    step <- 1
    x_name <- "Total Phosphate Concentration (mg phosphate/L)"
  } else if (length(unique(sim$TOTSO4_mg_L)) > 1) {
    type <- "TOTSO4_mg_L"
    step <- 50
    x_name <- "Total Sulfate Concentration (mg sulfate/L)"
  } else if (length(unique(sim$IS_mM)) > 1) {
    type <- "IS_mM"
    step <- 5
    x_name <- "Ionic Strength (mM)"
  } else if (length(unique(sim$Cl_minus_mg_L)) > 1) {
    type <- "Cl_minus_mg_L"
    step <- 50
    x_name <- "Chloride Concentration (mg chloride/L)"
  } else {
    type <- "pH"
    x_name <- "pH"
    step <- 1
  }

  Pb_control_combo <- sim %>%
    filter(analysis == "Multiple")

  if (type == "pH") {
    Pb_control <- Pb_control_combo %>%
      select(pH, lead_solid)
  } else if (type == "DIC_mg_L") {
    Pb_control <- Pb_control_combo %>%
      select(DIC_mg_L, lead_solid)
  } else if (type == "TOTP_mg_L") {
    Pb_control <- Pb_control_combo %>%
      select(TOTP_mg_L, lead_solid)
  } else if (type == "TOTSO4_mg_L") {
    Pb_control <- Pb_control_combo %>%
      select(TOTSO4_mg_L, lead_solid)
  } else if (type == "IS_mM") {
    Pb_control <- Pb_control_combo %>%
      select(IS_mM, lead_solid)
  } else if (type == "Cl_minus_mg_L") {
    Pb_control <- Pb_control_combo %>%
      select(Cl_minus_mg_L, lead_solid)
  }

  colnames(Pb_control) <- c("x_variable", "lead_solid")

  Pb_control$solid <- "Controlling Solid"

  plot <- ggplot(Pb_control, aes(x = x_variable, y = solid, color = lead_solid)) +
    geom_point(size = 2) +
    xlab("Controlling Solid") +
    scale_x_continuous(breaks = seq(min(Pb_control[1]), max(Pb_control[1]), step)) +
    mytheme +
    theme(axis.text.y = element_blank(),
          axis.title.y = element_blank(),
          legend.position = "top",
          legend.direction = "horizontal")
  return(plot)
}

#SERVER FUNCTION DEFINITION SECTION#########################################################################################
############################################################################################################################

# Define server logic required to run simulations and produce output
server <- function(input, output, session) {

  #Copy Simulation A's inputs to Simulation B's inputs
  observe({
    #Take a dependency on input$AtoBIC
    if(input$A_to_B_IC == 0) return(NULL)

    isolate(update_IC(session, "A", "B", input))
  })

  #Copy Simulation B's inputs to Simulation A's inputs
  observe({
    #Take a dependency on input$BtoAIC
    if(input$B_to_A_IC == 0) return(NULL)

    isolate(update_IC(session, "B", "A", input))
  })

  #Copy Simulation A's equilibrium constants to Simulation B's equilibrium constants
  observe({
    #Take a dependency on input$AtoBIC
    if(input$A_to_B_IC_EQ == 0) return(NULL)

    isolate(update_IC_EQ(session, "A", "B", input))
  })

  #Copy Simulation B's equilibrium constants to Simulation A's equilibrium constants
  observe({
    #Take a dependency on input$BtoAIC
    if(input$B_to_A_IC_EQ == 0) return(NULL)

    isolate(update_IC_EQ(session, "B", "A", input))
  })

  # Define function to get inputted simulation conditions and states based on prefix provided
  sim_Params <- function(prefix) {
    params <- lapply(sim_Names, function(p) {
      input[[paste0(prefix, "_", p)]]
    })
  }

  # Run simulation based on provided simulation conditions
  simA <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateA == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_solubility, sim_Params("A")))
  })

  simB <- reactive({
    #Take a dependency on input$simupdate
    if(input$simupdateB == 0) return(NULL)

    #Isolate simulation run only on input$simupdate button selection
    isolate(do.call(simulate_solubility, sim_Params("B")))
  })

  # Produce desired reactive plots
  output$A_Lead_Hydroxide <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Lead Hydroxide")
  })

  output$B_Lead_Hydroxide <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Lead Hydroxide")
  })

  # Produce desired reactive plots
  output$A_Cerussite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Cerussite")
  })

  output$B_Cerussite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Cerussite")
  })

  # Produce desired reactive plots
  output$A_Hydrocerussite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Hydrocerussite")
  })

  output$B_Hydrocerussite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Hydrocerussite")
  })

  # Produce desired reactive plots
  output$A_Hydroxypyromorphite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Hydroxypyromorphite")
  })

  output$B_Hydroxypyromorphite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Hydroxypyromorphite")
  })

  # Produce desired reactive plots
  output$A_Pyromorphite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Pyromorphite")
  })

  output$B_Pyromorphite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Pyromorphite")
  })

  # Produce desired reactive plots
  output$A_Primary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Primary Lead Orthophosphate")
  })

  output$B_Primary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Primary Lead Orthophosphate")
  })

  # Produce desired reactive plots
  output$A_Secondary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Secondary Lead Orthophosphate")
  })

  output$B_Secondary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Secondary Lead Orthophosphate")
  })

  # Produce desired reactive plots
  output$A_Tertiary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Tertiary Lead Orthophosphate")
  })

  output$B_Tertiary_Lead <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Tertiary Lead Orthophosphate")
  })

  # Produce desired reactive plots
  output$A_Anglesite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Anglesite")
  })

  output$B_Anglesite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Anglesite")
  })

  # Produce desired reactive plots
  output$A_Laurionite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Laurionite")
  })

  output$B_Laurionite <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Laurionite")
  })

  # Produce desired reactive plots
  output$A_Multiple <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simA(), "Multiple")
  })

  output$B_Multiple <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_sim(simB(), "Multiple")
  })

  # Produce desired reactive plots
  output$A_Multiple_control <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateA == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_control(simA())
  })

  output$B_Multiple_control <- renderPlot({
    #Do not create a plot until an initial simulation has been conducted
    if(input$simupdateB == 0) return(NULL)

    #Isolate plot to update only on input$simupdate selection
    plot_control(simB())
  })

  # Expression that gets data to be downloaded at User's request
  output$A_downloadData <- downloadHandler(
    filename = function() {
      paste('A', '_', substr(as.character(Sys.time()), 1, 10),
            '_', substr(as.character(Sys.time()), 12, 13),
            '_', substr(as.character(Sys.time()), 15, 16), '.csv', sep = '')
    },
    content = function(file) {write.csv(simA(), file, row.names = TRUE)
    }
  )

  output$B_downloadData <- downloadHandler(
    filename = function() {
      paste('B', '_', substr(as.character(Sys.time()), 1, 10),
            '_', substr(as.character(Sys.time()), 12, 13),
            '_', substr(as.character(Sys.time()), 15, 16), '.csv', sep = '')
    },
    content = function(file) {write.csv(simB(), file, row.names = TRUE)
    }
  )
}

#UI OBJECT SECTION BEGINS HERE##############################################################################################
############################################################################################################################
############################################################################################################################
############################################################################################################################

#UI OBJECT GENERAL SECTION##################################################################################################
############################################################################################################################

# Define function to take inputs for general simulation conditions
render_inputs <- function(prefix, prefix2) {
  wellPanel(
    #Header for Page
    h3(paste0("Select Simulation ", prefix, " Conditions")),

    # Call to display initial simulation notification
    conditionalPanel(condition = paste0("input.simupdate", prefix, "== 0"),
                     tags$div("Note: An initial simulation has not been run; therefore, no plot has been generated", id = "initialsim")
                     ),
    br(),

    fluidRow(
      # Select type of simulation to conduct
      radioButtons(paste0(prefix, "_", "sim_type"),
                  label = p("Type of Simulation", style = "font-size: 16px"),
                  c("pH range" = "multi_pH",
                    "Ionic Strength range" = "multi_IS",
                    "Chloride range" = "multi_chloride",
                    "Total Sulfate range" = "multi_sulfate",
                    "Dissolved Inorganic Carbon range" = "multi_DIC",
                    "Total Phosphate range" = "multi_phosphate",
                    "Single condition" = "single"),
                  selected = "multi_pH"),
      bsTooltip(id = paste0(prefix, "_", "sim_type"),
                "Select desired type of simulation to run:  (1) a pH range, (2) an ionic strength range, (3) a chloride range, (4) a total sulfate range, (5) a dissolved inorganic carbon range, (6) a total phosphate range, or (7) a single condition",
                "top",
                options = list(container = "body")),

      # Select which pH input to display
      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_pH'"),

        # Create input and tooltip pH range of simulation
        sliderInput(paste0(prefix, "_", "pH_range"),
                    label = p("pH Range", style = "font-size: 16px"),
                    min = 0.0,
                    max = 14.0,
                    value = c(6.0,11.0),
                    step = 0.05),
        br(),
        bsTooltip(id = paste0(prefix, "_", "pH_range"),
                  "Set slider to known pH range",
                  "top",
                  options = list(container = "body"))
        ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type != 'multi_pH'"),

        # Create input and tooltip pH range of simulation
        sliderInput(paste0(prefix, "_", "pH_single"),
                    label = p("pH", style = "font-size: 16px"),
                    min = 0.0,
                    max = 14.0,
                    value = 7.0,
                    step = 0.05),
        br(),
        bsTooltip(id = paste0(prefix, "_", "pH_single"),
                  "Set slider to known pH",
                  "top",
                  options = list(container = "body"))
        ),

      # Select which ionic strength input to display
      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_IS'"),

        # Create input and tooltip for ionic strength range of simulation
        sliderInput(paste0(prefix, "_", "IS_mM_range"),
                    label = p("Ionic Strength (mM) Range", style = "font-size: 16px"),
                    min = 0,
                    max = 100,
                    value = c(10, 50),
                    step = 0.5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "IS_mM_range"),
                  "Set slider to known ionic strength range in millimolar",
                  "top",
                  options = list(container = "body"))
        ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type != 'multi_IS'"),

        # Create input and tooltip for known ionic strength
        sliderInput(paste0(prefix, "_", "IS_mM_single"),
                    label = p("Ionic Strength (mM)", style = "font-size: 16px"),
                    min = 0,
                    max = 100,
                    value = 5,
                    step = 0.5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "IS_mM_single"),
                  "Set slider to known ionic strength in millimolar",
                  "top",
                  options = list(container = "body"))
        ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_chloride'"),

        # Create input and tooltip for chloride concentration range
        sliderInput(paste0(prefix, "_", "Cl_minus_mg_L_range"),
                    label = p("Chloride (mg chloride/L) Range", style = "font-size: 16px"),
                    min = 1,
                    max = 1000,
                    value = c(100, 500),
                    step = 5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "Cl_minus_mg_L_range"),
                  "Set slider to known chloride concentration range in mg chloride per liter",
                  "top",
                  options = list(container = "body"))
        ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type != 'multi_chloride'"),

        # Create input and tooltip for known chloride concentration
        sliderInput(paste0(prefix, "_", "Cl_minus_mg_L_single"),
                    label = p("Chloride (mg chloride/L)", style = "font-size: 16px"),
                    min = 0,
                    max = 1000,
                    value = 100,
                    step = 5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "Cl_minus_mg_L_single"),
                  "Set slider to known chloride concentration in mg chloride per liter",
                  "top",
                  options = list(container = "body"))
        ),

      # Select which Sulfate input to display
      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_sulfate'"),

        # Create input and tooltip for known sulfate concentration range
        sliderInput(paste0(prefix, "_", "TOTSO4_mg_L_range"),
                    label = p("Total Sulfate (mg sulfate/L) Range", style = "font-size: 16px"),
                    min = 1,
                    max = 1000,
                    value = c(100,500),
                    step = 5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "TOTSO4_mg_L_range"),
                  "Set slider to known sulfate concentration range in mg sulfate per liter",
                  "top",
                  options = list(container = "body"))
        ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type != 'multi_sulfate'"),

        # Create input and tooltip for known sulfate concentration
        sliderInput(paste0(prefix, "_", "TOTSO4_mg_L_single"),
                    label = p("Total Sulfate (mg sulfate/L)", style = "font-size: 16px"),
                    min = 0,
                    max = 1000,
                    value = 100,
                    step = 5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "TOTSO4_mg_L_single"),
                  "Set slider to known total sulfate concentration in mg sulfate per liter",
                  "top",
                  options = list(container = "body"))
      ),


      # Select which DIC input to display
      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_DIC'"),

        # Create input and tooltip for DIC range of simulation
        sliderInput(paste0(prefix, "_", "DIC_mg_L_range"),
                    label = p("Inorganic Carbon (mg carbon/L) Range", style = "font-size: 16px"),
                    min = 0.5,
                    max = 100,
                    value = c(5, 50),
                    step = 0.5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "DIC_mg_L_range"),
                  "Set slider to known dissolved inorganic carbon range in mg carbon per liter",
                  "top",
                  options = list(container = "body"))
      ),

      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type != 'multi_DIC'"),

        # Create input and tooltip for known dissolved inorganic carbon concentration
        sliderInput(paste0(prefix, "_", "DIC_mg_L_single"),
                    label = p("Inorganic Carbon (mg carbon/L)", style = "font-size: 16px"),
                    min = 0,
                    max = 100,
                    value = 10,
                    step = 0.5),
        br(),
        bsTooltip(id = paste0(prefix, "_", "DIC_mg_L_single"),
                  "Set slider to known dissolved inorganic carbon concentration in mg carbon per liter",
                  "top",
                  options = list(container = "body"))
        ),


      # Select which phosphate input to display
      conditionalPanel(
        condition = paste0("input.", prefix, "_", "sim_type == 'multi_phosphate'"),

         #Create input and tooltip for known total phosphate concentration
        sliderInput(paste0(prefix, "_", "TOTP_mg_L_range"),
                    label = p("Total Phosphate (mg phosphate/L) Range", style = "font-size: 16px"),
                    min = 0.05,
                    max = 10.00,
                    value = c(0.50,5.00),
                    step = 0.05),
        br(),
        bsTooltip(id = paste0(prefix, "_", "TOTP_mg_L_range"),
                  "Set slider to known total phosphate concentration range in mg phosphate per liter",
                  "top",
                  options = list(container = "body"))
        ),

        conditionalPanel(
          condition = paste0("input.", prefix, "_", "sim_type != 'multi_phosphate'"),

          #Create input and tooltip for known total phosphate concentration
          sliderInput(paste0(prefix, "_", "TOTP_mg_L_single"),
                      label = p("Total Phosphate (mg phosphate/L)", style = "font-size: 16px"),
                      min = 0.00,
                      max = 10.00,
                      value = 2.00,
                      step = 0.05),
          br(),
          bsTooltip(id = paste0(prefix, "_", "TOTP_mg_L_single"),
                    "Set slider to known total phosphate concentration in mg phosphate per liter",
                    "top",
                    options = list(container = "body"))
          )
      ),

      #Create input and tooltip to select which solids to include in minimization analysis
      checkboxGroupInput(paste0(prefix, "_", "solids_include"),
                         label = h4("Solids to Include in Multiple Solid Analysis (Must Select at Least One)"),
                         choices = c("Lead Hydroxide",
                                     "Cerussite",
                                     "Hydrocerussite",
                                     "Hydroxypyromorphite",
                                     "Pyromorphite",
                                     "Primary Lead Orthophosphate",
                                     "Secondary Lead Orthophosphate",
                                     "Tertiary Lead Orthophosphate",
                                     "Anglesite",
                                     "Laurionite"),
                         selected = c("Lead Hydroxide",
                                      "Cerussite",
                                      "Hydrocerussite",
                                      "Hydroxypyromorphite",
                                      "Pyromorphite",
                                      "Primary Lead Orthophosphate",
                                      "Secondary Lead Orthophosphate",
                                      "Tertiary Lead Orthophosphate",
                                      "Anglesite",
                                      "Laurionite"),
                         inline = FALSE
                         ),
      bsTooltip(id = paste0(prefix, "_", "solids_include"),
                "Use check boxes to select which solids to include in the multiple solid analysis",
                "top",
                options = list(container = "body")
                ),


    # Copy inputs from one simulation to the other
    actionButton(paste0(prefix, "_to_", prefix2, "_IC"),
                 paste0("Copy Simulation ", prefix, "'s Conditions to Simulation ", prefix2, "'s Conditions"),
                 icon("copy")
    ),
    bsTooltip(id = paste0(prefix, "_to_", prefix2, "_IC"),
              "Press button to copy current simulation conditions to other simulation",
              "top",
              options = list(container = "body")),
    br(),
    br(),

    # Update simulation
    actionButton(paste0("simupdate", prefix),
                 paste0("Update Simulation ", prefix, " (Press after Finished Changing Simulation Inputs)"), icon("refresh")),
    bsTooltip(id = paste0("simupdate", prefix),
              "Press button to update simulation using current input settings",
              "top",
              options = list(container = "body")),
    br(),
    br(),

    # Download simulation data
    downloadButton(paste0(prefix, "_", "downloadData"),
                   paste0("Simulation ", prefix, " ", "Chemical Concentration Data Download (.csv file)")),
    bsTooltip(id = paste0(prefix, "_", "downloadData"),
              "Press button to download Log C - pH plot data to a comma seperated variable (.csv) file for use in another program (e.g., Excel)",
              "top",
              options = list(container = "body"))
  )
}

# Define function to take inputs for general simulation conditions
render_inputs_EQ <- function(prefix, prefix2) {
  wellPanel(
    #Header for Page
    h3(paste0("Select Simulation ", prefix, " Equilibrium Constants")),

    #Select equilibrium constants to Use
    fluidRow(
      # Solids
      h4("Solids"),
      radioButtons(paste0(prefix, "_", "K_solid_lead_hydroxide"),
                   label = p(HTML("log K<sub>solid</sub> (Lead Hydroxide) for Pb(OH)<sub>2</sub> (s) + 2H<sup>+</sup> &#8651; Pb<sup>2+</sup> + 2H<sub>2</sub>O"),
                             style = "font-size: 14px"),
                   c("13.06 from Schock et al. (1996)" = 10^13.06),
                   selected = 10^13.06),
      bsTooltip(id = paste0(prefix, "_", "K_solid_lead_hydroxide"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_cerussite"),
                   label = p(HTML("log K<sub>solid</sub> (Cerussite) for Pb(CO)<sub>3</sub> (s) &#8651; Pb<sup>2+</sup> + CO<sub>3</sub><sup>2&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("-13.11 from Schock et al. (1996)" = 10^-13.11),
                   selected = 10^-13.11),
      bsTooltip(id = paste0(prefix, "_", "K_solid_cerussite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_hydrocerussite"),
                   label = p(HTML("log K<sub>solid</sub> (Hydrocerussite) for Pb<sub>3</sub>(CO<sub>3</sub>)<sub>2</sub>(OH)<sub>2</sub> (s) + 2H<sup>+</sup> &#8651; 3Pb<sup>2+</sup> + 2CO<sub>3</sub><sup>2&#8212;</sup> + 2H<sub>2</sub>O"),
                             style = "font-size: 14px"),
                   c("-18.00 from Schock et al. (1996)" = 10^-18.00),
                   selected = 10^-18.00),
      bsTooltip(id = paste0(prefix, "_", "K_solid_hydrocerussite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_hydroxypyromorphite"),
                   label = p(HTML("log K<sub>solid</sub> (Hydroxypyromorphite) for Pb<sub>5</sub>(PO<sub>4</sub>)<sub>3</sub>OH (s) + H<sup>+</sup> &#8651; 5Pb<sup>2+</sup> + 3PO<sub>4</sub><sup>3&#8212;</sup> + H<sub>2</sub>O"),
                             style = "font-size: 14px"),
                   c("-62.83 from Schock et al. (1996)" = 10^-62.83,
                     "-66.77 from Zhu et al. (2015)" = 10^-66.77),
                   selected = 10^-62.83),
      bsTooltip(id = paste0(prefix, "_", "K_solid_hydroxypyromorphite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_pyromorphite"),
                   label = p(HTML("log K<sub>solid</sub> (Pyromorphite) for Pb<sub>5</sub>(PO<sub>4</sub>)<sub>3</sub>Cl (s) &#8651; 5Pb<sup>2+</sup> + 3PO<sub>4</sub><sup>3&#8212;</sup> + Cl<sup>&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("-80.4 from Xie & Giammar (2007)" = 10^-80.4,
                     "-79.6 from Topolska et al. (2016)" = 10^-79.6),
                   selected = 10^-79.6),
      bsTooltip(id = paste0(prefix, "_", "K_solid_pyromorphite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_primary_lead_ortho"),
                   label = p(HTML("log K<sub>solid</sub> (Primary Lead Orthophosphate) for Pb(H<sub>2</sub>PO<sub>4</sub>)<sub>2</sub> (s) &#8651; Pb<sup>2+</sup> + 2PO<sub>4</sub><sup>3&#8212;</sup> + 4H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-48.916 from Powell et al. (2009)" = 10^-48.916),
                   selected = 10^-48.916),
      bsTooltip(id = paste0(prefix, "_", "K_solid_primary_lead_ortho"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_secondary_lead_ortho"),
                   label = p(HTML("log K<sub>solid</sub> (Secondary Lead Orthophosphate) for PbHPO<sub>4</sub> (s) &#8651; Pb<sup>2+</sup> + PO<sub>4</sub><sup>3&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-23.81 from Schock et al. (1996)" = 10^-23.81),
                   selected = 10^-23.81),
      bsTooltip(id = paste0(prefix, "_", "K_solid_secondary_lead_ortho"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_tertiary_lead_ortho"),
                   label = p(HTML("log K<sub>solid</sub> (Tertiary Lead Orthophosphate) for Pb<sub>3</sub>(PO<sub>4</sub>)<sub>2</sub> (s) &#8651; 3Pb<sup>2+</sup> + 2PO<sub>4</sub><sup>3&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("-44.4 from Powell et al. (2009)" = 10^-44.4),
                   selected = 10^-44.4),
      bsTooltip(id = paste0(prefix, "_", "K_solid_tertiary_lead_ortho"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_solid_anglesite"),
                   label = p(HTML("log K<sub>solid</sub> (Anglesite) for Pb(SO)<sub>4</sub> (s) &#8651; Pb<sup>2+</sup> + SO<sub>4</sub><sup>2&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("-7.79 from Schock et al. (1996)" = 10^-7.79),
                   selected = 10^-7.79),
      bsTooltip(id = paste0(prefix, "_", "K_solid_anglesite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),

      br(),

      radioButtons(paste0(prefix, "_", "K_solid_laurionite"),
                   label = p(HTML("log K<sub>solid</sub> (Laurionite) for PbClOH (s) + H<sup>+</sup> &#8651; Pb<sup>2+</sup> + Cl<sup>&#8212;</sup> + H<sub>2</sub>O"),
                             style = "font-size: 14px"),
                   c("0.619 from Nasanen & Lindell (1976)" = 10^0.619,
                     "0.29 from Lothenbach et al. (1999)" = 10^0.29),
                   selected = 10^0.619),
      bsTooltip(id = paste0(prefix, "_", "K_solid_laurionite"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),

      br(),

      # Lead-Hydroxide Complexes
      br(),
      h4("Lead-Hydroxide Complexes"),
      radioButtons(paste0(prefix, "_", "B_1_OH"),
                   label = p(HTML("log &beta;<sub>1,OH</sub> for Pb<sup>2+</sup> + H<sub>2</sub>O &#8651; PbOH<sup>+</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-7.22 from Schock et al. (1996)" = 10^-7.22),
                   selected = 10^-7.22),
      bsTooltip(id = paste0(prefix, "_", "B_1_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_2_OH"),
                   label = p(HTML("log &beta;<sub>2,OH</sub> for Pb<sup>2+</sup> + 2H<sub>2</sub>O &#8651; Pb(OH) (aq) + 2H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-16.91 from Schock et al. (1996)" = 10^-16.91),
                   selected = 10^-16.91),
      bsTooltip(id = paste0(prefix, "_", "B_2_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_3_OH"),
                   label = p(HTML("log &beta;<sub>3,OH</sub> for Pb<sup>2+</sup> + 3H<sub>2</sub>O &#8651; Pb(OH)<sub>3</sub><sup>&#8212;</sup> + 3H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-28.08 from Schock et al. (1996)" = 10^-28.08),
                   selected = 10^-28.08),
      bsTooltip(id = paste0(prefix, "_", "B_3_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_4_OH"),
                   label = p(HTML("log &beta;<sub>4,OH</sub> for Pb<sup>2+</sup> + 4H<sub>2</sub>O &#8651; Pb(OH)<sub>4</sub><sup>2&#8212;</sup> + 4H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-39.72 from Schock et al. (1996)" = 10^-39.72),
                   selected = 10^-39.72),
      bsTooltip(id = paste0(prefix, "_", "B_4_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_2_1_OH"),
                   label = p(HTML("log &beta;<sub>2,1,OH</sub> for 2Pb<sup>2+</sup> + H<sub>2</sub>O &#8651; Pb<sub>2</sub>OH<sup>3+</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-6.36 from Schock et al. (1996)" = 10^-6.36),
                   selected = 10^-6.36),
      bsTooltip(id = paste0(prefix, "_", "B_2_1_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_3_4_OH"),
                   label = p(HTML("log &beta;<sub>3,4,OH</sub> for 3Pb<sup>2+</sup> + 4H<sub>2</sub>O &#8651; Pb<sub>3</sub>(OH)<sub>4</sub><sup>2+</sup> + 4H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-23.86 from Schock et al. (1996)" = 10^-23.86),
                   selected = 10^-23.86),
      bsTooltip(id = paste0(prefix, "_", "B_3_4_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_4_4_OH"),
                   label = p(HTML("log &beta;<sub>4,4,OH</sub> for 4Pb<sup>2+</sup> + 4H<sub>2</sub>O &#8651; Pb<sub>4</sub>(OH)<sub>4</sub><sup>4+</sup> + 4H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-20.88 from Schock et al. (1996)" = 10^-20.88),
                   selected = 10^-20.88),
      bsTooltip(id = paste0(prefix, "_", "B_4_4_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_6_8_OH"),
                   label = p(HTML("log &beta;<sub>6,8,OH</sub> for 6Pb<sup>2+</sup> + 8H<sub>2</sub>O &#8651; Pb<sub>6</sub>(OH)<sub>8</sub><sup>4+</sup> + 8H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-43.62 from Schock et al. (1996)" = 10^-43.62),
                   selected = 10^-43.62),
      bsTooltip(id = paste0(prefix, "_", "B_6_8_OH"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      # Lead-Chloride Complexes
      br(),
      h4("Lead-Chloride Complexes"),

      radioButtons(paste0(prefix, "_", "K_1_Cl"),
                   label = p(HTML("log K<sub>1,Cl</sub> for Pb<sup>2+</sup> + Cl<sup>&#8212;</sup> &#8651; PbCl<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("1.59 from Schock et al. (1996)" = 10^1.59),
                   selected = 10^1.59),
      bsTooltip(id = paste0(prefix, "_", "K_1_Cl"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_2_Cl"),
                   label = p(HTML("log &beta;<sub>2,Cl</sub> for Pb<sup>2+</sup> + 2Cl<sup>&#8212;</sup> &#8651; PbCl<sub>2</sub> (aq)"),
                             style = "font-size: 14px"),
                   c("1.80 from Schock et al. (1996)" = 10^1.80),
                   selected = 10^1.80),
      bsTooltip(id = paste0(prefix, "_", "B_2_Cl"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_3_Cl"),
                   label = p(HTML("log &beta;<sub>3,Cl</sub> for Pb<sup>2+</sup> + 3Cl<sup>&#8212;</sup> &#8651; PbCl<sub>3</sub><sup>&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("1.71 from Schock et al. (1996)" = 10^1.71),
                   selected = 10^1.71),
      bsTooltip(id = paste0(prefix, "_", "B_3_Cl"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_4_Cl"),
                   label = p(HTML("log &beta;<sub>4,Cl</sub> for Pb<sup>2+</sup> + 4Cl<sup>&#8212;</sup> &#8651; PbCl<sub>4</sub><sup>2&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("1.43 from Schock et al. (1996)" = 10^1.43),
                   selected = 10^1.43),
      bsTooltip(id = paste0(prefix, "_", "B_4_Cl"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      # Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes
      br(),
      h4("Sulfate Acid-Base Chemistry and Lead-Sulfate Complexes"),

      radioButtons(paste0(prefix, "_", "K_s"),
                   label = p(HTML("log K<sub>s</sub> for HSO<sub>4</sub><sup>&#8212;</sup> &#8651; SO<sub>4</sub><sup>2&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-1.99 from Benjamin (2002)" = 10^-1.99),
                   selected = 10^-1.99),
      bsTooltip(id = paste0(prefix, "_", "K_s"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_1_SO4"),
                   label = p(HTML("log K<sub>1,SO4</sub> for Pb<sup>2+</sup> + SO<sub>4</sub><sup>2&#8212;</sup> &#8651; PbSO<sub>4</sub> (aq)"),
                             style = "font-size: 14px"),
                   c("2.73 from Schock et al. (1996)" = 10^2.73),
                   selected = 10^2.73),
      bsTooltip(id = paste0(prefix, "_", "K_1_SO4"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "B_2_SO4"),
                   label = p(HTML("log &beta;<sub>2,SO4</sub> for Pb<sup>2+</sup> + 2SO<sub>4</sub><sup>2&#8212;</sup> &#8651; Pb(SO<sub>4</sub>)<sub>2</sub><sup>2&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("3.50 from Schock et al. (1996)" = 10^3.50),
                   selected = 10^3.50),
      bsTooltip(id = paste0(prefix, "_", "B_2_SO4"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      # Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes
      br(),
      h4("Carbonate Acid-Base Chemistry and Lead-Carbonate Complexes"),

      radioButtons(paste0(prefix, "_", "K_c_1"),
                   label = p(HTML("log K<sub>c1</sub> for H<sub>2</sub>CO<sub>3</sub><sup>*</sup> &#8651; HCO<sub>3</sub><sup>&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-6.355 from Powell et al. (2005)" = 10^-6.355),
                   selected = 10^-6.355),
      bsTooltip(id = paste0(prefix, "_", "K_c_1"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_c_2"),
                   label = p(HTML("log K<sub>c2</sub> for HCO<sub>3</sub><sup>&#8212;</sup> &#8651; CO<sub>3</sub><sup>2&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-10.336 from Powell et al. (2005)" = 10^-10.336),
                   selected = 10^-10.336),
      bsTooltip(id = paste0(prefix, "_", "K_c_2"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_1_CO3"),
                   label = p(HTML("log K<sub>1,CO3</sub> for Pb<sup>2+</sup> + H<sup>+</sup> + CO<sub>3</sub><sup>2&#8212;</sup> &#8651; PbHCO<sub>3</sub><sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("12.59 from Schock et al. (1996)" = 10^12.59),
                   selected = 10^12.59),
      bsTooltip(id = paste0(prefix, "_", "K_1_CO3"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_2_CO3"),
                   label = p(HTML("log K<sub>2,CO3</sub> for Pb<sup>2+</sup> + CO<sub>3</sub><sup>2&#8212;</sup> &#8651; PbCO<sub>3</sub> (aq)"),
                             style = "font-size: 14px"),
                   c("7.10 from Schock et al. (1996)" = 10^7.10),
                   selected = 10^7.10),
      bsTooltip(id = paste0(prefix, "_", "K_2_CO3"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_3_CO3"),
                   label = p(HTML("log K<sub>3,CO3</sub> for Pb<sup>2+</sup> + 2CO<sub>3</sub><sup>2&#8212;</sup> &#8651; Pb(CO<sub>3</sub>)<sub>2</sub><sup>2&#8212;</sup>"),
                             style = "font-size: 14px"),
                   c("10.33 from Schock et al. (1996)" = 10^10.33),
                   selected = 10^10.33),
      bsTooltip(id = paste0(prefix, "_", "K_3_CO3"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      # Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes
      br(),
      h4("Phosphate Acid-Base Chemistry and Lead-Phosphate Complexes"),

      radioButtons(paste0(prefix, "_", "K_p_1"),
                   label = p(HTML("log K<sub>p1</sub> for H<sub>3</sub>PO<sub>4</sub> &#8651; H<sub>2</sub>PO<sub>4</sub><sup>&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-2.141 from Powell et al. (2005)" = 10^-2.141),
                   selected = 10^-2.141),
      bsTooltip(id = paste0(prefix, "_", "K_p_1"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_p_2"),
                   label = p(HTML("log K<sub>p2</sub> for H<sub>2</sub>PO<sub>4</sub><sup>&#8212;</sup> &#8651; HPO<sub>4</sub><sup>2&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-7.200 from Powell et al. (2005)" = 10^-7.200),
                   selected = 10^-7.200),
      bsTooltip(id = paste0(prefix, "_", "K_p_2"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_p_3"),
                   label = p(HTML("log K<sub>p3</sub> for HPO<sub>4</sub><sup>2&#8212;</sup> &#8651; PO<sub>4</sub><sup>3&#8212;</sup> + H<sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("-12.338 from Powell et al. (2005)" = 10^-12.338),
                   selected = 10^-12.338),
      bsTooltip(id = paste0(prefix, "_", "K_p_3"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_1_PO4"),
                   label = p(HTML("log K<sub>1,PO4</sub> for Pb<sup>2+</sup> + H<sup>+</sup> + PO<sub>4</sub><sup>3&#8212;</sup> &#8651; PbHPO<sub>4</sub> (aq)"),
                             style = "font-size: 14px"),
                   c("15.41 from Schock et al. (1996)" = 10^15.41),
                   selected = 10^15.41),
      bsTooltip(id = paste0(prefix, "_", "K_1_PO4"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body")),
      br(),

      radioButtons(paste0(prefix, "_", "K_2_PO4"),
                   label = p(HTML("log K<sub>2,PO4</sub> for Pb<sup>2+</sup> + 2H<sup>+</sup> + PO<sub>4</sub><sup>3&#8212;</sup> &#8651; PbH<sub>2</sub>PO<sub>4</sub><sup>+</sup>"),
                             style = "font-size: 14px"),
                   c("21.05 from Schock et al. (1996)" = 10^21.05),
                   selected = 10^21.05),
      bsTooltip(id = paste0(prefix, "_", "K_2_PO4"),
                "Set desired equilibrium constant for simulation",
                "top",
                options = list(container = "body"))
    ),

    br(),
    br(),

    # Copy inputs from one simulation to the other
    actionButton(paste0(prefix, "_to_", prefix2, "_IC_EQ"),
                 paste0("Copy Simulation ", prefix, "'s Equilibrium Constants to Simulation ", prefix2, "'s Equilibrium Constants"),
                 icon("copy")
    ),
    bsTooltip(id = paste0(prefix, "_to_", prefix2, "_IC_EQ"),
              "Press button to copy current simulation equilibrium constants to other simulation",
              "top",
              options = list(container = "body"))
  )
}

# Define function to take render solubility plots
render_plot_outputs <- function(prefix, prefix2) {
  wellPanel(
    h3(paste0("Simulation ", prefix, " Controlling")),
    h3("Log C Lead Solubility Plots"),
    br(),
    plotOutput(prefix2, height = "900px")
  )
}

# Define function to take render solubility control plot
render_solid_outputs <- function(prefix, prefix2) {
    wellPanel(plotOutput(prefix2, height = "100px"))
}

#UI OBJECT DEFINITION SECTION###############################################################################################
############################################################################################################################

#Define UI layout
ui <- dashboardPage(

  dashboardHeader(disable = TRUE),

  dashboardSidebar(

    width = 300,

    sidebarMenu(
      menuItem("1. General Information", tabName = "Information", icon = icon("info")),
      menuItem("2. Equilibrium Constants Selection", tabName = "Inputs_EQ", icon = icon("edit")),
      menuItem("3. Simulation Conditions Selection", tabName = "Inputs", icon = icon("edit")),
      menuItem("4. Lead Hydroxide", tabName = "Lead_Hydroxide", icon = icon("line-chart")),
      menuItem("5. Cerussite", tabName = "Cerussite", icon = icon("line-chart")),
      menuItem("6. Hydrocerussite", tabName = "Hydrocerussite", icon = icon("line-chart")),
      menuItem("7. Hydroxypyromorphite", tabName = "Hydroxypyromorphite", icon = icon("line-chart")),
      menuItem("8. Pyromorphite", tabName = "Pyromorphite", icon = icon("line-chart")),
      menuItem("9. Primary Lead Orthophosphate", tabName = "Primary_Lead", icon = icon("line-chart")),
      menuItem("10. Secondary Lead Orthophosphate", tabName = "Secondary_Lead", icon = icon("line-chart")),
      menuItem("11. Tertiary Lead Orthophosphate", tabName = "Tertiary_Lead", icon = icon("line-chart")),
      menuItem("12. Anglesite", tabName = "Anglesite", icon = icon("line-chart")),
      menuItem("13. Laurionite", tabName = "Laurionite", icon = icon("line-chart")),
      menuItem("14. Multiple Solids", tabName = "Multiple", icon = icon("line-chart"))
      )
    ),

  dashboardBody(fluidPage(theme = shinytheme("spacelab"),

    #Define header
    tags$head(tags$style(type = "text/css",

                         #Define progress bar class
                         "#loadmessage {
                         position: fixed;
                         width: 50%;
                         top: 25%;
                         right: 25%;
                         text-align: center;
                         font-weight: bold;
                         font-size: 300%;
                         color: black;
                         padding: 10px;
                         word-wrap: break-word;
                         line-height: 40px;
                         border-style: solid;
                         border-width: large;
                         border-color: black;
                         border-radius: 15px;
                         background-color: #f5f5f5;
                         opacity: 1;
                         z-index: 105;}",

    #Define style for buttons
    ".btn {
    width: 100%;
    word-wrap: break-word;
    white-space: normal;}",

    #Define initial simulation not run warning style
    "#initialsim {
    width: 90%;
    top: 0%;
    left: 5%;
    text-align: center;
    font-weight: bold;
    font-size: 12px;
    color: black;
    padding: 7.5px;
    word-wrap: break-word;
    line-height: 15px;
    border-style: solid;
    border-width: large;
    border-color: black;
    border-radius: 15px;
    background-color: Yellow;
    opacity: 1;
    z-index: 105;}"
    )
    ),

    #Call to display progress bar
    conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                     tags$div("Update in progress...", id = "loadmessage")
                     ),

    tabItems(

      #Information tab content
      tabItem(tabName = "Information",

              theme = shinytheme("spacelab"),

              #Title block
              h3(tags$b("Theoretical Equilibrium Lead Solubility Simulator (TELSS) Source Code")),

              h4("Code version 1.00"),

              h4("Code last updated February 12, 2021"),

              h4("R code implementation by David G. Wahman (wahman.david@epa.gov), United States Environmental Protection Agency"),

              h4("Based on the LEADSOL Fortran code created by Michael R. Schock, United States Environmental Protection Agency"),

              br(),

              h4(tags$u("Disclaimer:")),

              p("The source code was developed by the United States Environmental Protection Agency (EPA).
              No warranty expressed or implied is made regarding the accuracy
              or utility of the system, nor shall the act of distribution constitute any such warranty.  EPA has relinquished
              control of the information and no longer has responsibility to protect the integrity, confidentiality, or availability
              of the information.  Any reference to specific commercial products, processes, or services by service mark,
              trademark, manufacturer, or otherwise does not constitute or imply their endorsement, recommendation, or favoring by
              EPA. The EPA seal and logo shall not be used in any manner to imply endorsement of any commercial product or activity
              by EPA or the United States Government.  The views expressed in this source code do not necessarily represent the views
              or policies of the Agency. Although a reasonable effort has been made to assure that the results obtained are correct,
              this source code is experimental.  Therefore, the author and the EPA are not responsible and assume no liability whatsoever
              for any results or any use made of the results obtained from this source code, nor for any damages or litigation that result
              from the use of the source code for any purpose."),

              br(),

              h4(tags$u("General Information:")),

              p(HTML("The provided source code generates lead solubility plots for the selected conditions at 25 &#0176C.  The tabs
                located on the left panel are used as follows:"),

                tags$ol(
                  tags$li(tags$b("General Information"),
                          " - Provides general summary information about the source code."),
                  tags$li(tags$b("Equilibrium Constants Selection"),
                          " - User selects the equilibrium constants used
                          to simulate lead solubility."),
                  tags$li(tags$b("Simulation Conditions Selection"),
                          " - User selects the water quality parameters and conditions
                          to simulate lead solubility."),
                  tags$li(tags$b("Lead Hydroxide"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with lead hydroxide assumed as the controlling lead solid."),
                  tags$li(tags$b("Cerussite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with cerussite assumed as the controlling lead solid."),
                  tags$li(tags$b("Hydrocerussite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with hydrocerussite assumed as the controlling lead solid."),
                  tags$li(tags$b("Hydroxypyromorphite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with hydroxypyromorphite assumed as the controlling lead solid."),
                  tags$li(tags$b("Pyromorphite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with pyromorphite assumed as the controlling lead solid."),
                  tags$li(tags$b("Primary Lead Orthophosphate"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with primary lead orthophosphate assumed as the controlling lead solid."),
                  tags$li(tags$b("Secondary Lead Orthophosphate"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with secondary lead orthophosphate assumed as the controlling lead solid."),
                  tags$li(tags$b("Tertiary Lead Orthophosphate"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with tertiary lead orthophosphate assumed as the controlling lead solid."),
                  tags$li(tags$b("Anglesite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with anglesite assumed as the controlling lead solid."),
                  tags$li(tags$b("Laurionite"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with laurionite assumed as the controlling lead solid."),
                  tags$li(tags$b("Multiple Solids"),
                          " - Displays solubility plots for the inputed water quality
                          conditions with multiple solids considered as the controlling lead solid.
                          The solids considered in this analysis were selected on the Simulation Conditions Selection tab.")
                  )
                ),

              br(),
              h4(tags$u("References Cited")),

              p("The following are the references cited for equilibrium constants used in the solubility model (links are provided where possible):",

                tags$ol(
                  tags$li(tags$b("Benjamin, M. M. (2002)"),
                          "Water Chemistry, 1st Edition, McGraw-Hill, New York, NY."),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://inis.iaea.org/collection/NCLCollectionStore/_Public/31/009/31009694.pdf?r=1",
                                   "Lothenbach, B., Ochs, M., Wanner, H. & Yui, M. (1999)")),
                          HTML("<i>Thermodynamic Data for the Speciation and Solubility of Pd, Pb, Sn, Sb, Nb and Bi in Aqueous Solution</i>.
                               Japan Nuclear Cycle Development Institute, Ibaraki, Japan.")),

                  tags$li(tags$b("Nasanen, R. & Lindell, E. (1976)"),
                          HTML("Studies on Lead(II) Hydroxide Salts. Part I. The Solubility Product of Pb(OH)Cl, <i>Finnish Chemical Letters</i>, 95.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="http://iupac.org/publications/pac/pdf/2009/pdf/8112x2425.pdf",
                                   "Powell, K.J., Brown, P.L., Byrne, R.H., Gajda, T., Hefter, G., Leuz, A.K., Sjoberg, S. & Wanner, H. (2009)")),
                          HTML("Chemical Speciation of Environmentally Significant Metals with Inorganic Ligands - Part 3: The Pb<sup>2+</sup>,
                          OH<sup>&#8212;</sup>, Cl<sup>&#8212;</sup>, CO<sub>3</sub><sup>2&#8212;</sup>, SO<sub>4</sub><sup>2&#8212;</sup>,
                               and PO<sub>4</sub><sup>3&#8212;</sup> Systems - (IUPAC Technical Report). <i>Pure and Applied Chemistry</i>, 81:12:2425.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://iupac.org/publications/pac/2005/pdf/7704x0739.pdf",
                                   "Powell, K.J., Brown, P.L., Byrne, R.H., Gajda, T., Hefter, G., Sjoberg, S. & Wanner, H. (2005)")),
                          HTML("Chemical Speciation of Environmentally Significant Heavy Metals with Inorganic Ligands - Part 1:
                          The Hg<sup>2+</sup>, Cl<sup>&#8212;</sup>, OH<sup>&#8212;</sup>, CO<sub>3</sub><sup>2&#8212;</sup>,
                          SO<sub>4</sub><sup>2&#8212;</sup>, and PO<sub>4</sub><sup>3&#8212;</sup> Aqueous Systems - (IUPAC Technical Report).
                               <i>Pure and Applied Chemistry</i>, 77:4:739.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://www.waterrf.org/system/files/resource/2019-06/RFR90508_1996_62-725.pdf",
                                   "Schock, M.R., Wagner, I. & Oliphant, R.J. (1996)")),
                          HTML(" Chapter 4 - Corrosion and Solubility of Lead in Drinking Water.
                          <i>Internal Corrosion of Water Distribution Systems, 2nd Edition</i>.
                               American Water Works Association Research Foundation, Denver, CO.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://www.sciencedirect.com/science/article/pii/S0021961416300143",
                                   "Topolska, J., Manecki, M., Bajda, T., Borkiewicz, O. & Budzewski, P. (2016)")),
                          HTML("Solubility of Pyromorphite Pb<sub>5</sub>(PO<sub>4</sub>)<sub>3</sub>Cl at 5-65 &#0176C and Its Experimentally Determined
                          Thermodynamic Parameters. <i>The Journal of Chemical Thermodynamics</i>, 98:282.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://pubs.acs.org/doi/abs/10.1021/es071517e",
                                   "Xie, L. & Giammar, D.E. (2007)")),
                          HTML("Equilibrium Solubility and Dissolution Rate of the Lead Phosphate Chloropyromorphite.
                          <i>Environmental Science & Technology</i>, 41:23:8050.")),

                  tags$li(tags$b(a(target = "_blank",
                                   href="https://www.hindawi.com/journals/jchem/2015/269387/",
                                   "Zhu, Y.N., Zhu, Z.Q., Zhao, X., Liang, Y.P. & Huang, Y.H. (2015)")),
                          HTML("Characterization, Dissolution, and Solubility of Lead Hydroxypyromorphite [Pb<sub>5</sub>(PO<sub>4</sub>)<sub>3</sub>OH]
                               at 25-45 &#0176C. <i>Journal of Chemistry</i>, 2015:269387:1."))
                  )
                )
              ),

      #Equilibrium constants tab content
      tabItem(tabName = "Inputs_EQ",

              theme = shinytheme("spacelab"),

              fluidRow(
                column(6, render_inputs_EQ("A", "B")),
                column(6, render_inputs_EQ("B", "A")))
      ),

      #Simulation conditions tab content
      tabItem(tabName = "Inputs",

              theme = shinytheme("spacelab"),

              fluidRow(
                column(6, render_inputs("A", "B")),
                column(6, render_inputs("B", "A")))
              ),

      #Lead hydroxide tab content
      tabItem(tabName = "Lead_Hydroxide",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Lead Hydroxide", "A_Lead_Hydroxide"),
                render_plot_outputs("B - Lead Hydroxide", "B_Lead_Hydroxide"))
              ),

      #Cerussite tab content
      tabItem(tabName = "Cerussite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Cerussite", "A_Cerussite"),
                render_plot_outputs("B - Cerussite", "B_Cerussite"))
              ),

      #Hydrocerussite tab content
      tabItem(tabName = "Hydrocerussite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Hydrocerussite", "A_Hydrocerussite"),
                render_plot_outputs("B - Hydrocerussite", "B_Hydrocerussite"))
              ),

      #Hydroxypyromorphite tab content
      tabItem(tabName = "Hydroxypyromorphite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Hydroxypyromorphite", "A_Hydroxypyromorphite"),
                render_plot_outputs("B - Hydroxypyromorphite", "B_Hydroxypyromorphite"))
      ),

      #Pyromorphite tab content
      tabItem(tabName = "Pyromorphite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Pyromorphite", "A_Pyromorphite"),
                render_plot_outputs("B - Pyromorphite", "B_Pyromorphite"))
      ),

      #Primary lead orthophosphate tab content
      tabItem(tabName = "Primary_Lead",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Primary Lead Orthophosphate", "A_Primary_Lead"),
                render_plot_outputs("B - Primary Lead Orthophosphate", "B_Primary_Lead"))
              ),

      #Secondary lead orthophosphate tab content
      tabItem(tabName = "Secondary_Lead",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Secondary Lead Orthophosphate", "A_Secondary_Lead"),
                render_plot_outputs("B - Secondary Lead Orthophosphate", "B_Secondary_Lead"))
              ),

      #Tertiary lead orthophosphate tab content
      tabItem(tabName = "Tertiary_Lead",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Teritary Lead Orthophosphate", "A_Tertiary_Lead"),
                render_plot_outputs("B - Teritary Lead Orthophosphate", "B_Tertiary_Lead"))
              ),

      #Anglesite
      tabItem(tabName = "Anglesite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Anglesite", "A_Anglesite"),
                render_plot_outputs("B - Anglesite", "B_Anglesite"))
              ),

      #Laurionite
      tabItem(tabName = "Laurionite",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Laurionite", "A_Laurionite"),
                render_plot_outputs("B - Laurionite", "B_Laurionite"))
      ),

      #Multiple solids tab content
      tabItem(tabName = "Multiple",

              theme = shinytheme("spacelab"),

              #Layout for simulation conditions and plots
              fluidRow(
                render_plot_outputs("A - Multiple Solids", "A_Multiple"),
                render_solid_outputs("A", "A_Multiple_control"),
                render_plot_outputs("B - Multiple Solids", "B_Multiple"),
                render_solid_outputs("B", "B_Multiple_control"))
              )
      )
    )
    )
  )

#APPLICATION FUNCTION CALL DEFINITION#######################################################################################
############################################################################################################################
shinyApp(ui = ui, server = server)