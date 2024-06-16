

#####################################################################################################################
# Code written by John Buckell and Thomas Hancock (Health Economics Research Centre                                 #   
# and University of Oxford; Choice Modelling Centre)  for the analysis of stated choice                             #
# data gathered from an online survey for the COPPER project using Apollo package                                   #
#                                                                                                                   #
# The code is intended to help members of the research group understand the choice models that we used.             #
# see also http://www.apollochoicemodelling.com/index.html                                                          #
#####################################################################################################################



# ################################################################# #
#### LOAD LIBRARY AND DEFINE CORE SETTINGS                       ####
# ################################################################# #
### Load libraries
library(apollo)
library(readr)
library(tidyverse)

### Initialise code
apollo_initialise()

### Set core controls
apollo_control = list(
  modelName ="COPPER_DCE1_DCE2_mixedMNL_hybrid_3LV_norm_weighted_wtp_lu_trial30",# Make sure to use a new name for every model
  indivID   ="RID",  # Name of column in the database with each individual's ID
  modelDescr ="COPPER_DCE1_DCE2_mixedMNL_hybrid_3LV_norm_weighted_wtp_lu_trial30",
  mixing    = TRUE, # TRUE for models that include random parameters
  weights   ="post_srat_weights", # weighting variable if required
  nCores    = 40,    # Number of cores to use in estimation
  seed_draws= 13    # seed for random draw generation
)

database = read.csv("[insert your filename here].csv")

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

apollo_beta=c(
  left_to_right_bias_dce1 =0,
  tax_asc =0,
  subsidy_asc_midpoint =-0.112553830608875,
  subsidy_asc_range =1.10071167072456,
  subsidy_asc_prime =0.11396885312028,
  subsidy_asc_over65 =0,
  subsidy_asc_under30 =0,
  subsidy_asc_female =0.232497033915372,
  subsidy_asc_nonwhite =0,
  subsidy_asc_heduc =0,
  subsidy_asc_urban =0,
  subsidy_lambda_cc_belief =0,
  subsidy_lambda_meat_free_diet =0,
  subsidy_lambda_govt_int_health =-0.35820314524424,
  level_of_tax_dce1_midpoint =-4.67848604715652,
  level_of_tax_dce1_range =0,
  lambda_income_tax =-0.231048449072269,
  level_of_tax_dce1_over65 =0,
  level_of_tax_dce1_under30 =-0.836335906680298,
  level_of_tax_dce1_female =0,
  level_of_tax_dce1_nonwhite =0,
  level_of_tax_dce1_heduc =0,
  level_of_tax_dce1_urban =0,
  level_of_tax_dce1_lambda_cc_belief =0,
  level_of_tax_dce1_lambda_meat_free_diet =0,
  level_of_tax_dce1_lambda_govt_int_health =0,
  level_of_subsidy_midpoint =0.00194599799174165,
  level_of_subsidy_range =0,
  level_of_subsidy_over65 =0,
  level_of_subsidy_under30 =0,
  level_of_subsidy_female =0,
  level_of_subsidy_nonwhite =0,
  level_of_subsidy_heduc =0,
  level_of_subsidy_urban =-0.00863725101702796,
  level_of_subsidy_lambda_cc_belief =0,
  level_of_subsidy_lambda_meat_free_diet =0,
  level_of_subsidy_lambda_govt_int_health =-0.00319503010937902,
  tax_sugary_drinks =0,
  tax_unhealthy_foods_midpoint =51.4206782005257,
  tax_unhealthy_foods_range =0,
  tax_unhealthy_foods_over65 =0,
  tax_unhealthy_foods_under30 =45.5378968681265,
  tax_unhealthy_foods_female =0,
  tax_unhealthy_foods_nonwhite =0,
  tax_unhealthy_foods_heduc =0,
  tax_unhealthy_foods_urban =0,
  tax_unhealthy_foods_lambda_cc_belief =-44.0413065968641,
  tax_unhealthy_foods_lambda_meat_free_diet =0,
  tax_unhealthy_foods_lambda_govt_int_health =0,
  tax_high_sugar_foods_midpoint =52.473149389983,
  tax_high_sugar_foods_range =0,
  tax_high_sugar_foods_over65 =0,
  tax_high_sugar_foods_under30 =0,
  tax_high_sugar_foods_female =0,
  tax_high_sugar_foods_nonwhite =0,
  tax_high_sugar_foods_heduc =0,
  tax_high_sugar_foods_urban =0,
  tax_high_sugar_foods_lambda_cc_belief =-35.9912055987752,
  tax_high_sugar_foods_lambda_meat_free_diet =-21.6191596573176,
  tax_high_sugar_foods_lambda_govt_int_health =0,
  tax_high_salt_foods_midpoint =41.9349790809051,
  tax_high_salt_foods_range =0,
  tax_high_salt_foods_over65 =0,
  tax_high_salt_foods_under30 =0,
  tax_high_salt_foods_female =0,
  tax_high_salt_foods_nonwhite =-43.5243871734302,
  tax_high_salt_foods_heduc =0,
  tax_high_salt_foods_urban =0,
  tax_high_salt_foods_lambda_cc_belief =-37.6518147293227,
  tax_high_salt_foods_lambda_meat_free_diet =-28.8486328978759,
  tax_high_salt_foods_lambda_govt_int_health =0,
  subsidise_high_fibre_foods =0,
  subsidise_fresh_produce_midpoint =9.27682118881475,
  subsidise_fresh_produce_range =39.587871306885,
  subsidise_fresh_produce_over65 =-24.032176837235,
  subsidise_fresh_produce_under30 =0,
  subsidise_fresh_produce_female =0,
  subsidise_fresh_produce_nonwhite =0,
  subsidise_fresh_produce_heduc =0,
  subsidise_fresh_produce_urban =0,
  subsidise_fresh_produce_lambda_cc_belief =0,
  subsidise_fresh_produce_lambda_meat_free_diet =0,
  subsidise_fresh_produce_lambda_govt_int_health =0,
  policy_affects_general_population =0,
  tax_affects_high_income_midpoint =-9.58919369621217,
  tax_affects_high_income_range =-80.0304065519287,
  tax_affects_high_income_over65 =0,
  tax_affects_high_income_under30 =51.010236423807,
  tax_affects_high_income_female =-29.1689440216251,
  tax_affects_high_income_nonwhite =0,
  tax_affects_high_income_heduc =-23.9820564401487,
  tax_affects_high_income_urban =0,
  tax_affects_high_income_lambda_cc_belief =27.7195869145908,
  tax_affects_high_income_lambda_meat_free_diet =0,
  tax_affects_high_income_lambda_govt_int_health =-23.5995555351573,
  subsidy_affects_low_income_midpoint =38.0812967134772,
  subsidy_affects_low_income_range =0,
  subsidy_affects_low_income_over65 =0,
  subsidy_affects_low_income_under30 =0,
  subsidy_affects_low_income_female =-21.1355670479597,
  subsidy_affects_low_income_nonwhite =28.8816345760065,
  subsidy_affects_low_income_heduc =-33.8599471969283,
  subsidy_affects_low_income_urban =0,
  subsidy_affects_low_income_lambda_cc_belief =0,
  subsidy_affects_low_income_lambda_meat_free_diet =0,
  subsidy_affects_low_income_lambda_govt_int_health =0,
  subsidy_affects_families_children_midpoint =19.1542577269828,
  subsidy_affects_families_children_range =0,
  subsidy_affects_families_children_over65 =0,
  subsidy_affects_families_children_under30 =0,
  subsidy_affects_families_children_female =0,
  subsidy_affects_families_children_nonwhite =0,
  subsidy_affects_families_children_heduc =-69.1034707516321,
  subsidy_affects_families_children_urban =-29.2077949359882,
  subsidy_affects_families_children_lambda_cc_belief =0,
  subsidy_affects_families_children_lambda_meat_free_diet =18.944347504729,
  subsidy_affects_families_children_lambda_govt_int_health =0,
  subsidy_affects_overweight_obese_midpoint =-11.6001438502557,
  subsidy_affects_overweight_obese_range =46.3286973199886,
  subsidy_affects_overweight_obese_over65 =36.7140672850706,
  subsidy_affects_overweight_obese_under30 =0,
  subsidy_affects_overweight_obese_female =0,
  subsidy_affects_overweight_obese_nonwhite =0,
  subsidy_affects_overweight_obese_heduc =-47.3535334403127,
  subsidy_affects_overweight_obese_urban =0,
  subsidy_affects_overweight_obese_lambda_cc_belief =0,
  subsidy_affects_overweight_obese_lambda_meat_free_diet =0,
  subsidy_affects_overweight_obese_lambda_govt_int_health =0,
  general_taxation =0,
  NHS_social_care_tax_midpoint =-43.4064795017934,
  NHS_social_care_tax_range =0,
  NHS_social_care_tax_over65 =0,
  NHS_social_care_tax_under30 =0,
  NHS_social_care_tax_female =40.2539020719702,
  NHS_social_care_tax_nonwhite =0,
  NHS_social_care_tax_heduc =0,
  NHS_social_care_tax_urban =0,
  NHS_social_care_tax_lambda_cc_belief =30.3336207311992,
  NHS_social_care_tax_lambda_meat_free_diet =0,
  NHS_social_care_tax_lambda_govt_int_health =0,
  exercise_vouchers_tax_midpoint =-18.8440430718146,
  exercise_vouchers_tax_range =0,
  exercise_vouchers_tax_over65 =-50.5426926562383,
  exercise_vouchers_tax_under30 =-56.4805722060846,
  exercise_vouchers_tax_female =45.574281138391,
  exercise_vouchers_tax_nonwhite =56.692256501294,
  exercise_vouchers_tax_heduc =-37.5105158743934,
  exercise_vouchers_tax_urban =0,
  exercise_vouchers_tax_lambda_cc_belief =0,
  exercise_vouchers_tax_lambda_meat_free_diet =0,
  exercise_vouchers_tax_lambda_govt_int_health =0,
  NHS_social_care_subsidy_midpoint =-34.9349001577015,
  NHS_social_care_subsidy_range =0,
  NHS_social_care_subsidy_over65 =0,
  NHS_social_care_subsidy_under30 =0,
  NHS_social_care_subsidy_female =0,
  NHS_social_care_subsidy_nonwhite =28.0785409576281,
  NHS_social_care_subsidy_heduc =0,
  NHS_social_care_subsidy_urban =0,
  NHS_social_care_subsidy_lambda_cc_belief =0,
  NHS_social_care_subsidy_lambda_meat_free_diet =0,
  NHS_social_care_subsidy_lambda_govt_int_health =0,
  exercise_vouchers_subsidy_midpoint =-1.58293242197385,
  exercise_vouchers_subsidy_range =-37.0607096433912,
  exercise_vouchers_subsidy_over65 =0,
  exercise_vouchers_subsidy_under30 =0,
  exercise_vouchers_subsidy_female =0,
  exercise_vouchers_subsidy_nonwhite =0,
  exercise_vouchers_subsidy_heduc =31.379379320927,
  exercise_vouchers_subsidy_urban =39.1503191938837,
  exercise_vouchers_subsidy_lambda_cc_belief =0,
  exercise_vouchers_subsidy_lambda_meat_free_diet =-23.7539034416638,
  exercise_vouchers_subsidy_lambda_govt_int_health =0,
  left_to_right_bias_dce2 =0.0756724006786385,
  level_of_tax_midpoint =-4.84347128536585,
  level_of_tax_range =1.65024046001962,
  lambda_income =-0.420402044211007,
  level_of_tax_over65 =0,
  level_of_tax_under30 =-0.634797811011058,
  level_of_tax_female =0,
  level_of_tax_nonwhite =0,
  level_of_tax_heduc =0,
  level_of_tax_urban =0,
  level_of_tax_lambda_cc_belief =0,
  level_of_tax_lambda_meat_free_diet =1.01769988396254,
  level_of_tax_lambda_govt_int_health =-0.156036756159012,
  tax_water_use =0,
  tax_meat_dairy_midpoint =22.4591977571907,
  tax_meat_dairy_range =0,
  tax_meat_dairy_over65 =0,
  tax_meat_dairy_under30 =0,
  tax_meat_dairy_female =0,
  tax_meat_dairy_nonwhite =0,
  tax_meat_dairy_heduc =-6.49509734588251,
  tax_meat_dairy_urban =0,
  tax_meat_dairy_lambda_cc_belief =0,
  tax_meat_dairy_lambda_meat_free_diet =-14.8178351832749,
  tax_meat_dairy_lambda_govt_int_health =8.61777308504828,
  tax_greenhouse_gases_midpoint =31.2832802303082,
  tax_greenhouse_gases_range =3.76244110804812,
  tax_greenhouse_gases_over65 =0,
  tax_greenhouse_gases_under30 =0,
  tax_greenhouse_gases_female =0,
  tax_greenhouse_gases_nonwhite =0,
  tax_greenhouse_gases_heduc =-8.11821346055452,
  tax_greenhouse_gases_urban =0,
  tax_greenhouse_gases_lambda_cc_belief =5.0755840156661,
  tax_greenhouse_gases_lambda_meat_free_diet =-28.3166314315617,
  tax_greenhouse_gases_lambda_govt_int_health =7.13286365289513,
  tax_plastic_production_midpoint =45.0496829390505,
  tax_plastic_production_range =6.63725515993421,
  tax_plastic_production_over65 =0,
  tax_plastic_production_under30 =0,
  tax_plastic_production_female =0,
  tax_plastic_production_nonwhite =-18.3048759291552,
  tax_plastic_production_heduc =0,
  tax_plastic_production_urban =0,
  tax_plastic_production_lambda_cc_belief =5.27771876925289,
  tax_plastic_production_lambda_meat_free_diet =-31.1915721931157,
  tax_plastic_production_lambda_govt_int_health =8.69555239827327,
  low_impact_foods_midpoint =9.25278843753324,
  low_impact_foods_range =0,
  low_impact_foods_over65 =0,
  low_impact_foods_under30 =0,
  low_impact_foods_female =13.0501145063913,
  low_impact_foods_nonwhite =0,
  low_impact_foods_heduc =-4.78206065991382,
  low_impact_foods_urban =0,
  low_impact_foods_lambda_cc_belief =0,
  low_impact_foods_lambda_meat_free_diet =-15.1461699640379,
  low_impact_foods_lambda_govt_int_health =6.50660666667403,
  UK_environment_midpoint =3.43573986920064,
  UK_environment_range =0,
  UK_environment_over65 =0,
  UK_environment_under30 =0,
  UK_environment_female =8.44619350470017,
  UK_environment_nonwhite =0,
  UK_environment_heduc =-5.12372299199082,
  UK_environment_urban =0,
  UK_environment_lambda_cc_belief =4.56898803013108,
  UK_environment_lambda_meat_free_diet =-12.4824100783427,
  UK_environment_lambda_govt_int_health =0,
  donate_midpoint =-75.2282576633457,
  donate_range =-5.88508233875841,
  donate_over65 =-6.80297234090983,
  donate_under30 =11.741926621333,
  donate_female =0,
  donate_nonwhite =36.5203843673683,
  donate_heduc =0,
  donate_urban =-11.8600317597518,
  donate_lambda_cc_belief =0,
  donate_lambda_meat_free_diet =30.2295684473147,
  donate_lambda_govt_int_health =-10.8019117620273,
  gamma_cc_belief_over65 =1.0125244517148,
  gamma_cc_belief_under30 =0.331731155769418,
  gamma_cc_belief_female =1.18469356949369,
  gamma_cc_belief_nonwhite =0.211996394487185,
  gamma_cc_belief_heduc =0.720905420994021,
  gamma_cc_belief_urban =0.353958870509453,
  delta_believe_cc =-5,
  zeta_believe_cc =11.6069132224085,
  zeta_worry_cc =4.82388942846438,
  tau_worry_cc_1 =2.52177786320649,
  tau_worry_cc_2 =4.73258945714665,
  tau_worry_cc_3 =9.04714905734979,
  tau_worry_cc_4 =11.6840962663365,
  zeta_responsibility_cc =3.94665357768346,
  sigma_responsibility_cc =1.65590792157797,
  zeta_pay_env_friendly_prods =1.80089165965251,
  tau_pay_env_friendly_prods_1 =0.0327749517557665,
  tau_pay_env_friendly_prods_2 =1.60364270605681,
  tau_pay_env_friendly_prods_3 =3.15919637265826,
  tau_pay_env_friendly_prods_4 =6.14442977954111,
  zeta_govt_int_env =1.55589975949343,
  tau_govt_int_env_1 =-2.22326958336306,
  tau_govt_int_env_2 =-1.20757214906753,
  tau_govt_int_env_3 =0.50347913512999,
  tau_govt_int_env_4 =3.38442245138022,
  gamma_meat_free_diet_over65 =0.281007581351214,
  gamma_meat_free_diet_under30 =-0.302247000742793,
  gamma_meat_free_diet_female =0,
  gamma_meat_free_diet_nonwhite =-0.527385023992552,
  gamma_meat_free_diet_heduc =0,
  gamma_meat_free_diet_urban =0,
  delta_veg_vegan =-8.01232090329373,
  zeta_veg_vegan =-5.34617925486305,
  delta_eats_Dairy =3.1478242793864,
  zeta_eats_Dairy =1.92840851793123,
  delta_eats_RedMeat =1.89134953096108,
  zeta_eats_RedMeat =2.05564293429882,
  delta_eats_WhiteMeat =2.97533357748345,
  zeta_eats_WhiteMeat =3.05047664513609,
  delta_eats_Fish =1.67554507377944,
  zeta_eats_Fish =1.52730446350216,
  delta_eats_NoneOfThese =-8.95318213437705,
  zeta_eats_NoneOfThese =-4.13451604972688,
  gamma_gov_health_over65 =0,
  gamma_gov_health_under30 =-0.145658838787448,
  gamma_gov_health_female =0,
  gamma_gov_health_nonwhite =-0.144102205837348,
  gamma_gov_health_heduc =0.279711999232801,
  gamma_gov_health_urban =0,
  zeta_pay_env_friendly_prods2 =-0.212207866102678,
  zeta_govt_int_health =1.36187988909438,
  tau_govt_int_health_1 =-3.63979424238257,
  tau_govt_int_health_2 =-2.1497672661006,
  tau_govt_int_health_3 =-0.463078380898068,
  tau_govt_int_health_4 =2.21573232000753,
  zeta_govt_int_env2 =0.724708824672437,
  zeta_govt_tax_sugary_drinks =2.94507236403544,
  tau_govt_tax_sugary_drinks_1 =-4.69904420000931,
  tau_govt_tax_sugary_drinks_2 =-2.6094340403137,
  tau_govt_tax_sugary_drinks_3 =-0.407499505454978,
  tau_govt_tax_sugary_drinks_4 =3.39237081252905,
  zeta_govt_tax_sugary_drinks_health_spending =3.79515037052656,
  tau_govt_tax_sugary_drinks_health_spending_1 =-6.17463880673529,
  tau_govt_tax_sugary_drinks_health_spending_2 =-3.95801231814805,
  tau_govt_tax_sugary_drinks_health_spending_3 =-1.35659116448978,
  tau_govt_tax_sugary_drinks_health_spending_4 =3.19457008133031
)
apollo_fixed=names(apollo_beta)[apollo_beta==0|apollo_beta==1|apollo_beta==-5]


### Set parameters for generating draws
apollo_draws = list(
  interDrawsType="mlhs", 
  interNDraws=500,          
  interUnifDraws=c(
    "draws_unif_level_of_tax_dce1",
    "draws_unif_level_of_tax"
  ),            
  interNormDraws=c("eta_cc","eta_mfd","eta_gh",
                   "draws_norm_subsidy_asc",
                   "draws_norm_level_of_subsidy",
                   "draws_norm_tax_unhealthy_foods",
                   "draws_norm_tax_high_sugar_foods",
                   "draws_norm_tax_high_salt_foods",
                   "draws_norm_subsidise_fresh_produce",
                   "draws_norm_tax_affects_high_income",
                   "draws_norm_subsidy_affects_low_income",
                   "draws_norm_subsidy_affects_families_children",
                   "draws_norm_subsidy_affects_overweight_obese",
                   "draws_norm_NHS_social_care_tax",
                   "draws_norm_exercise_vouchers_tax",
                   "draws_norm_NHS_social_care_subsidy",
                   "draws_norm_exercise_vouchers_subsidy",
                   "draws_norm_tax_meat_dairy",
                   "draws_norm_tax_greenhouse_gases",
                   "draws_norm_tax_plastic_production",
                   "draws_norm_low_impact_foods",
                   "draws_norm_UK_environment",
                   "draws_norm_donate"), 
  
  intraDrawsType="",
  intraNDraws=0,          
  intraUnifDraws=c(),     
  intraNormDraws=c()      
)

### Create random parameters
apollo_randCoeff=function(apollo_beta, apollo_inputs){
  randcoeff = list()
  
  randcoeff[["LV_CC"]] = gamma_cc_belief_under30 * (age<30) + gamma_cc_belief_over65 * (age>65) + gamma_cc_belief_female*(gender=="Female") + gamma_cc_belief_nonwhite * (Ethnicity!="White") + gamma_cc_belief_heduc * (higher_education==1) + gamma_cc_belief_urban * (RuralUrban==1) + eta_cc
  randcoeff[["LV_MFD"]] = gamma_meat_free_diet_under30 * (age<30) + gamma_meat_free_diet_over65 * (age>65) + gamma_meat_free_diet_female*(gender=="Female") + gamma_meat_free_diet_nonwhite * (Ethnicity!="White") + gamma_meat_free_diet_heduc * (higher_education==1) + gamma_meat_free_diet_urban * (RuralUrban==1) + eta_mfd
  randcoeff[["LV_gov_health"]] = gamma_gov_health_under30 * (age<30) + gamma_gov_health_over65 * (age>65) + gamma_gov_health_female*(gender=="Female") + gamma_gov_health_nonwhite * (Ethnicity!="White") + gamma_gov_health_heduc * (higher_education==1) + gamma_gov_health_urban * (RuralUrban==1)  + eta_gh
  randcoeff[["level_of_tax_dce1_coeffs"]]= exp(
    level_of_tax_dce1_midpoint + level_of_tax_dce1_range *( draws_unif_level_of_tax_dce1 ) + level_of_tax_dce1_over65                 * (age>65) + level_of_tax_dce1_under30                 * (age<30)  + level_of_tax_dce1_female * (gender=="Female") + level_of_tax_dce1_nonwhite * (Ethnicity!="White") + level_of_tax_dce1_heduc * (higher_education==1) + level_of_tax_dce1_urban * (RuralUrban==1) +
      level_of_tax_dce1_lambda_govt_int_health * (
        gamma_gov_health_under30 * (age<30) + gamma_gov_health_over65 * (age>65) + gamma_gov_health_female*(gender=="Female") + gamma_gov_health_nonwhite * (Ethnicity!="White") + gamma_gov_health_heduc * (higher_education==1) + gamma_gov_health_urban * (RuralUrban==1)  + eta_gh
      ) +
      level_of_tax_dce1_lambda_cc_belief * (
        gamma_cc_belief_under30 * (age<30) + gamma_cc_belief_over65 * (age>65) + gamma_cc_belief_female*(gender=="Female") + gamma_cc_belief_nonwhite * (Ethnicity!="White") + gamma_cc_belief_heduc * (higher_education==1) + gamma_cc_belief_urban * (RuralUrban==1) + eta_cc
      ) + 
      level_of_tax_dce1_lambda_meat_free_diet * (
        gamma_meat_free_diet_under30 * (age<30) + gamma_meat_free_diet_over65 * (age>65) + gamma_meat_free_diet_female*(gender=="Female") + gamma_meat_free_diet_nonwhite * (Ethnicity!="White") + gamma_meat_free_diet_heduc * (higher_education==1) + gamma_meat_free_diet_urban * (RuralUrban==1) + eta_mfd
      )
  )
  randcoeff[["level_of_tax_coeffs"]]= exp(
    level_of_tax_midpoint + level_of_tax_range * (draws_unif_level_of_tax )  + level_of_tax_over65 * (age>65) + level_of_tax_under30 * (age<30) + level_of_tax_female * (gender=="Female") + level_of_tax_nonwhite * (Ethnicity!="White") + level_of_tax_heduc * (higher_education==1) + level_of_tax_urban * (RuralUrban==1)
    + level_of_tax_lambda_cc_belief * (
      gamma_cc_belief_under30 * (age<30) + gamma_cc_belief_over65 * (age>65) + gamma_cc_belief_female*(gender=="Female") + gamma_cc_belief_nonwhite * (Ethnicity!="White") + gamma_cc_belief_heduc * (higher_education==1) + gamma_cc_belief_urban * (RuralUrban==1) + eta_cc
    )
    + level_of_tax_lambda_govt_int_health * (
      gamma_gov_health_under30 * (age<30) + gamma_gov_health_over65 * (age>65) + gamma_gov_health_female*(gender=="Female") + gamma_gov_health_nonwhite * (Ethnicity!="White") + gamma_gov_health_heduc * (higher_education==1) + gamma_gov_health_urban * (RuralUrban==1)  + eta_gh
    ) +
      level_of_tax_lambda_meat_free_diet * (
        gamma_meat_free_diet_under30 * (age<30) + gamma_meat_free_diet_over65 * (age>65) + gamma_meat_free_diet_female*(gender=="Female") + gamma_meat_free_diet_nonwhite * (Ethnicity!="White") + gamma_meat_free_diet_heduc * (higher_education==1) + gamma_meat_free_diet_urban * (RuralUrban==1) + eta_mfd
      )
  )
  randcoeff[["subsidy_asc"]]= subsidy_asc_midpoint + subsidy_asc_range *( draws_norm_subsidy_asc )
  randcoeff[["level_of_subsidy"]]= level_of_subsidy_midpoint + level_of_subsidy_range *( draws_norm_level_of_subsidy )
  randcoeff[["tax_unhealthy_foods"]]= tax_unhealthy_foods_midpoint + tax_unhealthy_foods_range *( draws_norm_tax_unhealthy_foods )
  randcoeff[["tax_high_sugar_foods"]]= tax_high_sugar_foods_midpoint + tax_high_sugar_foods_range *( draws_norm_tax_high_sugar_foods )
  randcoeff[["tax_high_salt_foods"]]= tax_high_salt_foods_midpoint + tax_high_salt_foods_range * (draws_norm_tax_high_salt_foods )
  randcoeff[["subsidise_fresh_produce"]]= subsidise_fresh_produce_midpoint + subsidise_fresh_produce_range * (draws_norm_subsidise_fresh_produce )
  randcoeff[["tax_affects_high_income"]]= tax_affects_high_income_midpoint + tax_affects_high_income_range * (draws_norm_tax_affects_high_income )
  randcoeff[["subsidy_affects_low_income"]]= subsidy_affects_low_income_midpoint + subsidy_affects_low_income_range * (draws_norm_subsidy_affects_low_income )
  randcoeff[["subsidy_affects_families_children"]]= subsidy_affects_families_children_midpoint + subsidy_affects_families_children_range * (draws_norm_subsidy_affects_families_children )
  randcoeff[["subsidy_affects_overweight_obese"]]= subsidy_affects_overweight_obese_midpoint + subsidy_affects_overweight_obese_range * (draws_norm_subsidy_affects_overweight_obese )
  randcoeff[["NHS_social_care_tax"]]= NHS_social_care_tax_midpoint + NHS_social_care_tax_range * (draws_norm_NHS_social_care_tax )
  randcoeff[["exercise_vouchers_tax"]]= exercise_vouchers_tax_midpoint + exercise_vouchers_tax_range * (draws_norm_exercise_vouchers_tax )
  randcoeff[["NHS_social_care_subsidy"]]= NHS_social_care_subsidy_midpoint + NHS_social_care_subsidy_range * (draws_norm_NHS_social_care_subsidy )
  randcoeff[["exercise_vouchers_subsidy"]]= exercise_vouchers_subsidy_midpoint + exercise_vouchers_subsidy_range * (draws_norm_exercise_vouchers_subsidy )
  randcoeff[["tax_meat_dairy"]]= tax_meat_dairy_midpoint + tax_meat_dairy_range * (draws_norm_tax_meat_dairy )
  randcoeff[["tax_greenhouse_gases"]]= tax_greenhouse_gases_midpoint + tax_greenhouse_gases_range * (draws_norm_tax_greenhouse_gases )
  randcoeff[["tax_plastic_production"]]= tax_plastic_production_midpoint + tax_plastic_production_range * (draws_norm_tax_plastic_production )
  randcoeff[["low_impact_foods"]]= low_impact_foods_midpoint + low_impact_foods_range * (draws_norm_low_impact_foods )
  randcoeff[["UK_environment"]]= UK_environment_midpoint + UK_environment_range * (draws_norm_UK_environment )
  randcoeff[["donate"]]= donate_midpoint + donate_range * (draws_norm_donate)
  return(randcoeff)
}
apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()

  ##### Likelihood of indicators
  ## binary
  V_lv1 = list()
  V_lv1[['alt1']]  = delta_believe_cc + zeta_believe_cc*LV_CC
  V_lv1[['alt0']]  = 0
  mnl_settings_lv1 = list(
    alternatives = c(alt0=0, alt1=1),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Belive_climate_change,
    componentName = "cc_belief",
    V            = V_lv1
  )
  P[['cc_belief']] = apollo_mnl(mnl_settings_lv1, functionality) 
  
  V_lv2 = list()
  V_lv2[['alt1']]  = delta_veg_vegan + zeta_veg_vegan*LV_MFD
  V_lv2[['alt0']]  = 0
  mnl_settings_lv2 = list(
    alternatives = c(alt0=0, alt1=1),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = veg_vegan,
    componentName = "vegvegan",
    V            = V_lv2
  )
  P[['vegvegan']] = apollo_mnl(mnl_settings_lv2, functionality) 
  
  V_lv3 = list()
  V_lv3[['alt1']]  = delta_eats_Dairy + zeta_eats_Dairy*LV_MFD
  V_lv3[['alt0']]  = 0
  mnl_settings_lv3 = list(
    alternatives = c(alt0=1, alt1=2),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Eats_Dairy,
    componentName = "EatsDairy",
    V            = V_lv3
  )
  P[['EatsDairy']] = apollo_mnl(mnl_settings_lv3, functionality) 
  
  V_lv4 = list()
  V_lv4[['alt1']]  = delta_eats_RedMeat + zeta_eats_RedMeat*LV_MFD
  V_lv4[['alt0']]  = 0
  mnl_settings_lv4 = list(
    alternatives = c(alt0=1, alt1=2),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Eats_RedMeat,
    componentName = "EatsRedMeat",
    V            = V_lv4
  )
  P[['EatsRedMeat']] = apollo_mnl(mnl_settings_lv4, functionality) 
  
  V_lv5 = list()
  V_lv5[['alt1']]  = delta_eats_WhiteMeat + zeta_eats_WhiteMeat*LV_MFD
  V_lv5[['alt0']]  = 0
  mnl_settings_lv5 = list(
    alternatives = c(alt0=1, alt1=2),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Eats_WhiteMeat,
    componentName = "EatsWhiteMeat",
    V            = V_lv5
  )
  P[['EatsWhiteMeat']] = apollo_mnl(mnl_settings_lv5, functionality) 
  
  V_lv6 = list()
  V_lv6[['alt1']]  = delta_eats_Fish + zeta_eats_Fish*LV_MFD
  V_lv6[['alt0']]  = 0
  mnl_settings_lv6 = list(
    alternatives = c(alt0=1, alt1=2),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Eats_Fish,
    componentName = "EatsFish",
    V            = V_lv6
  )
  P[['EatsFish']] = apollo_mnl(mnl_settings_lv6, functionality) 
  
  V_lv7 = list()
  V_lv7[['alt1']]  = delta_eats_NoneOfThese + zeta_eats_NoneOfThese*LV_MFD
  V_lv7[['alt0']]  = 0
  mnl_settings_lv7 = list(
    alternatives = c(alt0=1, alt1=2),
    avail        = list(alt0=1, alt1=1),
    rows         = SEQ==1,
    choiceVar    = Eats_NoneOfThese,
    componentName = "EatsNoneOfThese",
    V            = V_lv7
  )
  P[['EatsNoneOfThese']] = apollo_mnl(mnl_settings_lv7, functionality) 
  
  
  ## ordered 
  ol_settings1 = list(outcomeOrdered = Worry_about_climate_change, 
                      V              = zeta_worry_cc*LV_CC, 
                      tau            = list(tau_worry_cc_1, tau_worry_cc_2, tau_worry_cc_3, tau_worry_cc_4),
                      rows           = SEQ==1,
                      componentName  = "cc_worry")
  P[["cc_worry"]]     = apollo_ol(ol_settings1, functionality)

  ol_settings3 = list(outcomeOrdered = Would_pay_env_friendly_products, 
                      V              = zeta_pay_env_friendly_prods*LV_CC + zeta_pay_env_friendly_prods2*LV_MFD, 
                      tau            = list(tau_pay_env_friendly_prods_1, tau_pay_env_friendly_prods_2, tau_pay_env_friendly_prods_3, tau_pay_env_friendly_prods_4),
                      rows           = SEQ==1,
                      componentName  = "pay_env_friendly_prods")
  P[["pay_env_friendly_prods"]]     = apollo_ol(ol_settings3, functionality)
  
  ol_settings4 = list(outcomeOrdered = Govt_should_intervene_health, 
                      V              = zeta_govt_int_health*LV_gov_health, 
                      tau            = list(tau_govt_int_health_1, tau_govt_int_health_2, tau_govt_int_health_3, tau_govt_int_health_4),
                      rows           = SEQ==1,
                      componentName  = "govt_int_health")
  P[["govt_int_health"]]     = apollo_ol(ol_settings4, functionality)
  
  ol_settings6 = list(outcomeOrdered = Govt_should_intervene_environment, 
                      V              = zeta_govt_int_env*LV_CC + zeta_govt_int_env2*LV_gov_health, 
                      tau            = list(tau_govt_int_env_1, tau_govt_int_env_2, tau_govt_int_env_3, tau_govt_int_env_4),
                      rows           = SEQ==1,
                      componentName  = "govt_int_env")
  P[["govt_int_env"]]     = apollo_ol(ol_settings6, functionality)
  
  ol_settings7 = list(outcomeOrdered = Govt_should_tax_sugary_drinks, 
                      V              = zeta_govt_tax_sugary_drinks*LV_gov_health, 
                      tau            = list(tau_govt_tax_sugary_drinks_1, tau_govt_tax_sugary_drinks_2, tau_govt_tax_sugary_drinks_3, tau_govt_tax_sugary_drinks_4),
                      rows           = SEQ==1,
                      componentName  = "govt_tax_sugary_drinks")
  P[["govt_tax_sugary_drinks"]]     = apollo_ol(ol_settings7, functionality)
  
  ol_settings8 = list(outcomeOrdered = Govt_should_tax_sugary_drinks_for_health_spending, 
                      V              = zeta_govt_tax_sugary_drinks_health_spending*LV_gov_health, 
                      tau            = list(tau_govt_tax_sugary_drinks_health_spending_1, tau_govt_tax_sugary_drinks_health_spending_2, tau_govt_tax_sugary_drinks_health_spending_3, tau_govt_tax_sugary_drinks_health_spending_4),
                      rows           = SEQ==1,
                      componentName  = "tax_sugary_drinks_health_spending")
  P[["tax_sugary_drinks_health_spending"]]     = apollo_ol(ol_settings8, functionality)
  
  ## continuous
  normalDensity_settings1 = list(outcomeNormal = Responsibility_for_climate_change, 
                                 xNormal       = zeta_responsibility_cc*LV_CC, 
                                 mu            = 0, 
                                 sigma         = sigma_responsibility_cc, 
                                 rows          = SEQ==1,
                                 componentName = "cc_responsibility")
  P[["cc_responsibility"]]     = apollo_normalDensity(normalDensity_settings1, functionality)
  
  
  ############## DCE 1
  ### build utility function parameters
  subsidy_asc_coeffs = subsidy_asc + subsidy_asc_prime * (prime=="subsidy_prime") + subsidy_asc_over65                       * (age>65) + subsidy_asc_under30                       * (age<30)  + subsidy_asc_female * (gender=="Female") + subsidy_asc_nonwhite * (Ethnicity!="White") + subsidy_asc_heduc * (higher_education==1) + subsidy_asc_urban * (RuralUrban==1)  + subsidy_lambda_govt_int_health * (LV_gov_health) + subsidy_lambda_cc_belief * (LV_CC) + subsidy_lambda_meat_free_diet * (LV_MFD)
  level_of_subsidy_coeffs = level_of_subsidy                                      + level_of_subsidy_over65                  * (age>65) + level_of_subsidy_under30                  * (age<30)  + level_of_subsidy_female * (gender=="Female") + level_of_subsidy_nonwhite * (Ethnicity!="White") + level_of_subsidy_heduc * (higher_education==1) + level_of_subsidy_urban * (RuralUrban==1) + level_of_subsidy_lambda_govt_int_health * (LV_gov_health) + level_of_subsidy_lambda_cc_belief * (LV_CC) + level_of_subsidy_lambda_meat_free_diet * (LV_MFD)
  tax_unhealthy_foods_coeffs = tax_unhealthy_foods                                + tax_unhealthy_foods_over65               * (age>65) + tax_unhealthy_foods_under30               * (age<30)  + tax_unhealthy_foods_female * (gender=="Female") + tax_unhealthy_foods_nonwhite * (Ethnicity!="White") + tax_unhealthy_foods_heduc * (higher_education==1) + tax_unhealthy_foods_urban * (RuralUrban==1) + tax_unhealthy_foods_lambda_govt_int_health * (LV_gov_health) + tax_unhealthy_foods_lambda_cc_belief * (LV_CC) + tax_unhealthy_foods_lambda_meat_free_diet * (LV_MFD)
  tax_high_sugar_foods_coeffs = tax_high_sugar_foods                              + tax_high_sugar_foods_over65              * (age>65) + tax_high_sugar_foods_under30              * (age<30)  + tax_high_sugar_foods_female * (gender=="Female") + tax_high_sugar_foods_nonwhite * (Ethnicity!="White") + tax_high_sugar_foods_heduc * (higher_education==1) + tax_high_sugar_foods_urban * (RuralUrban==1) + tax_high_sugar_foods_lambda_govt_int_health * (LV_gov_health) + tax_high_sugar_foods_lambda_cc_belief * (LV_CC) + tax_high_sugar_foods_lambda_meat_free_diet * (LV_MFD)
  tax_high_salt_foods_coeffs = tax_high_salt_foods                                + tax_high_salt_foods_over65               * (age>65) + tax_high_salt_foods_under30               * (age<30)  + tax_high_salt_foods_female * (gender=="Female") + tax_high_salt_foods_nonwhite * (Ethnicity!="White") + tax_high_salt_foods_heduc * (higher_education==1) + tax_high_salt_foods_urban * (RuralUrban==1) + tax_high_salt_foods_lambda_govt_int_health * (LV_gov_health) + tax_high_salt_foods_lambda_cc_belief * (LV_CC) + tax_high_salt_foods_lambda_meat_free_diet * (LV_MFD)
  subsidise_fresh_produce_coeffs = subsidise_fresh_produce                        + subsidise_fresh_produce_over65           * (age>65) + subsidise_fresh_produce_under30           * (age<30)  + subsidise_fresh_produce_female * (gender=="Female") + subsidise_fresh_produce_nonwhite * (Ethnicity!="White") + subsidise_fresh_produce_heduc * (higher_education==1) + subsidise_fresh_produce_urban * (RuralUrban==1) + subsidise_fresh_produce_lambda_govt_int_health * (LV_gov_health) + subsidise_fresh_produce_lambda_cc_belief * (LV_CC) + subsidise_fresh_produce_lambda_meat_free_diet * (LV_MFD) 
  tax_affects_high_income_coeffs = tax_affects_high_income                        + tax_affects_high_income_over65           * (age>65) + tax_affects_high_income_under30           * (age<30)  + tax_affects_high_income_female * (gender=="Female") + tax_affects_high_income_nonwhite * (Ethnicity!="White") + tax_affects_high_income_heduc * (higher_education==1) + tax_affects_high_income_urban * (RuralUrban==1)   + tax_affects_high_income_lambda_govt_int_health * (LV_gov_health) + tax_affects_high_income_lambda_cc_belief * (LV_CC) +tax_affects_high_income_lambda_meat_free_diet * (LV_MFD) 
  subsidy_affects_low_income_coeffs = subsidy_affects_low_income                  + subsidy_affects_low_income_over65        * (age>65) + subsidy_affects_low_income_under30        * (age<30)  + subsidy_affects_low_income_female * (gender=="Female") + subsidy_affects_low_income_nonwhite * (Ethnicity!="White") + subsidy_affects_low_income_heduc * (higher_education==1) + subsidy_affects_low_income_urban * (RuralUrban==1)  + subsidy_affects_low_income_lambda_govt_int_health * (LV_gov_health) + subsidy_affects_low_income_lambda_cc_belief * (LV_CC) + subsidy_affects_low_income_lambda_meat_free_diet * (LV_MFD) 
  subsidy_affects_families_children_coeffs = subsidy_affects_families_children    + subsidy_affects_families_children_over65 * (age>65) + subsidy_affects_families_children_under30 * (age<30)  + subsidy_affects_families_children_female * (gender=="Female") + subsidy_affects_families_children_nonwhite * (Ethnicity!="White") + subsidy_affects_families_children_heduc * (higher_education==1) + subsidy_affects_families_children_urban * (RuralUrban==1)   + subsidy_affects_families_children_lambda_govt_int_health * (LV_gov_health) + subsidy_affects_families_children_lambda_cc_belief * (LV_CC) + subsidy_affects_families_children_lambda_meat_free_diet * (LV_MFD)  
  subsidy_affects_overweight_obese_coeffs = subsidy_affects_overweight_obese      + subsidy_affects_overweight_obese_over65  * (age>65) + subsidy_affects_overweight_obese_under30  * (age<30)  + subsidy_affects_overweight_obese_female * (gender=="Female") + subsidy_affects_overweight_obese_nonwhite * (Ethnicity!="White") + subsidy_affects_overweight_obese_heduc * (higher_education==1) + subsidy_affects_overweight_obese_urban * (RuralUrban==1)   + subsidy_affects_overweight_obese_lambda_govt_int_health * (LV_gov_health) + subsidy_affects_overweight_obese_lambda_cc_belief * (LV_CC) + subsidy_affects_overweight_obese_lambda_meat_free_diet * (LV_MFD)  
  NHS_social_care_tax_coeffs = NHS_social_care_tax                                + NHS_social_care_tax_over65               * (age>65) + NHS_social_care_tax_under30               * (age<30)  + NHS_social_care_tax_female * (gender=="Female") + NHS_social_care_tax_nonwhite * (Ethnicity!="White") + NHS_social_care_tax_heduc * (higher_education==1) + NHS_social_care_tax_urban * (RuralUrban==1) + NHS_social_care_tax_lambda_govt_int_health * (LV_gov_health) + NHS_social_care_tax_lambda_cc_belief * (LV_CC) + NHS_social_care_tax_lambda_meat_free_diet * (LV_MFD)
  exercise_vouchers_tax_coeffs = exercise_vouchers_tax                            + exercise_vouchers_tax_over65             * (age>65) + exercise_vouchers_tax_under30             * (age<30)  + exercise_vouchers_tax_female * (gender=="Female") + exercise_vouchers_tax_nonwhite * (Ethnicity!="White") + exercise_vouchers_tax_heduc * (higher_education==1) + exercise_vouchers_tax_urban * (RuralUrban==1) + exercise_vouchers_tax_lambda_govt_int_health * (LV_gov_health) + exercise_vouchers_tax_lambda_cc_belief * (LV_CC) + exercise_vouchers_tax_lambda_meat_free_diet * (LV_MFD)
  NHS_social_care_subsidy_coeffs = NHS_social_care_subsidy                        + NHS_social_care_subsidy_over65           * (age>65) + NHS_social_care_subsidy_under30           * (age<30)  + NHS_social_care_subsidy_female * (gender=="Female") + NHS_social_care_subsidy_nonwhite * (Ethnicity!="White") + NHS_social_care_subsidy_heduc * (higher_education==1) + NHS_social_care_subsidy_urban * (RuralUrban==1) + NHS_social_care_subsidy_lambda_govt_int_health * (LV_gov_health) + NHS_social_care_subsidy_lambda_cc_belief * (LV_CC) + NHS_social_care_subsidy_lambda_meat_free_diet * (LV_MFD)
  exercise_vouchers_subsidy_coeffs = exercise_vouchers_subsidy                    + exercise_vouchers_subsidy_over65         * (age>65) + exercise_vouchers_subsidy_under30         * (age<30)  + exercise_vouchers_subsidy_female * (gender=="Female") + exercise_vouchers_subsidy_nonwhite * (Ethnicity!="White") + exercise_vouchers_subsidy_heduc * (higher_education==1) + exercise_vouchers_subsidy_urban * (RuralUrban==1) + exercise_vouchers_subsidy_lambda_govt_int_health * (LV_gov_health) + exercise_vouchers_subsidy_lambda_cc_belief * (LV_CC) + exercise_vouchers_subsidy_lambda_meat_free_diet * (LV_MFD)
  
  ## SP utility function
  income_multiplier_tax=(income_numeric/median(income_numeric))^lambda_income_tax
  income_multiplier_sub=1
  
  V = list()
  V[['tax']]      =  left_to_right_bias_dce1 * (left_hand=="tax") + tax_asc + 
    level_of_tax_dce1_coeffs * income_multiplier_tax * (-tax_numeric +  tax_unhealthy_foods_coeffs * (a1_x3==1) + tax_high_sugar_foods_coeffs * (a1_x3==2) + tax_high_salt_foods_coeffs * (a1_x3==3) + tax_sugary_drinks * (a1_x3==4) + policy_affects_general_population * (a1_x5==5) + tax_affects_high_income_coeffs * (a1_x5==1) + general_taxation * (a1_x6==3) + NHS_social_care_tax_coeffs * (a1_x6==2) + exercise_vouchers_tax_coeffs * (a1_x6==1))
  V[['subsidy']]  =  left_to_right_bias_dce1 * (left_hand=="subsidy") + subsidy_asc_coeffs + level_of_subsidy_coeffs * income_multiplier_sub * subsidy_numeric + 
    level_of_tax_dce1_coeffs * income_multiplier_tax * (subsidise_fresh_produce_coeffs * (a2_x4==1) + subsidise_high_fibre_foods * (a2_x4==2) + policy_affects_general_population * (a2_x5==5) + subsidy_affects_low_income_coeffs * (a2_x5==3) + subsidy_affects_families_children_coeffs * (a2_x5==2) + subsidy_affects_overweight_obese_coeffs * (a2_x5==4) + general_taxation * (a2_x6==3) + NHS_social_care_subsidy_coeffs * (a2_x6==2) + exercise_vouchers_subsidy_coeffs * (a2_x6==1))
  
  ### Enumerate alternatives and availability, and select choice variable.
  mnl_settings_dce1 = list(
    alternatives = c(tax=1, subsidy=2),
    avail        = list(tax=1, subsidy=1),
    rows         = DESIGN_ROW_1!=25,
    componentName= "DCE1_choice_SP",
    choiceVar    = pref1,
    V            = V
  )
  
  ### Compute probabilities using MNL model
  P[['DCE1_choice_SP']] = apollo_mnl(mnl_settings_dce1, functionality)
  
  
  ############## DCE 2
  ### build utility function parameters
  tax_meat_dairy_coeffs = tax_meat_dairy + tax_meat_dairy_over65 * (age>65) + tax_meat_dairy_under30 * (age<30)  + tax_meat_dairy_female * (gender=="Female") + tax_meat_dairy_nonwhite * (Ethnicity!="White") + tax_meat_dairy_heduc * (higher_education==1) + tax_meat_dairy_urban * (RuralUrban==1) + tax_meat_dairy_lambda_cc_belief * (LV_CC) + tax_meat_dairy_lambda_govt_int_health * (LV_gov_health) + tax_meat_dairy_lambda_meat_free_diet * (LV_MFD)
  tax_greenhouse_gases_coeffs = tax_greenhouse_gases + tax_greenhouse_gases_over65 * (age>65) + tax_greenhouse_gases_under30 * (age<30) + tax_greenhouse_gases_female * (gender=="Female") + tax_greenhouse_gases_nonwhite * (Ethnicity!="White")  + tax_greenhouse_gases_heduc * (higher_education==1) + tax_greenhouse_gases_urban * (RuralUrban==1) + tax_greenhouse_gases_lambda_cc_belief * (LV_CC) + tax_greenhouse_gases_lambda_govt_int_health * (LV_gov_health) + tax_greenhouse_gases_lambda_meat_free_diet * (LV_MFD)
  tax_plastic_production_coeffs = tax_plastic_production + tax_plastic_production_over65 * (age>65) + tax_plastic_production_under30 * (age<30) + tax_plastic_production_female * (gender=="Female") + tax_plastic_production_nonwhite * (Ethnicity!="White") + tax_plastic_production_heduc * (higher_education==1) + tax_plastic_production_urban * (RuralUrban==1) + tax_plastic_production_lambda_cc_belief * (LV_CC) + tax_plastic_production_lambda_govt_int_health * (LV_gov_health) + tax_plastic_production_lambda_meat_free_diet * (LV_MFD) 
  low_impact_foods_coeffs = low_impact_foods + low_impact_foods_over65 * (age>65) + low_impact_foods_under30 * (age<30) + low_impact_foods_female * (gender=="Female") + low_impact_foods_nonwhite * (Ethnicity!="White") + low_impact_foods_heduc * (higher_education==1) + low_impact_foods_urban * (RuralUrban==1) + low_impact_foods_lambda_cc_belief * (LV_CC) + low_impact_foods_lambda_govt_int_health * (LV_gov_health) + low_impact_foods_lambda_meat_free_diet * (LV_MFD)
  UK_environment_coeffs = UK_environment + UK_environment_over65 * (age>65) + UK_environment_under30 * (age<30)  + UK_environment_female * (gender=="Female") + UK_environment_nonwhite * (Ethnicity!="White") + UK_environment_heduc * (higher_education==1) + UK_environment_urban * (RuralUrban==1) + UK_environment_lambda_cc_belief * (LV_CC) + UK_environment_lambda_govt_int_health * (LV_gov_health) + UK_environment_lambda_meat_free_diet * (LV_MFD)
  donate_coeffs = donate + donate_over65 * (age>65) + donate_under30 * (age<30) + donate_female * (gender=="Female") + donate_nonwhite * (Ethnicity!="White") + donate_heduc * (higher_education==1) + donate_urban * (RuralUrban==1) + donate_lambda_cc_belief * (LV_CC) + donate_lambda_govt_int_health * (LV_gov_health) + donate_lambda_meat_free_diet * (LV_MFD)

  
  ## SP utility function
  income_multiplier=(income_numeric/median(income_numeric))^lambda_income

  V = list()
  V[['tax1']]  =  left_to_right_bias_dce2 +
    level_of_tax_coeffs * income_multiplier * (-tax_numeric_dce2_alt1 +  tax_water_use * (b1_x2==1) + tax_greenhouse_gases_coeffs * (b1_x2==2) + tax_plastic_production_coeffs * (b1_x2==3) + tax_meat_dairy_coeffs * (b1_x2==4) + general_taxation * (b1_x3==4) + low_impact_foods_coeffs * (b1_x3==1) + UK_environment_coeffs * (b1_x3==2) + donate_coeffs * (b1_x3==3) )
  V[['tax2']]  =  
    level_of_tax_coeffs * income_multiplier * (-tax_numeric_dce2_alt2 + tax_water_use * (b2_x2==1) + tax_greenhouse_gases_coeffs * (b2_x2==2) + tax_plastic_production_coeffs * (b2_x2==3) + tax_meat_dairy_coeffs * (b2_x2==4) + general_taxation * (b2_x3==4) + low_impact_foods_coeffs * (b2_x3==1) + UK_environment_coeffs * (b2_x3==2) + donate_coeffs * (b2_x3==3) )
  
  ### Enumerate alternatives and availability, and select choice variable.
  mnl_settings_dce2 = list(
  alternatives = c(tax1=1, tax2=2),
  avail        = list(tax1=1, tax2=1),
  rows         = DESIGN_ROW_2!=25,
  choiceVar    = prefb,
  componentName= "DCE2_choice_SP",
  V            = V
  )
  
  ### Compute probabilities using MNL model
  P[['DCE2_choice_SP']] = apollo_mnl(mnl_settings_dce2, functionality)
  
  ### Likelihood of the whole model
  P = apollo_combineModels(P, apollo_inputs, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)
  
  ### Apply weights
  P = apollo_weighting(P,apollo_inputs,functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)

}


# ################################################################# #
##### ESTIMATE THE MODEL AND GET OUTPUT                          ####
# ################################################################# #
### Estimate model
settings<-list(estimationRoutine="bgw",maxIterations=2000)
model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs,estimate_settings=settings)

s=list(printT1=TRUE)
apollo_modelOutput(model, modelOutput_settings=s)

