
% Bayesian estimation of Holston-Laubach-Williams
% Dynare version: 5.1
% 2Q 2022

% Paralelizacao
% - Comando geral de teste: dynare file.mod parallel_test
% - Comando geral: dynare file.mod parallel

% -----------------------------------------------------
% Variable Declaration
% -----------------------------------------------------

var dy dy_pot dy_gap y_gap g_pot r_nat z pi pi_exp i r r_gap;

    % Definitions:

    %'100*dlog(real GDP)'               dy
    %'100*dlog(potential GDP)'          dy_pot
    %'Output gap'                       y_gap
    %'1st dif output gap'               dy_gap
    %'Potential GDP Growth'             g_pot

    %'Real interest rate'               r
    %'Nominal interest rate'            i (trimestral)
    %'Natural interest rate'            r_nat
    %'Other determinansts of r_nat'     z

    %'Inflation (PCE Core)'             pi (trimestral)
    %'Expeted Inflation'                pi_exp (trimestral)

% -----------------------------------------------------
% Shocks
% -----------------------------------------------------

varexo eps_y_gap eps_pi eps_z eps_y_pot eps_g_pot eps_i eps_pi_exp; 

    % Definitions:

    %'Demand shock'               eps_y_gap
    %'Cost-push shock'            eps_pi
    %'Preferences/Productivity'   eps_z
    %'Shock to potential GDP'     eps_y_pot
    %'Shock to protential growth' eps_g_pot

% -----------------------------------------------------
% Observables
% -----------------------------------------------------

varobs dy pi i pi_exp;

% -----------------------------------------------------
% Parameters
% -----------------------------------------------------

parameters a1, a2, a3, b1, b2, c1, d1, d2, pi_target, lambda_g, lambda_z, sigma_eps_y_pot, sigma_eps_y_gap;

a1 = 1.44;  %1.55;
a2 = -0.54; %-0.61;
a3 = -0.06757;

b1 = 0.69; %0.67; 
b2 = 0.07836;

c1 = 1;

d1 = 1;
d2 = 0;

pi_target = 2/4;

% Mimic LW solution to pile-up problem
lambda_g = 0.053; %  = (sigma eps_g_pot)/(sigma eps_y_pot)
lambda_z = 0.030; %  = (sigma eps_z)/[(sigma eps_y_gap)*a3]

sigma_eps_y_pot = 0.1437;
sigma_eps_y_gap = 0.3540;

% -----------------------------------------------------
% Model
% -----------------------------------------------------

model(linear);

%1. Output gap definition    
dy = dy_pot + dy_gap;

%2. Output gap LoM
y_gap = a1*y_gap(-1) + a2*y_gap(-2) + a3*(r_gap(-1) + r_gap(-2))/2 + sigma_eps_y_gap*eps_y_gap;

%3. dy_gap definition
dy_gap = y_gap - y_gap(-1);

%4. Inflation
pi - pi_target = b1*(pi(-1) - pi_target) + (1 - b1)*(pi(-2) + pi(-3) + pi(-4) - 3*pi_target)/3 + b2*(y_gap(-1)) + eps_pi; 

%5. r_nat LoM
r_nat = c1*g_pot + z;

%6. Other factor growth (AR or RW)
z = d1*z(-1) + d2*z(-2) + ((sigma_eps_y_gap*lambda_z)/(a3))*eps_z;

%7. y_pot LoM
dy_pot = g_pot + sigma_eps_y_pot*eps_y_pot;

%8. g_pot LoM
g_pot = g_pot(-1) + sigma_eps_y_pot*lambda_g*eps_g_pot;

%9. Real interest rate definition
r = i - pi_exp;

%10. Inflation expectations LoM
%pi_exp - pi_target = (pi(-1) + pi(-2) + pi(-3) + pi(-4) - pi_target*4)/4 + eps_pi_exp;
pi_exp - pi_target = eps_pi_exp;

%11. Nominal interest rate LoM
i = i(-1) + eps_i;

%12. Interest gap definition
r_gap = r - r_nat;

end;

% -----------------------------------------------------
% Estado estacionario (OK)
% -----------------------------------------------------

steady_state_model;

g_pot = 2;
dy_pot = g_pot;
dy = dy_pot;
dy_gap = 0;
y_gap = 0;
z = 0;

pi_exp = pi_target;
pi = pi_exp;

r_nat = c1*g_pot + z;
i = pi_target + r_nat;
r = i - pi_exp;
r_gap = 0;

end;

steady;
check;

% -----------------------------------------------------
% Shock variances
% -----------------------------------------------------

shocks;

var eps_pi ;    stderr 0.2103; 
var eps_i;      stderr 0.2205; 
var eps_pi_exp; stderr 0.10; % alterar quando for observavel

var eps_y_gap ; stderr 1; 
var eps_z ;     stderr 1; 
var eps_y_pot ; stderr 1; 
var eps_g_pot ; stderr 1; 

end;

% -----------------------------------------------------
% Priors
% [param], [init_val], [prior_dist], [prior_mean], [prior_stderr];
% stderr [shock], [prior_dist], [prior_stderr], [shrinkage];
% -----------------------------------------------------

estimated_params;

a1, uniform_pdf,,,  0.0,   2;    
a2, uniform_pdf,,, -1.0,   0;    
a3, uniform_pdf,,, -0.3,   0;    

b1, uniform_pdf,,,  0.0,   1; 
b2, uniform_pdf,,,  0.0,   0.3; 

%d1, uniform_pdf,,,  0.0,   2;    
%d2, uniform_pdf,,, -1.0,   0;    

sigma_eps_y_pot,   inv_gamma_pdf,  0.1, inf;
sigma_eps_y_gap,   inv_gamma_pdf,  0.1, inf;

%stderr eps_pi,     inv_gamma_pdf,  1.0, inf;
%stderr eps_i,      inv_gamma_pdf,  0.1, inf;

end;


% -----------------------------------------------------
% Estimation
% -----------------------------------------------------

%identification(diffuse_filter);

%estimated_params_init(use_calibration);

%end;

%estimation(datafile = 'C:\Users\...\BD.xlsx',
%           xls_sheet = Data,
%           xls_range = B2:I242, % 4Q/22 = 254; 4Q/19 = 242 
%           mode_compute = 6,
%           mode_check,     
%           diffuse_filter,
%           mh_replic = 200000, 
%           mh_nblocks = 1,
%           filtered_vars,
%           smoother
%           ); 

% restore previous estimation
estimation(datafile = 'C:\Users\...\BD2.xlsx',
           mode_file = 'LW_estim\Output\LW_estim_mode',
           mh_jscale = 0.6673,
           xls_sheet = Data,
           xls_range = B2:I255, % 4Q/22 = 254; 4Q/19 = 242 
           load_mh_file,
           load_results_after_load_mh,
           mode_compute = 0,
           diffuse_filter,
           mh_replic = 0, 
           mh_nblocks = 1,
           filtered_vars,
           smoother
           ); 

%mh_autocorrelation_function(options_, M_, estim_params_, 'StructuralShocks', 2,oo_.SmoothedShocks.eps_y_gap,oo_.SmoothedShocks.eps_pi);

save("oo_","oo_")


%calib_smoother(datafile = 'C:\Users\Otavio Teixeira\OneDrive\Documents\Economia\Paper Replications\Laubach Williams Dynare\BD.xlsx',
%               xls_sheet = Data,
%               first_obs = 5,
%               filtered_vars,
%               xls_range = B2:I252, % 4Q/22 = 254; 4Q/19 = 242
%               diffuse_filter);

