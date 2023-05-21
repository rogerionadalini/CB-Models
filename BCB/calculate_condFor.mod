%% -----------------------------------------------
%  Declaração de variáveis
%  -----------------------------------------------

%Internal endogenous variables
var	 dYnomin d_Ypot G d_Y_anual ynomin ;

%CPI

    % SAE and Core CPI
    var inflIPC inflIPC4 inflsae inflsae_core inflsae_core_nt inflsae_core_t inflsae_resto; 
    var inflsae_core_anual inflsaeanual;

    %Food and Energy CPI
    var infle infla;

%Labor Market
var  Ubar u U gUbar Ubar_total zu;

%Domestic Financial Variables
var i in rlp bcu5 bcu10 rn r i_desv ;

%Exchange rate variables
var rer deus drerus deusrm de drer ;

%Foreing Variables
    %Foreing Demand
    var  yx_avan yx_emer; 
    %Foreing Financial Variables
    var iUSTbill90 rUSTbill90 inflCPIUS;  
    %Foreing inflation    
    var inflIPE inflnoUS embi_ch sum_inflUS;

%ToT variables
    var tdi pcu pmoil  px px_pcu px_pnpcu  pm pm_oil pm_pnoil ;
    var dtdi dpx dpx_pcu dpx_pnpcu dpm dpm_oil dpm_pnoil;
    var tdi_fun px_fun pm_fun px_pcu_fun px_pnpcu_fun pm_oil_fun pm_pnoil_fun;
    var delta_i delta_i_ext rer_gap rer_pot drer_gap;
    
%MEPCO
     var  d_pmoil_mepco;

% Endogenous Shocks (AR (1) Shocks)

%Demand Shocks
var zynomin ;
%Cost Push Shocks
var zinfle zinfla zinflsae_core_nt zinflsae_core_t zinflsae_resto;
% Monetary Shocks
var   zi zrn zrlp ;
% UIP Shocks
var zdeus  zdeusrm ;
% Foreing Shocks
     %Foreing Demand
     var zyx_emer zyx_avan; 
     %Foreing Financial Variables
     var ziUSTbill90;
     %Foreing inflation   
     var zembi_ch zinflCPIUS  zinflnoUS; 
  
% ToT Shocks   
var  zpx_pnpcu zpm_pnoil  zpmoil;

% Anual quarterly equivalences
var GROWTH_EY0 GROWTH_EY1 GROWTH_EY2 GROWTH_EY1LR GROWTH_EY2LR GROWTH_EY3LR GROWTH_EY4LR 
    GROWTH_EY5LR GROWTH_EY6TO10LR PIE_EY0 PIE_EY1 ;

% Others
var inflsae_2 Alimentos Energia IPC;

% Shocks IID - Variables Exogenas      

 %Output Shocks
 varexo zzd_Ypot zzG zzynomin ;

 %Cost Push Shocks
 varexo zzinfla zzinfle zzsae_core_nt zzsae_core_nt_s zzsae_core_t zzsae_core_t_s zzsae_resto zzsae_resto_s ;
 
 %Approximation Shocks
 varexo zzIPCSAE zzIPCSAEcore zzinflIPC ;  

 %Labor Shocks  
varexo zzUbar zzgUbar zzu;  

 %Monetary Shocks   
varexo zzi zzrlp zzrn;

%UIP Shocks
varexo zzdeus zzdeusrm ;

%Foreing Shocks
    %Foreing Demanda
    varexo zzyx_emer zzyx_avan; 
    %Foreing Financial Variables
    varexo zziUSTbill90; 
    %Foreing inflation   
    varexo zzinflCPIUS zzinflIPE zzembi_ch;

%ToT Shocks
varexo  zzpcu zzpmoil zzpx zzpx_pcu zzpx_pnpcu zzpm zzpm_oil zzpm_pnoil;

%MEPCO Shock
varexo  zzmepco;

%% -----------------------------------------------
%  Parametrization - Names
%  -----------------------------------------------

%IS Parameters
parameters	 a7 a6 a5 a4 a3 a2 a1;

%CPI Parameters
parameters share_core_nt share_core_t share_core share_sae share_a share_e inftarget;
    
%Phillips Curve Parameters
parameters	bnt1 bnt2 bnt3 bnt4 bnt5 rho_zinflsae_core_nt; 
parameters	bt1 bt2 bt3 bt4 bt5 bt_ec rho_zinflsae_core_t ;
parameters	rho_inflsae_resto rho_zinflsae_resto ;
    
%STD Scale Parameters
parameters std_inflsae_core_nt std_inflsae_core_nt_s std_inflsae_core_t std_inflsae_core_t_s std_inflsae_resto std_inflsae_resto_s ;   
parameters std_inflsae_core std_inflsae std_inflIPC;

%CPI Parameters
parameters rho_infla ;

%Taylor Rule Parameters
parameters c3 c2 c1 ;

%Foreing Parameters
    %Foreing Demand
    parameters rho1_yx_emer rho1_yx_avan; 
    %Foreing Financial and Inflation parameters
    parameters rho1_embi_ch rho1_inflCPIUS rho1_deusrm ;

%Shocks persistance parameters
parameters rhoyresto rhoi   rhozinfla rhozinfle rhorlp  rhozyx_emer  rhozyx_avan rhodeus;
parameters rhozdeusrm rho1_iUSTbill90 rhoziUSTbill90  rhozinflCPIUS rho1_inflnoUS rhozinflnoUS rhozembi_ch;


%ToT Parameters   
parameters  ec moil rho_pcu rho1_pmoil rho2_pmoil rhozpmoil ;
parameters  rho_zpx_pnpcu rho_zpm_pnoil;
parameters std_pcu std_pmoil std_px std_px_pcu std_px_pnpcu std_zpx_pnpcu std_pm std_pm_oil std_pm_pnoil std_zpm_pnoil;

  
%Labor Parameters
parameters tau4  tau3 tau2 tau1 rhozu;
parameters std_Ubar std_gUbar std_u;
parameters landa_1 landa_2;

%Long-Run Parameters 
parameters theta_G growth_ss Uss crnpot;

%Others Parameters
parameters cc1 e1 pie1  f1 ;

%STD Scale Parameters
parameters std_infla std_infle std_yxemer std_yxavan std_iUSTbill90 std_embi std_inflCPIUS std_inflIPE;

%UIP Parameter
parameters tita;
%MEPCO Parameter
parameters  std_mepco;

%Food CPI Parameters
parameters gap_infla;
%Volatile CPI Parameters
parameters br1 br2;


%% -----------------------------------------------
%  Parametrization - values
%  -----------------------------------------------

%IS Parameters
 a7        = 0.022094; % estimado 
 a6        = 0.010961; % estimado 
 a5        = 0.053116; % estimado 
 a4        = 0.044703; % estimado 
 a3        = 0.059471; % estimado 
 a2        = 0.062599; % estimado 
 a1        = 0.153720; % estimado 
 rhoyresto = 0.422850; % estimado 

%Taylor Rule Parameters
 c3        = 0.20948;   % estimado 
 c2        = 1.6014;   % estimado 
 c1        = 0.7852;   % estimado 
 rhoi      = 0;
 
%Long-Run Parameters 
 theta_G        = 0.07;
 growth_ss      = 0.00825;
 Uss            = 0.081; 
 
%Labor Parameters
tau4   = 0.1;
tau3   = 0.1;
tau1   = 0.027;
tau2   = 0.503;
rhozu  = 0;
     
landa_1    = 1;
landa_2    = 0.8/0.3;    
std_Ubar   = 0.0011517; % estimado 
std_gUbar  = 1;
std_u      = 1;
 

%CPI Parameters
rho_infla  = 0.18818; % estimado 
rhozinfla  = 0;
pie1       = 0.5;
rhozinfle  = 0;
 
 
%CPI Parameters (com pesos de acordo com nova metodologia!)
 share_core_nt   = 0.3842; % = 0.3816159;
 share_core_t    = 0.2672; % = 0.1311754;
 share_core      = 0.6514; % = 0.5127913;
 share_sae       = 0.8240; % = 0.73158220;
 share_a         = 0.1011; % = 0.19301310000;
 share_e         = 0.0750; % = 0.07540470000;
    
%Phillips Curve Parameters
      %Non Tradable PC
            bnt1   = 0.157500;   % Foward Loking Expectation  % estimado 
            bnt2   = 0.561250;   % Back Loking Indexation     % estimado 
            bnt3   = 0.064121;   % Output Gap Pressure        % estimado 
            bnt4   = 0.3;       % Salaries Push
            bnt5   = 0;         % Error Correction
            
            rho_zinflsae_core_nt  = 0.19981; % estimado 

      %Tradable PC
            bt1   = 0.1*0;      % Foward Loking Expectation
            bt2   = 0.67899;    % Back Loking Indexation     % estimado 
            bt3   = 0.02202;    % Output Gap Pressure        % estimado 
            bt4   = 0.034391;   % Error Correction           % estimado 
            bt5   = 0.016330;   % Error Correction           % estimado 
            bt_ec = 0.03;
            
            rho_zinflsae_core_t = 0;

       %Volatile CPIC
            br1                 = 0.10397;  % estimado 
            br2                 = 0.04368;  % estimado 
            
            rho_inflsae_resto   = 0.20359;  % estimado 
            rho_zinflsae_resto  = 0.24145;  % estimado 

%STD Scale Parameters 
std_inflsae_core_nt    = 1;
std_inflsae_core_nt_s  = 1;
std_inflsae_core_t     = 1;
std_inflsae_core_t_s   = 1;
std_inflsae_resto      = 1;
std_inflsae_resto_s    = 1; 
std_inflsae_core       = 1;
std_inflsae            = 1;
std_inflIPC            = 1;

        
%Others Parameters 
 cc1  = 0.875;
 
 %UIP
 e1     = 0;
 rhorlp = 0.4125;
 
%Long-Run Parameters 
 
 crnpot     = (1/3.3)*4;
 inftarget  = 0.03;
 
 %Persistencias
 %UIP Parameter
  tita = 0.20194; % estimado 
  
 %Shocks persistance parameters 
 rhodeus       = 0.61344; % estimado 
 rho1_deusrm   = 0.3262305975522828;
 rhozdeusrm    = 0;
 rho1_yx_emer  = 0.89596; % estimado 
 rhozyx_emer   = 0;
 rho1_yx_avan  = 0.8934; % estimado 
 rhozyx_avan   = 0;
 
%Foreing Parameters
 rho1_iUSTbill90   = 0.884827689520846;% //  0.75;  %Calibracion XMAS Search
 rhoziUSTbill90    = 0;
 rho1_embi_ch      = 0.7778; % estimado 
 rho1_inflCPIUS    = 0.25451; % estimado 
 rhozinflCPIUS     = 0;
 rho1_inflnoUS     = 0.25609; % estimado 
 rhozinflnoUS      = 0;
 f1                = 0.2232;
 rhozembi_ch       = 0;
 
  %ToT Parameters   
  std_pcu       = 1;
  std_pmoil     = 1;
  std_px        = 1;
  std_px_pcu    = 1;
  std_px_pnpcu  = 1;
  std_zpx_pnpcu = 1;
  std_pm        = 1;
  std_pm_oil    = 1;
  std_pm_pnoil  = 1;
  std_zpm_pnoil = 1;

  ec              = 0.4;
  moil            = 0.1;
  rho_pcu         = 0.76539;  % estimado 
  rho2_pmoil      = 0;
  rho1_pmoil      = 0.82642; % estimado 
  rhozpmoil       = 0;
  rho_zpx_pnpcu   = 0.5685;  % estimado 
  rho_zpm_pnoil   = 0.42501;  % estimado 

     
 %STD Scale Parameters     
 std_infla      = 1;
 std_infle      = 1;
 std_yxemer     = 1;
 std_yxavan     = 1;
 std_iUSTbill90 = 1;
 std_embi       = 1;
 std_inflCPIUS  = 1;
 std_inflIPE    = 1;
 std_mepco      = 1;
 
 %Food CPI Parameters
 gap_infla      = 0.26505;  % estimado 
 

%% -----------------------------------------------
%  Model
%  -----------------------------------------------

model(linear);

// Equation #1 IS

ynomin - ynomin(-1) = -a1*(ynomin(-1) + ynomin(-2)) -a2*(ynomin(-1) -ynomin(-2)) -a3*(r + r(-1) - rn - rn(-1)) + a4*(yx_emer +yx_emer(-1)) +a5*(yx_avan + yx_avan(-1)) + a6*(rer(-1)) +a7*tdi +zynomin;

// Equation #2 
zynomin =rhoyresto*(zynomin(-1)) +zzynomin;

    
// Equation #3 - Non Tradable PC 
inflsae_core_nt = bnt1*inflsae_core_nt(+1) + bnt2*(inflsae_core_nt(-1) - std_inflsae_core_nt_s*zzsae_core_nt_s(-1)) + bnt3*ynomin +  std_inflsae_core_nt_s*zzsae_core_nt_s + zinflsae_core_nt;

// Equation #4 
zinflsae_core_nt = rho_zinflsae_core_nt*zinflsae_core_nt(-1) + std_inflsae_core_nt*zzsae_core_nt;

// Equation #5 - Tradable PC 
inflsae_core_t = bt1*inflsae_core_t(+1) + bt2*(inflsae_core_t(-1) - std_inflsae_core_t_s*zzsae_core_t_s(-1)) + 
                 bt4*((drer + inflIPC-inflIPE)+ (drer(-1) + inflIPC(-1)- inflIPE(-1))) 
                 + bt3*ynomin +bt5*rer(-1) +  zinflsae_core_t + std_inflsae_core_t_s*zzsae_core_t_s ;

// Equation #6 - Tradable PC             
zinflsae_core_t = rho_zinflsae_core_t*zinflsae_core_t(-1) + std_inflsae_core_t*zzsae_core_t ;

// Equation #7 - Volatile CPI             
 inflsae_resto = rho_inflsae_resto*(inflsae_resto(-1) - std_inflsae_resto_s*zzsae_resto_s(-1)) 
            + br1*(drer + inflIPC-inflIPE) 
            + br2*(rer(-1))         
            + zinflsae_resto + std_inflsae_resto_s*zzsae_resto_s;

// Equation #8             
zinflsae_resto = rho_zinflsae_resto*zinflsae_resto(-1)+  std_inflsae_resto*zzsae_resto;

// Equation #9 - Core CPI agregation             
inflsae_core = (share_core_nt*inflsae_core_nt + share_core_t*inflsae_core_t)/share_core + std_inflsae_core*zzIPCSAEcore ;

// Equation #10 - Non Food and Energy CPI agregation             
inflsae = (share_core*inflsae_core + (share_sae-share_core)*inflsae_resto)/share_sae + std_inflsae*zzIPCSAE ;

// Equation #11 - CPI agregation             
 inflIPC = share_sae*inflsae + share_a*infla + share_e*infle + std_inflIPC*zzinflIPC;

// Equation #12 - Annual Core CPI              
inflsae_core_anual = inflsae_core + inflsae_core(-1) + inflsae_core(-2) + inflsae_core(-3);

// Equation #13 - Annual Non Food and Energy CPI              
inflsaeanual =inflsae +inflsae(-1) +inflsae(-2) +inflsae(-3);

// Equation #14 Taylor Rule
 i - in = c1*(i(-1) - in(-1)) +(1-c1)*(c2*(inflsaeanual(+1)) +c3*ynomin) +zi;
                 
// Equation #15 
zi =rhoi*(zi(-1)) + zzi;

// Equation #16 - Growth decomposition
ynomin -ynomin(-1) = dYnomin - d_Ypot;

// Equation #17 - Potencial Growth 
d_Ypot =G +zzd_Ypot;

// Equation #17  - Potencial Growth
G =theta_G*growth_ss+(1-theta_G)*(G(-1)) +zzG;

// Equation #18 - Annual Growth
d_Y_anual =dYnomin +dYnomin(-1) +dYnomin(-2) +dYnomin(-3);

// Equation #19 - Unemployment
U = u +Ubar;

// Equation #20 - NAIRU 
Ubar =tau4*Uss+(1-tau4)*(Ubar(-1)) +gUbar +std_Ubar*landa_1*zzUbar;

// Equation #21 - NAIRU growth
gUbar =(1-tau3)*(gUbar(-1)) + std_Ubar*landa_1*zzgUbar;

// Equation #22 - Okun Law
u = tau2*u(-1)-tau1*ynomin + zu;

// Equation #23
zu = rhozu*zu(-1) + std_Ubar*landa_2*zzu;

// Equation #30 - Food Inflation
infla =rho_infla*(infla(-1)) + gap_infla*ynomin +zinfla;

// Equation #31 
zinfla =rhozinfla*(zinfla(-1)) + std_infla*zzinfla;

// Equation #32 - MEPCO: "The MEPCO is a device designed to smooth oil price variations and implemented by the National Petroleum Enterprise"
d_pmoil_mepco = 0.7*0.43*d_pmoil_mepco(-1) + (1-0.7)*( (drer +inflIPC -inflIPE) + inflIPE + pmoil -pmoil(-1))+std_mepco*zzmepco;

// Equation #33 - Energy Inflation
infle =(5.268090*d_pmoil_mepco + 2.272380*zinfle)/7.540470;

// Equation #34 
zinfle =rhozinfle*(zinfle(-1)) +std_infle*zzinfle;

// Equation #35 - Annual CPI 
inflIPC4 =inflIPC +inflIPC(-1) +inflIPC(-2) +inflIPC(-3);

// Equation #36 - Real rate definition
r - rn = i - in -4*(inflIPC(1));

// Equation #37 - Foward  Equation
rlp =cc1*(rlp(1)) +(1-cc1)*(r -rn) +zrlp;

// Equation #39  - bcu5 Equation
bcu5 =cc1*(bcu5(1)) +(1-cc1)*(r -rn);

// Equation #40 - bcu10 Equation
bcu10 =cc1*(bcu10(1)) +(1-cc1)*(r -rn);

// Equation #41 
    delta_i = ((i-in) - (i(-1)-in(-1)))/4;

// Equation #42 
 delta_i_ext = iUSTbill90/4 + embi_ch/4 - (iUSTbill90(-1)/4 + embi_ch(-1)/4);
    
// Equation #43 - UIP 

(i -in)/4 -iUSTbill90/4 -embi_ch/4 =(1-e1)*deus(+1) -e1*deus + zdeus;

// Equation #44 - Real Exchange rate 
rer = rer_gap + rer_pot;

// Equation #45 - Equilibrium Real Exchange rate 
rer_pot = -(i-in)/4 + iUSTbill90/4 + embi_ch/4  - tita*tdi_fun;

// Equation #46 - Change in cycle Real exchange rate 
drer_gap =deus +inflIPE -inflIPC;

// Equation #47 - Cycle Real exchange rate
rer_gap =rer_gap(-1) +drer_gap;

// Equation #48 - Real Exchange rate
rer =rer(-1) +drer;    

// Equation #49 
zrlp =rhorlp*(zrlp(-1)) +zzrlp;

// Equation #50 - Real Neutral Interest Rate 
rn =crnpot*(d_Ypot(1)) +zrn;

// Equation #51 
zrn =0*(zrn(-1)) + zzrn;

// Equation #52 - Nominal Neutral Interest Rate
in =rn +inftarget;

// Equation #53 
i_desv =i -in;

// Equation #54 - Dollar/Clp Depreciation 
drerus =deus +inflCPIUS -inflIPC;

// Equation #55 
de =deus +deusrm;

// Equation #56 - Real Foreing Interet Rate
rUSTbill90 =iUSTbill90 -4*(inflCPIUS(1));

// Equation #57- UIP Shock 
zdeus =rhodeus*(zdeus(-1)) +zzdeus;

// Equation #58 
deusrm =rho1_deusrm*(deusrm(-1)) +zdeusrm;

// Equation #59 
zdeusrm =rhozdeusrm*(zdeusrm(-1)) +zzdeusrm;

// Equation #60 - Foreing Demand: E 
yx_emer =rho1_yx_emer*(yx_emer(-1)) +zyx_emer;

// Equation #61 
zyx_emer =rhozyx_emer*(zyx_emer(-1)) +std_yxemer*zzyx_emer;

// Equation #62 - Foreing Demand: A
yx_avan =rho1_yx_avan*(yx_avan(-1)) +zyx_avan;

// Equation #63 
zyx_avan =rhozyx_avan*(zyx_avan(-1)) +std_yxavan*zzyx_avan;

// Equation #64 - Foreing Interest Rate
iUSTbill90 =rho1_iUSTbill90*(iUSTbill90(-1)) +ziUSTbill90;

// Equation #65 
ziUSTbill90 =rhoziUSTbill90*(ziUSTbill90(-1)) +std_iUSTbill90*zziUSTbill90;

// Equation #66 - Risk Premium
embi_ch =rho1_embi_ch*(embi_ch(-1)) +zembi_ch;

// Equation #67 - US CPI
inflCPIUS =rho1_inflCPIUS*(inflCPIUS(-1)) +zinflCPIUS;

// Equation #68 
zinflCPIUS =rhozinflCPIUS*(zinflCPIUS(-1)) +std_inflCPIUS*zzinflCPIUS;

// Equation #69 - Non US CPI
inflnoUS =rho1_inflnoUS*(inflnoUS(-1)) +zinflnoUS;

// Equation #70 
zinflnoUS =rhozinflnoUS*(zinflnoUS(-1)) +std_inflIPE*zzinflIPE;

// Equation #71 
zembi_ch =rhozembi_ch*zembi_ch +std_embi*zzembi_ch;

// Equation #72 - ToT definition 
tdi = px - pm;  

// Equation #73 - Fundamental ToT definition    
tdi_fun = px_fun - pm_fun;

// Equation #74 - Fundamental Export Prices    
px_fun = ec*px_pcu_fun + (1-ec)*px_pnpcu_fun;

// Equation #75 - Fundamental Import Prices    
pm_fun = moil*pm_oil_fun + (1-moil)*pm_pnoil_fun;

// Equation #76 - Fundamental Copper Export prices    
px_pcu_fun=pcu;

// Equation #77 - Fundamental Non-Copper Export prices    
px_pnpcu_fun=zpx_pnpcu;

// Equation #78 - Fundamental Oil Import prices    
pm_oil_fun=pmoil;

// Equation #79 - Fundamental Non-Oil Import prices    
pm_pnoil_fun=zpm_pnoil;
    
// Equation #80 - Export Prices    
px = ec*px_pcu + (1-ec)*px_pnpcu + std_px*zzpx;

// Equation #81 - Copper Export Prices    
px_pcu = pcu + std_px_pcu*zzpx_pcu;

// Equation #82 - Non-Copper Export Prices    
px_pnpcu = zpx_pnpcu;

// Equation #83    
zpx_pnpcu=rho_zpx_pnpcu*zpx_pnpcu(-1) + std_zpx_pnpcu*zzpx_pnpcu;

// Equation #84 - Import Prices        
pm = moil*pm_oil + (1-moil)*pm_pnoil + std_pm*zzpm;

// Equation #85 - Oil Import Prices        
pm_oil = pmoil + std_pm_oil*zzpm_oil;

// Equation #86 - Non-Oil Import Prices        
pm_pnoil = zpm_pnoil;

// Equation #87        
zpm_pnoil=rho_zpm_pnoil*zpm_pnoil(-1) + std_zpm_pnoil*zzpm_pnoil;

// Equation #88 - Copper Price        
pcu =rho_pcu*pcu(-1) +std_pcu*zzpcu;

// Equation #89 - Oil Price        
pmoil =rho1_pmoil*(pmoil(-1)) +rho2_pmoil*(pmoil(-2)) +zpmoil;

// Equation #90         
zpmoil =rhozpmoil*(zpmoil(-1)) +std_pmoil*zzpmoil;
    
// Equation #91       
dtdi = tdi - tdi(-1);   

// Equation #92       
dpx = px - px(-1);

// Equation #93       
dpx_pcu = px_pcu - px_pcu(-1) ;

// Equation #94       
dpx_pnpcu=px_pnpcu - px_pnpcu(-1);

// Equation #95       
dpm=pm - pm(-1) ;

// Equation #95       
dpm_oil =pm_oil - pm_oil(-1)  ;

// Equation #96       
dpm_pnoil= pm_pnoil - pm_pnoil(1) ;

// Equation #97 - Foreing Inflation       
inflIPE =f1*inflCPIUS +(1-f1)*inflnoUS;
    
// Equation #98 - Annual Growth       
GROWTH_EY0 =(dYnomin(3) +2*(dYnomin(2)) +3*(dYnomin(1)) +4*(dYnomin(0)) +3*(dYnomin(-1)) +2*(dYnomin(-2)) +dYnomin(-6))/4;

// Equation #99 - Annual Growth (+1)
GROWTH_EY1 =(dYnomin(7) +2*(dYnomin(6)) +3*(dYnomin(5)) +4*(dYnomin(4)) +3*(dYnomin(3)) +2*(dYnomin(2)) +dYnomin(-2))/4;

// Equation #100 - Annual Growth (+2)
GROWTH_EY2 =(dYnomin(11) +2*(dYnomin(10)) +3*(dYnomin(9)) +4*(dYnomin(8)) +3*(dYnomin(7)) +2*(dYnomin(6)) +dYnomin(2))/4;

// Equation #101 - Annual Growth (+1)
GROWTH_EY1LR =(dYnomin(5) +2*(dYnomin(4)) +3*(dYnomin(3)) +4*(dYnomin(2)) +3*(dYnomin(1)) +2*(dYnomin(0)) +dYnomin(-1))/4;

// Equation #102 - Annual Growth (+2)
GROWTH_EY2LR =(dYnomin(9) +2*(dYnomin(8)) +3*(dYnomin(7)) +4*(dYnomin(6)) +3*(dYnomin(5)) +2*(dYnomin(4)) +dYnomin(3))/4;

// Equation #103 - Annual Growth (+3)
GROWTH_EY3LR =(dYnomin(13) +2*(dYnomin(12)) +3*(dYnomin(11)) +4*(dYnomin(10)) +3*(dYnomin(9)) +2*(dYnomin(8)) +dYnomin(7))/4;

// Equation #104 - Annual Growth (+4)
GROWTH_EY4LR =(dYnomin(17) +2*(dYnomin(16)) +3*(dYnomin(15)) +4*(dYnomin(14)) +3*(dYnomin(13)) +2*(dYnomin(12)) +dYnomin(11))/4;

// Equation #105 - Annual Growth (+5)
GROWTH_EY5LR =(dYnomin(21) +2*(dYnomin(20)) +3*(dYnomin(19)) +4*(dYnomin(18)) +3*(dYnomin(17)) +2*(dYnomin(16)) +dYnomin(15))/4;

// Equation #106 - Annual Growth (+6)
GROWTH_EY6TO10LR =((dYnomin(25) +2*(dYnomin(24)) +3*(dYnomin(23)) +4*(dYnomin(22)) +3*(dYnomin(21)) +2*(dYnomin(20)) +dYnomin(19))/4 +(dYnomin(29) +2*(dYnomin(28)) +3*(dYnomin(27)) +4*(dYnomin(26)) +3*(dYnomin(25)) +2*(dYnomin(24)) +dYnomin(23))/4 +(dYnomin(33) +2*(dYnomin(32)) +3*(dYnomin(31)) +4*(dYnomin(30)) +3*(dYnomin(29)) +2*(dYnomin(28)) +dYnomin(27))/4 +(dYnomin(37) +2*(dYnomin(36)) +3*(dYnomin(35)) +4*(dYnomin(34)) +3*(dYnomin(33)) +2*(dYnomin(32)) +dYnomin(31))/4 +(dYnomin(41) +2*(dYnomin(40)) +3*(dYnomin(39)) +4*(dYnomin(38)) +3*(dYnomin(37)) +2*(dYnomin(36)) +dYnomin(35))/4)/5;

// Equation #107 - Annual CPI
PIE_EY0 =inflIPC +inflIPC(1) +inflIPC(2) +inflIPC(3);

// Equation #108 - Annual CPI
PIE_EY1 =inflIPC(4) +inflIPC(5) +inflIPC(6) +inflIPC(7);

// Equation #109 - 
Ubar = Ubar_total;

// Equation #110 -
inflsae_2=inflsae;

// Equation #111 - 
Alimentos = infla;

// Equation #112 -
Energia =infle;

// Equation #113 -
IPC=inflIPC;

// Equation #114 
sum_inflUS=inflCPIUS + inflCPIUS(-1) +inflCPIUS(-2) +inflCPIUS(-4);

end;

%% -----------------------------------------------
%  Shocks variance
%  -----------------------------------------------

shocks;

var	zzynomin	;	stderr 0.0081548; % estimado       

%Cost Push Shocks    
 var	zzsae_core_nt	;	stderr 0.0019831; % estimado 
 var	zzsae_core_nt_s	;	stderr 0.0019076; % estimado 
 var	zzsae_core_t	;	stderr 0.0043712; % estimado 
 var	zzsae_core_t_s	;	stderr 0.0022873; % estimado 
 var	zzsae_resto     ;	stderr 0.0088195; % estimado 
 var	zzsae_resto_s	;	stderr 0.0058397; % estimado 
 
%Approximation Shocks
 var	zzIPCSAE	;	stderr 0.00087285; % estimado 
 var	zzIPCSAEcore;	stderr 0.00088138; % estimado 
 var	zzinflIPC	;	stderr 0.00210330; % estimado 
    
%Monetary Shocks
var	zzi	;	stderr 0.0067988; % estimado 

%Output Shocks
var	zzd_Ypot	;	stderr 0.007*(13/82.5); % FMV-X Ratio Calibration
var	zzG         ;	stderr 0.007*(13/82.5); % FMV-X Ratio Calibration

%Labor Shocks  
var	zzUbar	;	stderr 0.006419; % FMV-X Ratio Calibration  % estimado 
var	zzgUbar	;	stderr 1;         % FMV-X Ratio Calibration
var	zzu     ;	stderr 1;         % FMV-X Ratio Calibration

var	zzinfla	;	stderr 0.02175; % estimado 
var	zzinfle	;	stderr 0.10443; % estimado 

 %UIP Shocks 
var	zzrlp	;	stderr 0.00369064299293808;
var	zzrn	;	stderr 0.1/100;
var	zzdeus	;	stderr 0.013273; % estimado 
var	zzdeusrm;	stderr 0.0160444294872015;

%Foreing Shocks
var	zzyx_emer	;	stderr 0.0115960; % estimado 
var	zzyx_avan	;	stderr 0.0084808; % estimado 
var	zziUSTbill90;	stderr 0.0043972; % estimado 
var	zzinflCPIUS	;	stderr 0.0052340; % estimado 
var	zzinflIPE	;	stderr 0.0317290; % estimado 
var	zzembi_ch	;	stderr 0.0030867; % estimado 

%ToT Shocks
var	zzpcu       ;	stderr 0.148670; % estimado 
var	zzpmoil     ;	stderr 0.137540; % estimado 
var	zzpx        ;	stderr 0.045514; % estimado 
var	zzpx_pcu	;	stderr 0.112490; % estimado 
var	zzpx_pnpcu	;	stderr 0.030699; % estimado 
var	zzpm        ;	stderr 0.024072; % estimado 
var	zzpm_oil	;	stderr 0.072778; % estimado 
var	zzpm_pnoil	;	stderr 0.034328; % estimado 

%MEPCO Shock 
var	zzmepco     ;	stderr 0.0052218; % estimado 

end;

%% -----------------------------------------------
%  Steady State
%  -----------------------------------------------

initval;
                      
tdi=0;
pcu=0;
pmoil=0;
zpmoil=0;
px=0;
px_pcu=0;
px_pnpcu=0;
zpx_pnpcu=0;
pm=0;
pm_oil=0;
pm_pnoil=0;
zpm_pnoil=0;
dtdi=0;
dpx=0;
dpx_pcu=0;
dpx_pnpcu=0;
dpm=0;
dpm_oil=0;
dpm_pnoil=0;

rer=0;
yx_avan=0;
yx_emer=0;
rn=crnpot*growth_ss;
r =rn;
ynomin=0;
zynomin=0;

inflIPC=0;
infla=0; 
infle=0;
inflIPC4=0;
zinfla=0;
zinfle=0;
inflsae=0;
inflsae_core=0;
inflsae_core_nt=0;
inflsae_core_t=0;
inflsae_resto=0;
inflsae_core_anual=0;
inflsaeanual=0;

deus=0;
in=rn+0.03;
i=in; 
zi=0; 
d_Ypot=growth_ss;
dYnomin =growth_ss;
G=growth_ss;
d_Y_anual=4*growth_ss;

Ubar=Uss;
u=0;
U=Uss;
gUbar=0;
Ubar_total =Ubar;
zu=0;

rlp=0;
zrlp=0;
bcu5=0;
bcu10=0;
zdeus=0;
embi_ch=0;
iUSTbill90=0;
zrn=0;
i_desv=0;
inflCPIUS=0;
drerus =0;
deusrm=0;
de=0;
inflIPE=0;
drer=0;
rUSTbill90=0;
zdeusrm =0;
zyx_emer=0;
zyx_avan=0;
ziUSTbill90=0;
zembi_ch=0;
zinflCPIUS=0;
inflnoUS=0;
zinflnoUS=0;

GROWTH_EY0 = 4*growth_ss      ;
GROWTH_EY1 = 4*growth_ss      ;
GROWTH_EY2 = 4*growth_ss      ;
GROWTH_EY1LR = 4*growth_ss    ;
GROWTH_EY2LR = 4*growth_ss    ;
GROWTH_EY3LR = 4*growth_ss    ;
GROWTH_EY4LR = 4*growth_ss    ;
GROWTH_EY5LR = 4*growth_ss    ;

GROWTH_EY6TO10LR = 4*growth_ss;

PIE_EY0=0;
PIE_EY1=0;

end;


%% -----------------------------------------------
%  Declare observed variables
%  -----------------------------------------------

varobs          dYnomin inflsae i rer yx_emer
                yx_avan embi_ch iUSTbill90  
                U infla infle inflIPC inflIPE
                
                inflsae_core inflsae_core_nt inflsae_core_t inflsae_resto 
                
                pmoil pcu dpx dpx_pcu dpx_pnpcu dpm dpm_oil pm_pnoil 
                 
                ynomin  tdi; 


%% -----------------------------------------------
%  Check equation residuals and steady state
%  -----------------------------------------------

% resid;
% steady;

           
%% -----------------------------------------------
%  Solution and IRFs
%  -----------------------------------------------

% remove option 'noprint' to get POLICY AND TRANSITION FUNCTIONS
stoch_simul(periods = 0, irf=60, noprint, nodecomposition, nocorr, nomoments, relative_irf, nograph, irf_plot_threshold = 1e-5);


%% -----------------------------------------------
%  Smoother and set initial state for forecasting
%  -----------------------------------------------

calib_smoother(datafile = data_nueva_MEP_reducida, parameter_set = calibration);
    
smoother2histval;

%% -----------------------------------------------
%  Scenarios
%  -----------------------------------------------

% Scenario 1 - Trajetória de Câmbio Nominal

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus), conf_sig = 0.90);

scen(1).smoothed = oo_.SmoothedVariables
scen(1).forecasts = oo_.conditional_forecast.cond

% Scenario 2 - Trajetória de Câmbio Nominal + Hiato do Produto

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

var ynomin;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.02,0.009,0.003,-0.003,-0.01,-0.011,-0.007,0.003,0.005,0.005,0.002,0.001;

end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus, zzynomin), conf_sig = 0.90);

scen(2).smoothed = oo_.SmoothedVariables
scen(2).forecasts = oo_.conditional_forecast.cond

% Scenario 3 - Trajetória de Câmbio Nominal + Hiato do Produto + Termos de Troca

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

var ynomin;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.02,0.009,0.003,-0.003,-0.01,-0.011,-0.007,0.003,0.005,0.005,0.002,0.001;

var tdi;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.35,0.315,0.2835,0.25515,0.229635,0.2066715,0.18600435,0.167403915,0.1506635235,0.13559717115,0.122037454035,0.1098337086315;


end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus, zzynomin, zzpm), conf_sig = 0.90);

scen(3).smoothed = oo_.SmoothedVariables
scen(3).forecasts = oo_.conditional_forecast.cond

% Scenario 4 - Cenário Otimista

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

var ynomin;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.01,-0.000999999999999999,-0.007,-0.013,-0.02,-0.021,-0.017,-0.007,-0.005,-0.005,-0.008,-0.009;

var tdi;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.35,0.315,0.2835,0.25515,0.229635,0.2066715,0.18600435,0.167403915,0.1506635235,0.13559717115,0.122037454035,0.1098337086315;

end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus, zzynomin, zzpm), conf_sig = 0.90);

scen(4).smoothed = oo_.SmoothedVariables
scen(4).forecasts = oo_.conditional_forecast.cond

% Scenario 5 - Cenário Pessimista

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

var ynomin;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.03,0.019,0.013,0.007,0,-0.001,0.003,0.013,0.015,0.015,0.012,0.011;

var tdi;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.35,0.315,0.2835,0.25515,0.229635,0.2066715,0.18600435,0.167403915,0.1506635235,0.13559717115,0.122037454035,0.1098337086315;

end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus, zzynomin, zzpm), conf_sig = 0.90);

scen(5).smoothed = oo_.SmoothedVariables
scen(5).forecasts = oo_.conditional_forecast.cond

% Scenario 6 - Cenário Condicionando o Juros (produto solto)

set_dynare_seed('default')

conditional_forecast_paths;

var deus;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025,0.0025;

var tdi;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.35,0.315,0.2835,0.25515,0.229635,0.2066715,0.18600435,0.167403915,0.1506635235,0.13559717115,0.122037454035,0.1098337086315;

var i;
periods 1,2,3,4,5,6,7,8,9,10,11,12;
values 0.07,0.0725,0.07,0.0675,0.0608333333333333,0.055,0.05,0.0475,0.0425,0.04,0.04,0.04;

end;

conditional_forecast(parameter_set = calibration, periods = 12, replic = 15000, controlled_varexo = (zzdeus, zzpm, zzi), conf_sig = 0.90);

scen(6).smoothed = oo_.SmoothedVariables
scen(6).forecasts = oo_.conditional_forecast.cond
