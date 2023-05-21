%----------------------------------------------------------------
% - Sugestão: adicionar ss p/ CDS diferente de zero, e possivelmente adicionar lei de movimento AR(1). Fazer isso para outras variáveis.
%----------------------------------------------------------------

%----------------------------------------------------------------
% Set tasks (by default I turn off all tasks, i.e., = 0)
%----------------------------------------------------------------


@#ifndef projecoes_incondicionais
    @#define projecoes_incondicionais            = 0
@#endif   

@#ifndef smoothed_variables
    @#define smoothed_variables                  = 0
@#endif   

@#ifndef projecoes_condicionais
    @#define projecoes_condicionais              = 0
@#endif   

@#ifndef projecoes_condicionais_delta_COPOM
    @#define projecoes_condicionais_delta_COPOM  = 0
@#endif   

@#ifndef shock_decomposition
    @#define shock_decomposition                 = 0
@#endif   

%----------------------------------------------------------------
% Endogenous
%----------------------------------------------------------------

var h                   ${H}$               	(long_name='Hiato do produto')              % 1. latente
    r_eq                ${r^{eq}}$              (long_name='Juros de equilíbrio')           % 2. latente
    pi_exp              ${\pi^{exp}$            (long_name='Focus expectativa IPCA 12m')    % 3. observavel e endógena.
    
    r_hat               ${\hat{r}}$             (long_name='Hiato da taxa de juros real')   % 4. latente: ex-ante - juro de equilíbrio
    pi_exp_hat          ${\hat{\pi}^{e}}$       (long_name='Desvio focus-meta')             % 5. auxiliar
    pi_desv             ${\hat{\pi}}$           (long_name='Desvio inflação-meta')          % 6. auxiliar

    i                   ${i}$                   (long_name='Selic Anualizada')                          % 7. observável e endógeno
    pi_lsa              ${\pi^{lsa}$            (long_name='Inflação Livres Dessaz Anualizada')         % 8. observavel. Livres e dessaz. Está anualizada. Observável e endógena.
    pi_adm              ${\pi^{adm}$            (long_name='Inflação Administrados Dessaz Anualizada')  % 8. observavel. Livres e dessaz. Está anualizada. Observável e endógena.
    IPCA                ${IPCA}$                (long_name='Inflação Cheia Anualizada')                 % 9. observavel. Tratada como exógena, trimestral anualizada dessaz? Quebrar em adm e livres
    pi_meta             ${META}$                (long_name='Meta de Inflação 12m')                      % 10. observavel 
    pi_meta_anunc       ${META}^{anunc}$        (long_name='Anuncio Meta de Inflação 12m')              % 10. observavel 

    f_nuci              ${f_{nuci}}$            (long_name='NUCI - Ciclo')                  % 11. observável e endógeno
    f_pib               ${f_{pib}}$             (long_name='PIB - Ciclo')                   % 12. observável e endógeno
    f_unemp             ${f_{unemp}}$           (long_name='Unemp - Ciclo')                 % 13. observável e endógeno
    f_caged             ${f_{caged}}$           (long_name='CAGED - Ciclo')                 % 14. observável e endógeno
    
    dy_obs              ${dy^{obs}}$            (long_name='DLog do PIB')                      % 21. observável
    dy_pot              ${dy^{pot}}$            (long_name='DLog do PIB Pot. (RW + g)')        % 22. latente, estimado
    gy_pot              ${g_y^{pot}}$           (long_name='Crescimento Potencial PIB')        % 22. latente, estimado, ar(1)
    dcaged_obs          ${dCAGED^{obs}}$        (long_name='DLog do CAGED')                    % 21. observável
    dcaged_pot          ${dCAGED^{pot}}$        (long_name='DLog do CAGED Pot. (RW + g)')      % 22. latente, estimado
    gcaged_pot          ${g_CAGED^{pot}}$       (long_name='Crescimento Potencial CAGED')      % 22. latente, estimado, ar(1)
    
    r_exante            ${r^{ex-ante}}$         (long_name='Selic Esperada 12m')            % 15. observavel
    pi_lsa_acc4q        ${\pi_{acc4q}$          (long_name='Inflação Livres Acumulada 4Q')  % 16. auxiliar
    IPCA_acc4q          ${IPCA_{acc4q}$         (long_name='Inflação Acumulada 4Q')         % 16. auxiliar
    
    i_dif               ${i^{diff}$             (long_name='Diferencial de Juros')                              % 17. auxiliar
    delta_e_hat         ${\hat{\Delta e}$       (long_name='Desvio da variação do câmbio prevista pela PPC')    % 19. auxiliar
    delta_e             ${\Delta e}$            (long_name='Variação Observada no câmbio')                      % 20. observavel

    rp_hat_obs          ${\hat{rp}^{obs}$             (long_name='Resultado Primário Estrutural')      % 21. observavel/aux p/ choque
    ie_hat_obs          ${\hat{ie}^{obs}}$            (long_name='Incerteza Economica')                % 22. observavel/aux p/ choque
    pi_star_obs         ${\pi}^*}^{obs}}$             (long_name='Inflação Externa')                   % 23. observavel/aux p/ choque, variacao % em dolar
    pi_agro_obs         ${\pi}_{agro}}^{obs}}$        (long_name='IC-Br Agro')                         % 23. observavel/aux p/ choque, variacao % em dolar
    pi_energia_obs      ${\pi}_{energ}}^{obs}}$       (long_name='IC-Br Energia')                      % 23. observavel/aux p/ choque, variacao % em dolar
    pi_metal_obs        ${\pi}_{metal}}^{obs}}$       (long_name='IC-Br Mwtal')                        % 23. observavel/aux p/ choque, variacao % em dolar
    pi_petro_obs        ${\pi}_{brent}}^{obs}}$       (long_name='Brent')                              % 23. observavel/aux p/ choque, variacao % em dolar
    clima_el_obs        ${clima^{obs, el}}$           (long_name='Clima - El Nino')                    % 24. observavel/aux p/ choque
    clima_la_obs        ${clima^{obs, la}}$           (long_name='Clima - La Nina')                    % 24. observavel/aux p/ choque
    i_star_obs          ${Fed Funds^{obs}}$           (long_name='Fed Funds')                          % 25. observavel/aux p/ choque
    CDS_obs             ${CDS^{obs}}$                 (long_name='CDS')                                % 26. observavel/aux p/ choque
    
% DESAGREGAÇÃO DE ADMINISTRADOS - Todos observáveis  
    
    combustiveis        $Combustíveis$                (long_name='Gasolina + Diesel')
    energia             $Energia$                     (long_name='Energia Eletrica')
    botijao             $botijao$                     (long_name='Botijao de Gas')
    repetem             $Repetem$                     (long_name='Itens que repetem ao longo do ano todo')
    adm_outros          $Adm_ex$                      (long_name='Administrados ex aberturas selecionadas')
    adm_aberturas       $Adm_abs$                     (long_name='Administrados agregação dos itens modelados')
    
;

%----------------------------------------------------------------
% Observaveis
%----------------------------------------------------------------

% Juro natural observável até 2009. Depois deixo livre.
varobs pi_exp i pi_lsa pi_adm IPCA pi_meta pi_meta_anunc f_nuci f_pib f_unemp f_caged r_exante delta_e rp_hat_obs ie_hat_obs pi_star_obs pi_agro_obs pi_metal_obs pi_energia_obs pi_petro_obs clima_el_obs clima_la_obs i_star_obs CDS_obs dy_obs dcaged_obs r_eq combustiveis energia botijao repetem adm_outros 

;

%----------------------------------------------------------------
% Exogenous
%----------------------------------------------------------------

varexo rp_hat           ${\hat{rp}}$                    % observavel
       h_star           ${h^*}$                         % observavel, não será usado
       ie_hat           ${\hat{ie}}$                    % observavel
       eps_h            ${\varepsilon_h}$
 
       eps_pi_star_obs  ${\varepsilon}_{\pi^*}}}$  % resíduo do observado (erro de ponderação)
       eps_pi_lsa       ${\varepsilon_{pi}}$

       eps_meta         ${\varepsilon_{META}}$       
       eps_meta_anunc   ${\varepsilon_{META^{anunc}}}$
       eps_i            ${\varepsilon_i}$
       eps_selic_exp    ${\varepsilon_i_exp}$

       eps_pi_exp       ${\varepsilon_{\pi^e}}$
       clima_el         ${clima el nino}$               % observavel
       clima_la         ${clima la nina}$               % observavel
       eps_ADM          ${\varepsilon_{ADM}}$           % erro em relação ao observado
       eps_IPCA         ${\varepsilon_{IPCA}}$          % erro em relação ao observado
       eps_r_eq         ${\varepsilon_{r^eq}}$

       eps_pib          ${\varepsilon_{pib}}$ 
       eps_nuci         ${\varepsilon_{nuci}}$ 
       eps_unemp        ${\varepsilon_{unemp}}$ 
       eps_caged        ${\varepsilon_{caged}}$ 
    
       i_star           ${Fed Funds}$                   % observavel
       eps_e            ${\varepsilon_{e}}$ 
       CDS              ${CDS}$                         % observavel

       eps_pi_energia_obs   ${Energia}$   % observavel
       eps_pi_metal_obs     ${Metal}$     % observavel
       eps_pi_agro_obs      ${Agro}$      % observavel
       eps_pi_petro_obs     ${Brent}$     % observavel
       
      eps_dy_pot        ${\varepsilon_{y^{pot}}}$
      eps_gy_pot        ${\varepsilon_{g^{pot}_y}}$
      eps_dcaged_pot    ${\varepsilon_{{CAGED}^{pot}}}$
      eps_gcaged_pot    ${\varepsilon_{g^{pot}_{CAGED}}}$
      
      eps_combustiveis  ${\varepsilon_{combustiveis}}$
      eps_botijao       ${\varepsilon_{botijao}}$
      eps_energia       ${\varepsilon_{energia}}$
      eps_repetem       ${\varepsilon_{repetem}}$
      eps_adm_outros    ${\varepsilon_{admoutros}}$
       
;   

%----------------------------------------------------------------
% Parameters
%----------------------------------------------------------------

parameters 

beta_1  ${\beta_{11}}$
beta_2  ${\beta_{21}}$
beta_3  ${\beta_{30}}$
beta_4  ${\beta_{40}}$
beta_5  ${\beta_{50}}$
rho_ie  ${\rho_{ie}}$

alpha_1L ${\alpha_{1L}}$
alpha_1I ${\alpha_{1I}}$
alpha_2  ${\alpha_{2}}$
alpha_3  ${\alpha_{3}}$ 
alpha_4  ${\alpha_{4}}$
alpha_5  ${\alpha_{5}}$
alpha_6  ${\alpha_{6}}$

psi_comum    ${\psi_{comum}}$
psi_comum_e  ${\psi_{comume}}$
psi_comb_1   ${\psi_{comb1}}$
psi_comb_2   ${\psi_{comb2}}$
psi_botij_1  ${\psi_{botij1}}$
psi_botij_2  ${\psi_{botij2}}$

w_combustiveis    ${w_{comb}}$
w_botijao         ${w_{botij}}$
w_energia         ${w_{energia}}$
w_repetem         ${w_{repetem}}$
w_adm_aberturas   ${w_{admabert}}$
w_adm_outros      ${w_{admoutros}}$
w_livres          ${w_{livres}}$
w_adm             ${w_{adm}}$

theta_1 ${\theta_{1}}$
theta_2 ${\theta_{2}}$
theta_3 ${\theta_{k}}$
theta_l ${\theta_{l}}$

phi_1 ${\varphi_{11}}$
phi_2 ${\varphi_{2}}$
phi_3 ${\varphi_{3}}$

sigma_h      ${\sigma_h}$
gamma_nuci   ${\gamma_{nuci}}$
gamma_unemp  ${\gamma_{unemp}}$
gamma_caged  ${\gamma_{caged}}$

meta_dom_LP   ${\Pi^{METALP}}$
meta_externa  ${\Pi^{*META}}$
delta_e_ppc   ${\Delta_e^{PPC}}$
delta         ${\delta}$

rho_g_dy_pot           ${\rho_g^{y^{pot}}}$
g_y_ss                 ${\bar{g^{y^{pot}}}}$
rho_g_dcaged_pot       ${\rho_g^{caged^{pot}}}$
g_caged_ss             ${\bar{g^{caged^{pot}}}}$

;

beta_1 = 0.73897;    % BC: 0.738970 % 0.7882
beta_2 = -0.54876;   % BC: -0.54876 % -0.15900
beta_3 = -0.02985;   % BC: -0.02985 % -0.02910
beta_4 = -0.04073;   % BC: -0.04073 % -0.03910
beta_5 = 0.0;
rho_ie = 0.80;       % BC: 0.80

alpha_1L = 0.23756;   % BC: 0.23756 % 0.29530
alpha_1I = 0.25568;   % BC: 0.25568 % 0.29530
alpha_2  = 0.01826;   % BC: 0.01826 % 0.06810
alpha_3  = 0.01727;   % BC: 0.01727 % 0.09520            
alpha_4  = 0.13866;   % BC: 0.13866 % 0.33700
alpha_5  = 0.00119;   % BC: 0.00119 % 0.00260
alpha_6  = 0.00104;   % BC: 0.00104 % 0.00300

psi_comum   = 0.70; % Inercia de ADM em relacao ao IPCA AC.4Q
psi_comum_e = 0.10; % pass-through cambio (MM4Q)
psi_comb_1  = 0.55; % pass-through brent
psi_comb_2  = 0.50; % pass-through cambio
psi_botij_1 = 0.55; % pass-through brent
psi_botij_2 = 0.50; % pass-through cambio

w_combustiveis  = 0.25/0.71; % peso dentro de adm_aberturas
w_botijao       = 0.05/0.71; % peso dentro de adm_aberturas
w_energia       = 0.19/0.71; % peso dentro de adm_aberturas
w_repetem       = 0.22/0.71; % peso dentro de adm_aberturas
w_adm_aberturas = 0.71; % peso dentro de administrados total
w_adm_outros    = 0.29; % peso dentro de administrados total
w_livres        = 0.73; % peso no IPCA
w_adm           = 0.27; % peso no IPCA

theta_1 =  1.45688;   % BC: 1.45688 % 1.56010
theta_2 = -0.54402;   % BC: -0.54402 % -0.70820
theta_3 =  1.29981;   % BC: 1.29981  % 2.09120    
theta_l =  0.0;       % BC: 0.0

phi_1 = 0.73260;    % BC: 0.73260 % 0.60530
phi_2 = 0.12271;    % BC: 0.12271 % 0.20550
phi_3 = 0.04370;    % BC: 0.04370 % 0.06250

sigma_h     = 1.15590; 
gamma_nuci  = 2.47040; 
gamma_unemp = 0.98950; 
gamma_caged = 0.78730; 

meta_externa = 2.00;
meta_dom_LP = 3.00;
delta_e_ppc = (meta_dom_LP - meta_externa)/4; % diferencial de metas de inflação
delta = 1.71813;  % = BC*4 pq diferencial de juros entra anualizado no modelo do BC!

rho_g_dy_pot     = 0.9706;
g_y_ss           = 0.5345;
rho_g_dcaged_pot = 0.9676;
g_caged_ss       = 0.5948;

%----------------------------------------------------------------
% Model Equations
%----------------------------------------------------------------

% options_.TeX=1;

model(linear);

% 1. IS Curve, eq. (1)
h = beta_1*h(-1) + 
    beta_2*r_hat(-1) + 
    beta_3*rp_hat + 
    beta_5*h_star + 
    beta_4*ie_hat_obs + eps_h;

% aux: Hiato de Juros
r_hat = r_exante - pi_exp - r_eq;
r_exante = (0.5*i + (EXPECTATION(0)(i(+1) + i(+2) + i(+3) + 0.5*i(+4))))/4 + eps_selic_exp;

% 2. Phillips Curve, eq. (2)
pi_lsa =  alpha_1L*pi_lsa(-1) +
          alpha_1I*IPCA_acc4q(-1) +
          (1 - alpha_1L - alpha_1I)*pi_exp + 
          alpha_2*(pi_star_obs - meta_externa + delta_e_hat) +
          alpha_3*delta_e_hat(-2) +
          alpha_4*h + 
          alpha_5*(clima_el_obs + clima_el_obs(-1) + clima_el_obs(-2))/3 - alpha_5*(clima_el_obs(-3) + clima_el_obs(-4) + clima_el_obs(-5))/3 +
          alpha_6*(clima_la_obs + clima_la_obs(-1) + clima_la_obs(-2))/3 - alpha_6*(clima_la_obs(-3) + clima_la_obs(-4) + clima_la_obs(-5))/3 +
          eps_pi_lsa;

% 2. Itens administrados modelados e não modelados ('adm_outros').
% OBS 1: Todos os componentes compartilham da mesma estrutura de inércia com o IPCA4Q e meta. Bom para projeções incondicionais, sem atrapalhar projs. condicionais.
% OBS 2: É possível ancorar os administrados nas expectativas em vez da meta. Não é claro qual faz mais sentido.
% OBS 3: Retirar psi_comum de combustiveis e botijão, pq está elevando a inércio de choques no petróleo articifialmente.
combustiveis  =  psi_comum*IPCA_acc4q(-1) + (1-psi_comum)*pi_meta + psi_comb_1*(pi_petro_obs - meta_externa) + psi_comb_2*delta_e_hat + eps_combustiveis; 
botijao       =  psi_comum*IPCA_acc4q(-1) + (1-psi_comum)*pi_meta + psi_botij_1*(pi_petro_obs - meta_externa)+ psi_botij_2*delta_e_hat + eps_botijao; 
energia       =  psi_comum*IPCA_acc4q(-1) + (1-psi_comum)*pi_meta + eps_energia;
repetem       =  psi_comum*IPCA_acc4q(-1) + (1-psi_comum)*pi_meta + eps_repetem;
adm_outros    =  psi_comum*IPCA_acc4q(-1) + (1-psi_comum)*pi_meta + psi_comum_e*(delta_e_hat(-4) + delta_e_hat(-3) + delta_e_hat(-2) + delta_e_hat(-1))/4 + eps_adm_outros;

% Agregações: administrados e IPCA. Erros de medida acomodam erros de ponderação.
adm_aberturas =  w_botijao*botijao + w_energia*energia + w_repetem*repetem + w_combustiveis*combustiveis;
pi_adm        =  w_adm_aberturas*adm_aberturas + w_adm_outros*adm_outros + eps_ADM; 
IPCA          =  w_livres*pi_lsa + w_adm*pi_adm + eps_IPCA; % atualizar todos os pesos conforme forem mudando.
          
% 3. Taylor Rule, eq. (3)
i = theta_1*i(-1) + theta_2*i(-2) + 
    (1 - theta_1 - theta_2)*(r_eq + pi_meta + theta_3*(pi_exp - pi_meta)) + eps_i;

% 4. Inflation expectations (latent), eq. (4)
pi_exp_hat = phi_1*pi_exp_hat(-1) + 
             phi_2*(EXPECTATION(0)((IPCA(+1) + IPCA(+2) + IPCA(+3) + IPCA(+4))/4) - pi_meta) + 
             phi_3*(pi_desv(-1) + pi_desv(-2) + pi_desv(-3) + pi_desv(-4))/4 + 
             eps_pi_exp;

% aux. pi_exp_hat and pi_desv
pi_exp_hat     = pi_exp - pi_meta;
pi_desv        = IPCA - pi_meta;
pi_meta        = pi_meta_anunc(-6) + eps_meta;
pi_meta_anunc  = pi_meta_anunc(-1) + eps_meta_anunc;

% 5. Taxa de juros de equilíbrio - passeio aleatório (não aparece no box) - PENSAR! USAR PRIOR DIFFUSA - OLHAR FORUM - infinity of steady states
r_eq = r_eq(-1) + eps_r_eq;

% 8, 9, 10 e 11. Medidas de atividade eq. (4) - Box assume implicitamente que os choques aqui são N(0,sigma_h)
f_pib                       = h + sigma_h*eps_pib;
f_nuci/gamma_nuci           = h + sigma_h*eps_nuci;
f_unemp/gamma_unemp         = h(-1) + sigma_h*eps_unemp;
f_caged/gamma_caged         = h(-1) + sigma_h*eps_caged;

% 4, 5 e 6. Bloco UIP
i_dif        = 4*(i - (i_star + CDS/100));                       % diferencial de juros ajustado pelo risco (CDS). CDS observado trimestral.
delta_e      = delta_e_ppc - delta*(i_dif - i_dif(-1)) + eps_e;  % depreciação prevista pela PPC
delta_e_hat  = delta_e - delta_e_ppc;                            % desvio do câmbio para além do previsto pela PPC

% AUXILIARES
pi_lsa_acc4q = pi_lsa(-3)/4 + pi_lsa(-2)/4 + pi_lsa(-1)/4 + pi_lsa/4; % média trimestral
IPCA_acc4q   = IPCA(-3)/4 + IPCA(-2)/4 + IPCA(-1)/4 + IPCA/4; % média trimestral

% MAPA DAS OBSERVÁVEIS - explicitando a subtração da meta doméstica e PPC

pi_energia_obs = (1-0.34)*(meta_externa) + 0.34*pi_petro_obs + eps_pi_energia_obs;
pi_metal_obs   = meta_externa + eps_pi_metal_obs;
pi_agro_obs    = meta_externa + eps_pi_agro_obs;
pi_petro_obs   = meta_externa + eps_pi_petro_obs;
pi_star_obs    = 0.18*pi_metal_obs + 0.68*pi_agro_obs + 0.14*pi_energia_obs + eps_pi_star_obs; % pesos estimados no R, amostra a partir de 2017. 0.18, 0.63, 0.19

rp_hat_obs    = rp_hat;
ie_hat_obs    = rho_ie*ie_hat_obs(-1) + ie_hat;
clima_el_obs  = clima_el;
clima_la_obs  = clima_la;
i_star_obs    = i_star;
CDS_obs       = CDS;

% CRESCIMENTO POTENCIAL PIB E CAGED
f_pib - f_pib(-1)      = dy_obs - dy_pot;
dy_pot                 = gy_pot + eps_dy_pot;
gy_pot                 = rho_g_dy_pot*gy_pot(-1) + (1-rho_g_dy_pot)*g_y_ss + eps_gy_pot;

f_caged - f_caged(-1)  = dcaged_obs - dcaged_pot;
dcaged_pot             = gcaged_pot + eps_dcaged_pot;
gcaged_pot             = rho_g_dcaged_pot*gcaged_pot(-1) + (1-rho_g_dcaged_pot)*g_caged_ss + eps_gcaged_pot;

end;

%----------------------------------------------------------------
% Steady State
%----------------------------------------------------------------

steady_state_model;

% Definem o ss do sistema todo.
r_eq = 4/4;
pi_meta_anunc = meta_dom_LP/4;
pi_meta = pi_meta_anunc;

% dependem dos 'parâmetros' acima: meta e taxa natural inicial
i = r_eq + pi_meta;
pi_exp = pi_meta;
pi_desv = 0;
IPCA = pi_meta;
r_exante = i;
pi_lsa = pi_meta;
pi_adm = pi_meta;
pi_lsa_acc4q = pi_meta;
IPCA_acc4q = pi_meta;

% aberturas de administrados
combustiveis = pi_meta;
botijao = pi_meta;
energia = pi_meta;
repetem = pi_meta;
adm_outros = pi_meta;
adm_aberturas =  w_botijao*botijao + w_energia*energia + w_repetem*repetem + w_combustiveis*combustiveis;

i_dif = 4*i;
delta_e_hat = 0;
delta_e = delta_e_ppc;

% sempre 0, são desvios do ss
h = 0;
r_hat = 0;
pi_exp_hat = 0;
f_nuci = 0;
f_pib = 0;
f_unemp = 0;
f_caged = 0;

% choques observaveis
pi_star_obs    = meta_externa;
pi_energia_obs = meta_externa;
pi_metal_obs   = meta_externa;
pi_agro_obs    = meta_externa;
pi_petro_obs   = meta_externa;

rp_hat_obs = 0;
ie_hat_obs = 0;
clima_el_obs = 0;
clima_la_obs = 0;
i_star_obs = 0;
CDS_obs = 0;

% crescimento potencial PIB e CAGED
gy_pot    = g_y_ss;
dy_pot    = gy_pot;
dy_obs    = dy_pot;                     

gcaged_pot    = g_caged_ss;
dcaged_pot    = gcaged_pot;
dcaged_obs    = dcaged_pot;                     

end;

%----------------------------------------------------------------
% Shocks
%----------------------------------------------------------------

shocks;

var rp_hat;         stderr 1.00;
var h_star;         stderr 1.00; 	 
var ie_hat;         stderr 16;  
var eps_h;          stderr 0.2971;             % estimado
 
var eps_pi_star_obs;    stderr 1;
var eps_pi_energia_obs; stderr 1;
var eps_pi_metal_obs;   stderr 1;
var eps_pi_agro_obs;    stderr 1;
var eps_pi_petro_obs;   stderr 1;
var eps_pi_lsa;         stderr 2.3751/4;             % estimado

var eps_meta;       stderr 0.01;
var eps_meta_anunc; stderr 0.01;
var eps_i;          stderr 0.5305/4;             % estimado
var eps_selic_exp;  stderr 0.9012/4;             % estimado

var clima_el;       stderr 60;
var clima_la;       stderr 60;
var eps_pi_exp;     stderr 0.2543/4;             % estimado
var eps_ADM;        stderr 5/4;
var eps_IPCA;       stderr 2.8/4;

var eps_r_eq;       stderr 0.12/4;             % BC = 0.0936

var eps_pib;        stderr 1; 
var eps_nuci;       stderr 1;
var eps_unemp;      stderr 1; 
var eps_caged;      stderr 1; 

var i_star;         stderr 1.25/4;
var eps_e;          stderr 7.5;
var CDS;            stderr 120/4;

var eps_dy_pot;     stderr 0.2652;               % estimado
var eps_gy_pot;     stderr 0.0976;               % estimado
var eps_dcaged_pot; stderr 0.2671;               % estimado
var eps_gcaged_pot; stderr 0.1398;               % estimado

var eps_combustiveis; stderr 1;
var eps_botijao;      stderr 1;
var eps_energia;      stderr 1;
var eps_repetem;      stderr 1;
var eps_adm_outros;   stderr 1;

end;

%resid;

%----------------------------------------------------------------
% LaTeX Model
%----------------------------------------------------------------

% write_latex_original_model;
% write_latex_parameter_table;

%----------------------------------------------------------------
% IRFs & Solution
%----------------------------------------------------------------

% remove option 'noprint' to get POLICY AND TRANSITION FUNCTIONS
stoch_simul(irf=24, nodecomposition, nocorr, nomoments, relative_irf, nograph, irf_plot_threshold = 1e-5);

%----------------------------------------------------------------
% Unconditional forecast
%----------------------------------------------------------------
@#if projecoes_incondicionais == 1
    
    set_dynare_seed('default')

    calib_smoother(datafile = dataset, diffuse_filter);
    
    smoother2histval;

    forecast(periods = 12, conf_sig = 0.90, nograph, nodisplay); 

@#endif

%----------------------------------------------------------------
% Shock decomposition (for counterfactuals)
%----------------------------------------------------------------
@#if shock_decomposition == 1
    
    set_dynare_seed('default')
    
    calib_smoother(datafile = dataset, diffuse_filter);
    
    shock_decomposition(nograph);

@#endif

%----------------------------------------------------------------
% Conditional Forecast Scenario
%----------------------------------------------------------------
@#if projecoes_condicionais == 1
    
    set_dynare_seed('default')
    
    calib_smoother(datafile = dataset, diffuse_filter);
    
    smoother2histval;

    @#include "proj_cond.mod"

@#endif

%----------------------------------------------------------------
% Conditional Forecast Scenario - DELTA ENTRE COPOMS
%----------------------------------------------------------------
@#if projecoes_condicionais_delta_COPOM == 1
    
    set_dynare_seed('default')

    @#include "proj_cond_delta_COPOM.mod"
    
@#endif

% -------------------------------------
% SMOOTHED VARIABLES 
% -------------------------------------

@#if smoothed_variables == 1
    
    set_dynare_seed('default')

    scen = struct(); % struct vazio p/ armazenas cenários

    calib_smoother(datafile = dataset, diffuse_filter);
    
@#endif



%% AUX
%% ETAPA 1.1 - Build dataset
% Choose excel range properly (last observation)
filename = '\Database.xlsx';
sheet = 'BASE_TRI';
xlRange = 'C3:AI77'; % ATENÇÃO: mar/22 = 77

% load excel
[data, header] = xlsread(filename,sheet,xlRange);

% create variables dynamically
for a = 1:numel(header)
    eval([char(header(a)) '= data(:,a);'])
end

% save dataset
save('dataset.mat','pi_exp', 'i', 'pi_lsa', 'pi_adm', 'IPCA', 'pi_meta', 'pi_meta_anunc', 'f_nuci', 'f_pib', 'f_unemp', 'f_caged', 'r_exante', 'delta_e', 'rp_hat_obs', 'ie_hat_obs', 'pi_star_obs', 'pi_agro_obs', 'pi_metal_obs', 'pi_energia_obs', 'pi_petro_obs', 'clima_el_obs', 'clima_la_obs', 'i_star_obs', 'CDS_obs', 'dy_obs', 'dcaged_obs', 'h', 'r_eq', 'combustiveis', 'energia', 'botijao', 'repetem', 'adm_outros');

%% ETAPA 1.2 - Auxiliary variables for dates/tables
                     
t0 = datetime(2003,12,1); % first obs date
Dates = t0' + 3*calmonths(0:(length(i) - 1))'; % fill for smoothed sample size
Dates_forecast = t0' + 3*calmonths(0:(length(i) - 1 + 12))'; % fill for smoothed + forecast sample size (12 periods ahead)
Dates_scenario = t0' + 3*calmonths((length(i)-1):(length(i) - 1 + 12))'; % forecast sample size only (12 periods ahead, includes base period)

table_date  = array2table(Dates);
table_date_forecast  = array2table(Dates_forecast);
table_date_scenario  = array2table(Dates_scenario);

save('dates_aux.mat','Dates', 'Dates_forecast', 'Dates_scenario', 'table_date', 'table_date_forecast', 'table_date_scenario') 


%% ETAPA 1.3 - clear workspace

clear all
