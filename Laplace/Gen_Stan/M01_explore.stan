functions {
#include "lts.stanfunctions"

}
// Laplace Library Imports

// Data Input Block =====================================================================================================
data{
    int<lower = 1> T;                              // Weeks
    int<lower = 1> K;                              // Weeks in the reference price eg(4)
    int<lower = 0> H;                              // Forecast horizon

    // Outcome
    array[T] int<lower = 0> weekly_sales;          // Outcome

    // Covariates
    vector <lower = 0>[T]              weekly_price;  // Prices
    array[T] int<lower = 0, upper = 1> F;             // Featured product indicator
    vector <lower = 0, upper = 1>[T]   BF;            // Black Friday indicator

    // Planned covariates for the forecast weeks 
    vector <lower = 0>[H]                 weekly_price_new;
    vector <lower = 0, upper = 1>[H]      BF_new;  
    array[H] int<lower = 0, upper = 1>    F_new;

    // Test The model internally
    int <lower = 0, upper = 1> test_switch;
    array[H] int<lower = 0> y_test;
  
}
// Transformed Data Block =============================================================================================
transformed data{
    int N = T - K;                                 // Modelled weeks

    // Log prices for the observed and the planned weeks, in one vector
    vector[T + H] log_price = log(append_row(weekly_price, weekly_price_new));
    real mean_log_price     = mean(log_price[(K + 1):T]);   

    // Price terms
    vector[N] p;                                   // Centred log price
    vector[N] d;                                   // Log price minus the mean of the previous K log prices
    for(n in 1:N){
        int t = n + K;
        p[n] = log_price[t] - mean_log_price;
        d[n] = log_price[t] - mean(log_price[(t - K):(t - 1)]);
    }

    // Same terms for the forecast weeks
    vector[H] p_new;
    vector[H] d_new;
    for(h in 1:H){
        int t = T + h;
        p_new[h] = log_price[t] - mean_log_price;        // TRAINING mean
        d_new[h] = log_price[t] - mean(log_price[(t - K):(t - 1)]);
    }

    // Modelled slices of the outcome and covariates
    array[N] int y   = weekly_sales[(K + 1):T];
    array[N] int F_m = F[(K + 1):T];
    vector[N] BF_m   = BF[(K + 1):T];

    // Data part of the starting level: log of the mean of the first 4 modelled weeks, + 1 for zeros
    real L_start = log(mean(to_vector(y[1:4])) + 1);
}

// Model Parameters =====================================================================================================
parameters{
    // NB overdispersion
    real<lower = 0> phi;

    // Level and slope
    real z1;                  // Start level, standardized
    real s1;                  // Start slope, standardised
    real<lower = 0> sigma_b;  // Slope innovation scale
    real<lower = 0> sigma_L;  // Level innovation scale
    vector[N - 1] zeta_b;     // Standardised slope shocks
    vector[N - 1] zeta_L;     // Standardised level shocks

    // Effects
    real beta_p;                                   // Price elasticity
    real delta_d;                                  // Response to a discount vs the reference price
    real kappa_BF;                                 // Black Friday lift
    real<lower = 0, upper = 1> lambda;             // Feature adstock decay
    real gamma_F;                                  // Feature effect
}

// Transformed Parameters ================================================================================================
transformed parameters{
    real L1 = L_start + z1;
    real b1 = 0.05 * s1;

    // Argument order: (delta1, mu1, sigma_delta, sigma_mu, zeta_delta, zeta_mu) -> slope first
    vector[N] L = lts__local_linear_trend(b1, L1, sigma_b, sigma_L, zeta_b, zeta_L);

    // Feature adstock
    vector[N] a;
    a[1] = F_m[1];
    for(n in 2:N){
        a[n] = F_m[n] + lambda * a[n - 1];
    }

    // Linear predictor 
    vector[N] eta = L + gamma_F * a + beta_p * p + delta_d * d + kappa_BF * BF_m;
}

// Model ================================================================================================================
model{
    // Priors
    z1 ~ std_normal();
    s1 ~ std_normal();
    zeta_b ~ std_normal();
    zeta_L ~ std_normal();
    sigma_b ~ exponential(50);
    sigma_L ~ normal(0, 0.1);
    lambda  ~ beta(2, 3);
    beta_p  ~ normal(-1.5, 0.75);
    delta_d ~ normal(-0.5, 0.5);
    gamma_F ~ normal(0.3, 0.5);
    kappa_BF ~ normal(0.5, 0.75);
    phi      ~ gamma(2, 0.1);

    // Likelihood
    y ~ neg_binomial_2_log(eta, phi);
}

// Generated Quantities ===============================================================================================
generated quantities{
    // Forecast in the Future Horizon 
    vector[N] b   = lts__local_linear_trend_slope(b1, sigma_b, zeta_b);
    vector[H] L_f = lts__local_linear_trend_forecast_rng(L[N], b[N], sigma_L, sigma_b, H);
    array[H] int y_f;
    {
        real a_cur = a[N];
        for(h in 1:H){
            a_cur  = F_new[h] + lambda * a_cur;
            y_f[h] = neg_binomial_2_log_rng(
                L_f[h] + gamma_F   * a_cur 
                       + beta_p    * p_new[h]
                       + delta_d   * d_new[h] 
                       + kappa_BF  * BF_new[h]
                       ,phi
                    );
        }
    }
    vector[N] log_lik;
    for(n in 1:N) log_lik[n] = neg_binomial_2_log_lpmf(y[n] | eta[n], phi);

    // Internal Model Test ============================================================================================
    if(test_switch == 1){
        vector[H] forecasted = to_vector(y_f);
        vector[H] truth      = to_vector(y_test);
        // Calculate loss metrics of interest from the forecasted sales and the provided y_test
        real rmse = sqrt(mean(square(truth - forecasted)));
        real mae  = mean(abs(truth - forecasted));
        real mse  = mean(square(truth - forecasted));
    }
}