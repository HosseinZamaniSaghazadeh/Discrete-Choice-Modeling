%% Travel Utility Function

%% Function
function utility_travel = u_travel(t, l, m, m_tilde, d_tilde, ...
                                   theta_mode_constant, theta_TT_mode, ...
                                   theta_cost, theta_sz_walk, time_cost_data)
    % u_travel: Computes travel utility for a given action

    % --- Mode constant ---
    if m_tilde == "car" || m_tilde == "walk"
        mode_constant = theta_mode_constant.(m_tilde);
    elseif m_tilde == "stay"
        mode_constant = 0;
    else
        error("Invalid mode in u_travel()");
    end

    % --- Travel Time coefficient ---
    if m_tilde == "car" || m_tilde == "walk"
        mode_TT_coef = theta_TT_mode.(m_tilde);
    else
        mode_TT_coef = 0;
    end

    % --- Get travel time and cost ---
    TT_value = TT_lookup(t, l, d_tilde, m_tilde, time_cost_data);
    TC_value = TC_lookup(t, l, d_tilde, m_tilde, time_cost_data);

    % --- Same-zone walk bonus ---
    delta_sz = delta_szwalk(l, d_tilde, m_tilde);

    % --- Full utility expression ---
    utility_travel = mode_constant + ...
                     mode_TT_coef * TT_value + ...
                     theta_cost * TC_value + ...
                     theta_sz_walk * delta_sz;
end

%{
utility_travel = u_travel(0, 2, "walk", "car", 5, ...
             theta_mode_constant, theta_TT_mode, ...
             theta_cost, theta_sz_walk, time_cost_data)
%}