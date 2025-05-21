%% Activity Utility Function

%% Function
function activity_utility = u_act(t, p_tilde, tau, theta_t_shop, theta_home_t, delta_t)
    % u_act: Computes utility of performing an activity of type p_tilde

    if p_tilde == "shop"
        activity_utility = theta_t_shop * delta_t;

    elseif p_tilde == "home"
        activity_utility = u_stay_home(t, theta_home_t, delta_t);

    elseif p_tilde == "work"
        activity_utility = 0;

    else
        error("Invalid activity purpose p_tilde!");
    end
end