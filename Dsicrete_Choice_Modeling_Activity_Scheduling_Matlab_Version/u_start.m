%% Starting Activity Utility Function

%% Function
function utility_start_activity = u_start(l, t, h, p_tilde, ...
                                 x_pls, theta_shop_s, ...
                                 theta_C_shop, theta_size_shop, ...
                                 theta_work_t, ...
                                 t_work_start_after, t_work_start_before)
    % u_start: Computes the utility of starting an activity at location l and time t

    if p_tilde == "shop"
        utility_start_activity = u_p_size(l, p_tilde, x_pls, theta_shop_s, theta_C_shop, theta_size_shop);

    elseif p_tilde == "work"
        % Check time constraint for starting work
        work_constraint = u_work_constraint(t, t_work_start_after, t_work_start_before);
        
        % Interpolate utility from theta_work_t
        start_work_utility = u_start_work(t, theta_work_t);
        
        utility_start_activity = work_constraint + start_work_utility;

    elseif p_tilde == "home"
        utility_start_activity = 0;

    else
        error("Invalid p_tilde provided in u_start()");
    end
end
