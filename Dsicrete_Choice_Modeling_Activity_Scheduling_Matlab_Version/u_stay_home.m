%% Stay Home Utility Function

%% Function
function stay_home_utility = u_stay_home(t, theta_home_t, delta_t)
    % u_stay_home: Computes stay-home utility with struct-style time coefficients

    % Step 1: Get grid from field names like 's0', 's60', etc.
    field_names = fieldnames(theta_home_t);
    t_grid = sort(cellfun(@(f) str2double(f(2:end)), field_names));  % remove 's' prefix

    % Step 2: Find bounding time grid points
    t_j = max(t_grid(t_grid <= t));
    t_j1_candidates = t_grid(t_grid > t);
    
    if isempty(t_j1_candidates)
        % If t is at or beyond the last grid point
        t_j = t_grid(end-1);
        t_j1 = t_grid(end);
    else
        t_j1 = t_j1_candidates(1);
    end

    % Build field names
    s_j = sprintf('s%d', t_j);
    s_j1 = sprintf('s%d', t_j1);

    % Step 3: If exact match
    if ismember(t, t_grid)
        stay_home_utility = theta_home_t.(sprintf('s%d', t)) * delta_t;
        return;
    end

    % Step 4: Interpolation
    alpha1 = (t_j1 - t - 0.5 * delta_t) / (t_j1 - t_j);
    alpha2 = (t + 0.5 * delta_t - t_j) / (t_j1 - t_j);

    u_j = theta_home_t.(s_j);
    u_j1 = theta_home_t.(s_j1);

    stay_home_utility = u_j * delta_t * alpha1 + u_j1 * delta_t * alpha2;
end
