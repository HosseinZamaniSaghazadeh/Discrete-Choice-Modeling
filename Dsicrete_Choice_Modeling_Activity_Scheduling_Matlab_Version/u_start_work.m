%% Start Working Utility Function

%% Function
function utility_start_work = u_start_work(t, theta_work_t)
    % u_start_work: Computes interpolated utility for starting work at time t
    % theta_work_t: containers.Map with keys = time strings, values = utility

    % Extract and sort numeric keys
    t_keys = keys(theta_work_t);
    t_grid = sort(str2double(t_keys));

    % Handle boundaries
    if t <= min(t_grid)
        t_j = min(t_grid);
        t_j1 = t_grid(find(t_grid == t_j, 1) + 1);
    elseif t >= max(t_grid)
        t_j1 = max(t_grid);
        t_j = t_grid(find(t_grid == t_j1, 1) - 1);
    else
        t_j = max(t_grid(t_grid <= t));
        t_j1 = min(t_grid(t_grid >= t & t_grid ~= t_j));
    end

    % Direct match
    if ismember(t, t_grid)
        utility_start_work = theta_work_t(num2str(t));
        return;
    end

    % Interpolate
    alpha1 = (t_j1 - t) / (t_j1 - t_j);
    alpha2 = (t - t_j) / (t_j1 - t_j);

    u_j  = theta_work_t(num2str(t_j));
    u_j1 = theta_work_t(num2str(t_j1));

    utility_start_work = u_j * alpha1 + u_j1 * alpha2;
end
