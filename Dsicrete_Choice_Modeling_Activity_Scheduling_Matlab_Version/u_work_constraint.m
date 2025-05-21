%% Work Constraint Utility Function

%% Function
function utility_work_constraint = u_work_constraint(t, t_work_start_after, ...
    t_work_start_before)
    % u_work_constraint: Returns utility for starting work at time t
    if t >= t_work_start_after && t <= t_work_start_before
        utility_work_constraint = 0;  % valid time
    else
        utility_work_constraint = -Inf;  % not allowed
    end
end