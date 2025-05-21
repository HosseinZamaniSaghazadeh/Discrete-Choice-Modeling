%% Travel Cost Look up Functions

%% Function
function TC_value = TC_lookup(t, l, d_tilde, m_tilde, time_cost_data)
    % TC_lookup: Returns travel cost from l to d_tilde using mode m_tilde
    % Inputs same as TT_lookup, but returns TC (Travel Cost)
    
    row = time_cost_data( ...
        time_cost_data.FROM == l & ...
        time_cost_data.TO == d_tilde & ...
        strcmp(time_cost_data.MODE, m_tilde), :);

    if isempty(row)
        TC_value = Inf;
    else
        TC_value = row.TC;
    end
end

% TT_value = TT_lookup(0, 2, 5, 'car', time_cost_data)