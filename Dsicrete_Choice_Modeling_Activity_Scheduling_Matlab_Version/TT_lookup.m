%% Travel Time Look up Functions

%% Function
function TT_value = TT_lookup(t, l, d_tilde, m_tilde, time_cost_data)
    % TT_lookup: Finds travel time between zones l -> d_tilde for a mode
    %
    % Inputs:
    % - t: current time (not used in this basic version)
    % - l: current location (FROM)
    % - d_tilde: destination (TO)
    % - m_tilde: mode ('car' or 'walk')
    % - time_cost_data: table with columns FROM, TO, MODE, TT
    %
    % Output:
    % - TT_value: travel time, or Inf if not found

    % Logical filter for matching row
    row = time_cost_data( ...
        time_cost_data.FROM == l & ...
        time_cost_data.TO == d_tilde & ...
        strcmp(time_cost_data.MODE, m_tilde), :);

    % Return TT if found, else Inf
    if isempty(row)
        TT_value = Inf;
    else
        TT_value = row.TT;
    end
end

% TT_value = TT_lookup(0, 2, 5, 'car', time_cost_data)

