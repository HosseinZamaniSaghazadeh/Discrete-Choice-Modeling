%% Travel Choice Set (General)

%% Function
function travel_choice_set_general = get_C_travel_general(m, h, l, delta_car_value)

    % Get available purposes and modes
    available_p = get_P_n(h);
    available_m = get_M_n(delta_car_value, m);

   travel_choice_set_general = {};  % initialize as cell array
    idx = 1;

    for i = 1:length(available_p)
        p_tilde = available_p{i};

        % Define destinations based on purpose
        if p_tilde == "shop"
            destinations = [5, 6];
        elseif p_tilde == "work"
            destinations = [3, 4];
        elseif p_tilde == "home"
            destinations = [1, 2];
        else
            error("Unknown purpose: %s", p_tilde);
        end

        for d = 1:length(destinations)
            d_tilde = destinations(d);

            for j = 1:length(available_m)
                m_tilde = available_m{j};

                % Skip car trips to same zone
                if m_tilde == "car" && l == d_tilde
                    continue;
                end

                % Allow same-zone walk
                if m_tilde == "walk" && l == d_tilde
                    travel_choice_set_general{idx} = struct( ...
                        'd_tilde', d_tilde, ...
                        'm_tilde', "walk", ...
                        'p_tilde', p_tilde);
                    idx = idx + 1;
                    continue;
                end

                % General case
                travel_choice_set_general{idx} = struct( ...
                    'd_tilde', d_tilde, ...
                    'm_tilde', m_tilde, ...
                    'p_tilde', p_tilde);
                idx = idx + 1;
            end
        end
    end
end

%{
travel_choice_set_general = get_C_travel_general('car',0,4,0)

% Convert cell array to struct array
travel_choice_set_general_structs = [travel_choice_set_general{:}];

% Turn into a table
travel_choice_set_general_table = struct2table(travel_choice_set_general_structs);

% Display table
disp(ctravel_choice_set_general_table);
%}