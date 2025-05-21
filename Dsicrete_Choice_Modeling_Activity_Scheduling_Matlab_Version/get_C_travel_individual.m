%% Travel Choice Set (Individuals)

%% Function
function travel_choice_set_individual = get_C_travel_individual(person_id, m, h, l, delta_car_n, L_act_n_list)
    % Get available purposes and modes
    available_p = get_P_n(h);
    delta_value = delta_car_n(person_id);  % access car availability
    available_m = get_M_n(delta_value, m);

    travel_choice_set_individual = {};
    idx = 1;

    % Loop through purposes
    for i = 1:length(available_p)
        p_tilde = available_p{i};

        % Get person's allowed destinations for this purpose
        destinations = L_act_n_list(person_id).(p_tilde);

        for d = 1:length(destinations)
            d_tilde = destinations(d);

            for j = 1:length(available_m)
                m_tilde = available_m{j};

                % Skip car to same zone
                if m_tilde == "car" && l == d_tilde
                    continue;
                end

                % Same-zone walk allowed
                if m_tilde == "walk" && l == d_tilde
                    travel_choice_set_individual{idx} = struct( ...
                        'd_tilde', d_tilde, ...
                        'm_tilde', "walk", ...
                        'p_tilde', p_tilde);
                    idx = idx + 1;
                    continue;
                end

                % General case
                travel_choice_set_individual{idx} = struct( ...
                    'd_tilde', d_tilde, ...
                    'm_tilde', m_tilde, ...
                    'p_tilde', p_tilde);
                idx = idx + 1;
            end
        end
    end
end

%{
travel_choice_set_individual = get_C_travel_individual(1, 'car', 0, 2, delta_car_n, L_act_n_list)

% Convert cell array to struct array
travel_choice_set_individual_structs = [travel_choice_set_individual{:}];

% Turn into a table
travel_choice_set_individual_table = struct2table(travel_choice_set_individual_structs);

% Display table
disp(travel_choice_set_individual_table);
%}