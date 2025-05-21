function final_choice_set_individual = get_C_individual(person_id, m, h, tau, l, p, delta_car_n, L_act_n_list)

    if tau == 0
        % Only staying is allowed
        final_choice_set_individual = {get_C_stay(l, p)};
        
    elseif tau == 1
        % Generate travel choices for this person
        final_choice_set_individual = get_C_travel_individual(person_id, m, h, l, delta_car_n, L_act_n_list);
        
    else
        error("Invalid tau value provided!");
    end
end

%{
final_choice_set_individual =  get_C_individual(1, "walk", 0, 1, 5, "home", delta_car_n, L_act_n_list)

% Convert cell array to struct array
final_choice_set_individual_structs = [final_choice_set_individual{:}];

% Turn into a table
final_choice_set_individual_table = struct2table(final_choice_set_individual_structs);

% Display table
disp(final_choice_set_individual_table);
%}