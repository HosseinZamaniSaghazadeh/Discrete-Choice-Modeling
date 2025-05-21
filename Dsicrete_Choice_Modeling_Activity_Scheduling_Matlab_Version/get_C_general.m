%% # Full Choice Set (General)

%% Function
function final_choice_set_general = get_C_general(m, h, tau, l, p, delta_car_value)

    if tau == 0
        % Only "stay" is allowed
        final_choice_set_general = {get_C_stay(l, p)};
        
    elseif tau == 1
        % Generate general travel choice set
        final_choice_set_general = get_C_travel_general(m, h, l, delta_car_value);
        
    else
        error("Invalid tau value provided!");
    end
end

%{
final_choice_set_general = get_C_general("walk", 0, 1, 2, "home", delta_car_value)

% Convert cell array to struct array
final_choice_set_general_structs = [final_choice_set_general{:}];

% Turn into a table
final_choice_set_general_table = struct2table(final_choice_set_general_structs);

% Display table
disp(final_choice_set_general_table);
%}