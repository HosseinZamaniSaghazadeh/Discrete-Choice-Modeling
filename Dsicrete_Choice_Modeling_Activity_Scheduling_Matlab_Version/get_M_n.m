%% Mode
% get_M_n(delta_car_n["1"], "stay") = the output is 'car' 'walk' because the person 1 has car and currently at home

%% Function
function modes = get_M_n(delta_car_value, m)
    if delta_car_value == 0
        modes = {"walk"};
    else
        switch m
            case "stay"
                modes = {"car", "walk"};
            case "car"
                modes = {"car"};
            case "walk"
                modes = {"walk"};
            otherwise
                error("Invalid mode!");
        end
    end
end
