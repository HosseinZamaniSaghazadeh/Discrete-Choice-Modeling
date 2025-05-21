%% Set of Available Activity Purpose

%% Function
function purposes = get_P_n(h)
    if h == 0
        purposes = {"home", "work", "shop"};
    elseif h == 1
        purposes = {"home", "shop"};
    else
        error("Invalid h value!");
    end
end