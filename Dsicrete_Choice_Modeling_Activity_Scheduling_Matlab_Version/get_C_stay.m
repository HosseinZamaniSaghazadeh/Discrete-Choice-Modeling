%% Stay Choice Set (General and Individual)
% C_stay_n = get_C_stay(l, p)

%% Function
function stay_choice_set = get_C_stay(l, p)
    stay_choice_set = struct( ...
        'd_tilde', l, ...
        'm_tilde', "stay", ...
        'p_tilde', p);
end