%% Define delta_szwalk

%% Function
function delta = delta_szwalk(l, d_tilde, m_tilde)
    % delta_szwalk: Returns 1 if walking in same zone, else 0
    if m_tilde == "walk" && l == d_tilde
        delta = 1;
    else
        delta = 0;
    end
end

% delta = delta_szwalk(2, 2, 'walk')